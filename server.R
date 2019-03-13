library(shiny)
prov_quebec <- read_rds("prov_quebec.rds")
trajets2 <- read_rds("trajets2_small.rds")  %>% 
  filter(!is.na(ID_quartier_destination)  & !is.na(ID_quartier_origine) )

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  # reactive expression
  mytest3 <- eventReactive( input$recalc, {
    coords  <- gepaf::decodePolyline(enc_polyline = input$polygone )
    
    tram <- st_as_sf(coords, coords = c("lon", "lat"))  %>%# get points
      summarize(., do_union = FALSE) %>% # convert to a single line
      st_cast("LINESTRING") %>%
      st_set_crs(4326)
    
    # TROUVER RÉGIONS TRAVERSÉES PAR LE TRAM pour calculer les distances seulement pour ces trajets là.
    # pour un trajet donné, trouver quels secteurs incluent le trajet ou sont a moins de 1000 metres 
    temp_regions <- prov_quebec
    start <- Sys.time()
    temp_regions$region_distance <- as.numeric(st_distance(temp_regions, tram))
    stop <- Sys.time()
    print(stop-start) # 12 secondes
    temp_regions$region_accessible <- temp_regions$region_distance == 0
    
    # Pour les trajets dans les régions concernées, vérifier si ils peuvent utiliser le tram
    
    start <- Sys.time()
    temp <- trajets2 %>%
      left_join(temp_regions %>% 
                  select(ID_quartier_origine= ID, region_accessible_origine = region_accessible) %>%  
                  st_set_geometry(NULL)) %>%
      left_join(temp_regions %>% 
                  select(ID_quartier_destination= ID, region_accessible_destination = region_accessible) %>%  
                  st_set_geometry(NULL)) %>%
      filter(region_accessible_origine == TRUE, region_accessible_destination == TRUE) %>%
      mutate(
        distance_origine_tram = map(point_origine, function(.point){st_distance(.point, tram)}) %>% as.numeric(),
        distance_destination_tram = map(point_destination, function(.point){st_distance(.point, tram)}) %>% as.numeric(),
        distance_totale = distance_origine_tram + distance_destination_tram,
        tramway_accessible = ifelse(distance_totale < 1000, TRUE, FALSE)
      )
    stop <- Sys.time() # 23 secondes
    print(stop-start) 
    
    # définir pour tous les trajets si ils sont acceessibles.  Si ne commence ou fini
    # pas dans un secteur couvert alors NON
    trajets3 <- trajets2 %>%
      left_join(temp %>% 
                  select(TRIP_ID, 
                         tramway_accessible, 
                         distance_totale, 
                         distance_origine_tram, 
                         distance_destination_tram )) %>%
      mutate(tramway_accessible = ifelse(is.na(tramway_accessible), FALSE, tramway_accessible))
    
    # Mesures de performance
    compte_trajet_region_quebec_levis <- trajets2 %>%
      filter(ID_quartier_origine <= 49 | ID_quartier_destination <= 49 ) %>%
      tally() # 13 925
    
    compte_trajet_region_tram <- trajets2 %>%
      left_join(temp_regions %>% 
                  select(ID_quartier_origine= ID, region_accessible_origine = region_accessible) %>%  
                  st_set_geometry(NULL)) %>%
      left_join(temp_regions %>% 
                  select(ID_quartier_destination= ID, region_accessible_destination = region_accessible) %>%  
                  st_set_geometry(NULL)) %>%
      filter(region_accessible_origine == TRUE | region_accessible_destination == TRUE) %>%
      tally() #  6700
    
    compte_trajets_remplaces <-
      temp %>% filter(tramway_accessible == TRUE) %>% tally() # 618
    
    
    pct_trajet_remplaces_region_couverte <- compte_trajets_remplaces / compte_trajet_region_tram
    pct_trajet_remplaces_region_quebec_levis <- compte_trajets_remplaces / compte_trajet_region_quebec_levis
    pct_quebec_levis_par_km <- pct_trajet_remplaces_region_quebec_levis /  as.numeric(st_length(tram))  * 1000
    
    # Carte: pourcentages remplacables par région, 
    test1 <- trajets3 %>% select(TRIP_ID, NOM = NOM_quartier_origine, tramway_accessible)
    test2 <- trajets3 %>% select(TRIP_ID, NOM = NOM_quartier_destination, tramway_accessible)
    test <- bind_rows(test1, test2)
    test3 <- test %>% distinct() %>% 
      group_by(NOM) %>% 
      summarise(count_trip = n(),
                pct_tramway = 100*mean(tramway_accessible))
  })
  

  output$mymap <- renderLeaflet({
    mypalette_pct <- leaflet::colorNumeric(palette = "plasma", domain = c(mytest3() %>% pull(pct_tramway)))
    
    prov_quebec %>% filter (ID <= 49) %>% 
      left_join( mytest3()) %>% 
      leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addPolygons( color= ~ mypalette_pct(pct_tramway),
                   label = ~ paste0(
                     NOM, 
                     " : ", 
                     signif(pct_tramway,3),
                     " %")) %>%
      addPolylines(data=tram, color = "red", label = "TCHOU TCHOU!!", opacity = 1, weight = 5) %>%
      addLegend("bottomleft",
                pal = mypalette_pct,
                values =  ~ c(pct_tramway),
                title = "Pourcentage des voyages  <br>débutant ou se terminant dans ce secteur <br> remplaçables par le tramway <br> (moins de 1000 mètres de marche)")
  })
  
  output$mytable <- renderTable({ mytest3() %>% arrange(-pct_tramway)})
})