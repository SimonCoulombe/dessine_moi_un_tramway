library(shiny)
prov_quebec <- read_rds("prov_quebec.rds")
trajets2 <- read_rds("trajets2_small.rds")  %>% 
  filter(!is.na(ID_quartier_destination)  & !is.na(ID_quartier_origine) )

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  # reactive expression
  my_results <- eventReactive( input$recalc, {
    start <- Sys.time()
    coords  <- gepaf::decodePolyline(enc_polyline = input$polygone )
    
    tram <- st_as_sf(coords, coords = c("lon", "lat"))  %>%# get points
      summarize(., do_union = FALSE) %>% # convert to a single line
      st_cast("LINESTRING") %>%
      st_set_crs(4326)
    
    # TROUVER RÉGIONS TRAVERSÉES PAR LE TRAM pour calculer les distances seulement pour ces trajets là.
    # pour un trajet donné, trouver quels secteurs incluent le trajet ou sont a moins de 1000 metres 
    temp_regions <- prov_quebec
    
    temp_regions$region_distance <- as.numeric(st_distance(temp_regions, tram))
    
    
    temp_regions$region_accessible <- temp_regions$region_distance < 500
    
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
    
    stop <- Sys.time() # 23 secondes
    print(stop-start) 
    list("trajets3"= trajets3,
         "test3" = test3,
         "pct_trajet_remplaces_region_couverte" = pct_trajet_remplaces_region_couverte,
         "pct_trajet_remplaces_region_quebec_levis" = pct_trajet_remplaces_region_quebec_levis, 
         "pct_quebec_levis_par_km" = pct_quebec_levis_par_km,
         "tram_length" = signif(as.numeric(st_length(tram)),3)/1000,
         "tram" = tram)
    
  })
  

  output$mymap <- renderLeaflet({
    mypalette_pct <- leaflet::colorNumeric(palette = "plasma", domain = c(my_results()$"test3" %>% pull(pct_tramway)))
    
    prov_quebec %>% filter (ID <= 49) %>% 
      left_join( my_results()$"test3") %>% 
      leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addPolygons( color= ~ mypalette_pct(pct_tramway),
                   label = ~ paste0(
                     NOM, 
                     " : ", 
                     signif(pct_tramway,3),
                     " %")) %>%
      addPolylines(data=my_results()$"tram", color = "red", label = "TCHOU TCHOU!!", opacity = 1, weight = 5) %>%
      addLegend("bottomleft",
                pal = mypalette_pct,
                values =  ~ c(pct_tramway),
                title = "Pourcentage des voyages  <br>débutant ou se terminant dans ce secteur <br> remplaçables par le tramway <br> (moins de 1000 mètres de marche)")
  })
  
  output$mytable <- renderTable({ my_results()$"test3" %>% arrange(-pct_tramway)})
  
  output$myplot <- renderPlot({
    dest.xy <- my_results()$"trajets3" %>% 
      filter(ID_quartier_origine<=49, ID_quartier_destination <= 49) %>%
      mutate(tramway = as.logical(tramway_accessible)) %>%
      rename( oX= origine_x,
              oY= origine_y,
              dX = destination_x,
              dY = destination_y)
    
    xquiet<- scale_x_continuous("", breaks=NULL)
    yquiet<-scale_y_continuous("", breaks=NULL)
    quiet<-list(xquiet, yquiet)
    
    ggplot(dest.xy  , aes(oX, oY))+
      #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below 
      geom_segment(aes(x=oX, y=oY,xend=dX, yend=dY, col = tramway),  alpha=0.03)+
      #Here is the magic bit that sets line transparency - essential to make the plot readable
      scale_alpha_continuous(range = c(0.03, 0.3))+
      #Set black background, ditch axes and fix aspect ratio
      theme(panel.background = element_rect(fill='black',colour='black'))+quiet+coord_equal()+
      scale_color_manual(values = c("white", "red")) + 
      ggtitle(paste0(signif(100*my_results()$"pct_trajet_remplaces_region_quebec_levis",3), 
                     " % de l'ensemble des trajets à origine ou destination de Québec et Lévis peuvent utiliser le tramway.")) +
      labs(subtitle = paste0(
        "Performance: ", signif(my_results()$"pct_quebec_levis_par_km"*100,3),
        " % par kilomètre de tramway"),
        caption = paste0("Longueur: ",my_results()$"tram_length", " km."))
    
    
  })
})