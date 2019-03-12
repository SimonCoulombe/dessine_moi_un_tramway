# load libraries ----
library(sf)
library(leaflet)
library(tidyverse)
library(furrr)
library(tmap)
library(gepaf)
# create shapefile  quartiers ville de Québec, Lévis, Ancienne Lorette, Wendake, Rive-Nord et Rive-Sud ----

# Quartiers de la vilel de Québec (exclut ancienne-lorette et wendake)
download.file(
  "https://www.donneesquebec.ca/recherche/dataset/5b1ae6f2-6719-46df-bd2f-e57a7034c917/resource/508594dc-b090-407c-9489-73a1b46a8477/download/vdq-quartier.zip",
  destfile = "vdq-quartier.zip")
utils::unzip("vdq-quartier.zip")

quartiers <- sf::read_sf("vdq-quartier.shp") %>% 
  st_zm() %>% # remove third dimension, which leads to errors in leaflet
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]), # find a centroid on the polygon  to add name as label
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))
names(st_geometry(quartiers)) = NULL # prevents polygons not getting drawn : https://stackoverflow.com/questions/53227205/polygons-not-getting-plotted-in-leaflet-r-map-since-update

tm_shape(quartiers) +
  tm_fill() +
  tm_borders() + 
  tm_text("NOM", size = 0.5)

#download shapefile recensement 2001 pour vieilles villes fusionnées  de lévis
# 2001 mapinfo format
download.file("http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gcsd000b01m_e.zip",
              destfile = "gcsd000b01m_e.zip")
utils::unzip("gcsd000b01m_e.zip")
csd2001 <- sf::read_sf("gcsd000b02m_e/gcsd000b02m_e.MID" ) %>%
  mutate(CSDNAME = iconv(CSDNAME, from="CP1252"))  %>%
  st_set_crs(4326)
names(st_geometry(csd2001)) = NULL

levis <- csd2001 %>%  
  filter( ERUID == 2425 & CMAUID == 421 )  %>%  # chaudiere appalache, cma quebec
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]), # find a centroid on the polygon  to add name as label
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))

tm_shape(levis) +
  tm_fill() +
  tm_borders() + 
  tm_text("CSDNAME", size = 0.5)

# Méthodologie Ancienne Lorette et Wendake
# On ne peut pas juste utiliser shapefile du recensement de 2001 car les frontières ne sont pas exactes avec les quartiers de la ville de Québec
# Solution: prendre un "gros" bounding box autour de la ville du recensement de 2001, et en retirer les intersections avec les secteurs de la ville de Québec


st_padded_envelope = function(x, padding = 0.01){  # polygon of bounding box plus 0.01 padding in degrees
  x <- x
  box <- st_bbox(x)
  bigbox <- st_bbox(c(xmin = as.numeric(box$xmin - padding), 
                    ymin = as.numeric(box$ymin - padding),
                    xmax = as.numeric(box$xmax + padding),
                    ymax = as.numeric(box$ymax + padding)), 
                    crs = st_crs(4326))
  st_as_sfc(bigbox)
}

anciennelorette <- csd2001 %>% filter(CSDNAME == "L'Ancienne-Lorette") %>%
  st_padded_envelope(.) %>% 
  st_difference(., st_union(quartiers)) %>%  # start from padded rectangle around old city of ancienne lorette, keep only what isnt already allocated to a sector of the city of quebec
  st_sf %>% # convert sfc_polygon to sf https://github.com/r-spatial/sf/issues/243
  mutate(
    NOM = "L'Ancienne-Lorette",
    lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]), # find a centroid on the polygon  to add name as label
    lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))

wendake <- csd2001 %>% 
  filter(str_detect(CSDNAME, "Wendake")) %>%
  st_union(.) %>% # il y a 2 lignes pour wendake.. alors st_union
  st_padded_envelope(.) %>% 
  st_difference(., st_union(quartiers)) %>%  # start from padded rectangle around old city of ancienne lorette, keep only what isnt already allocated to a sector of the city of quebec
  st_sf %>% # convert sfc_polygon to sf https://github.com/r-spatial/sf/issues/243
  mutate(
    NOM = "Wendake",
    lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]), # find a centroid on the polygon  to add name as label
    lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))


# Merge Quebec, Levis, Ancienen Lorette et Wendake en un seul fichier:
quebec_levis <- quartiers %>% select(NOM, lon, lat) %>%
  rbind(levis %>% select(NOM = CSDNAME, lon, lat)) %>%
  rbind(anciennelorette  %>% select(NOM, lon, lat)) %>%
  rbind(wendake  %>% select(NOM, lon, lat)) %>%
  mutate(ID = row_number())
write_rds(quebec_levis, "quebec_levis.rds")


# Ce serait bien d'avoir un polygon "reste de la rive nord" et "reste de la rive sud".
# On va être très généreux, on va grouper les régions économiques du québec en rive-sud et rive-nord et on va 
# ensuite retirer quebec/lévis de ces deux polygones

zzz <- csd2001 %>% filter(PRUID== 24 ) %>%
  group_by(ERUID) %>%
  summarize(., do_union = TRUE) %>% # convert to a single line
  st_cast()
# faire une carte pour savoir quelles régions économiques sont sur la rive nord et sud
#zzz %>% leaflet %>% addPolygons(label = ~ERUID)
ERUID_rive_nord = c(2490,2465,2460,2455,2450,2470,2420,2475,2480)
rives <- zzz %>% mutate(NOM = ifelse(ERUID %in% ERUID_rive_nord, "rive nord", "rive sud"))%>%
  group_by(NOM) %>%
  summarize(., do_union = TRUE) %>% # convert to a single line
  st_cast()
rives_sans_quebec_levis <- rives %>% st_difference(.,st_union(quebec_levis)) %>%
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]), # find a centroid on the polygon  to add name as label
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))

# fusionner mes rives nord et sud à québec/lévis pour couvrir toute la province.
prov_quebec <- quebec_levis    %>% select(-ID) %>%
  rbind(rives_sans_quebec_levis) %>%
  mutate(ID = row_number())
write_rds(prov_quebec,"prov_quebec.rds")



# sanity check looking good! ----
tm_shape(quebec_levis) + tm_borders() + tm_text("NOM", size = 0.5)
# prov_quebec %>% leaflet() %>% addTiles() %>% addPolygons()
# quebec_levis %>% leaflet() %>% addTiles() %>%  addPolygons(label = ~NOM)
# quartiers %>% leaflet %>% addTiles() %>% addPolygons()
# plot(quebec_levis)
# noms de quartier en facteur pour (j'espère) pouvoir faire des colorFactor

nom_quartiers = prov_quebec %>% 
  select(NOM)  %>%
  mutate(NOM = as.factor(NOM),
         ID = row_number()) %>%
  st_set_geometry(NULL) 



# définir le trajet du tramway (ligne créée manuellement sur cet outil) ----
# https://developers.google.com/maps/documentation/utilities/polylineutility
# à partir de cette carte https://images.radio-canada.ca/v1/ici-info/perso/trajet-tramway.png
# cgn|GpporLsWgYvUgg@~Lqz@|~@g{AovAi`Cra@gr@|MnXtMyJ}_@gaAo|@ooBwUyCwr@m{Ael@ylAqd@y_AaQa`@cSyl@_OdQjAbDOpRxBhN}ZnSf@vMwN|LePkOiK|So^rZwH~IqQ`S}|@bv@oi@pe@qj@`_@ub@bh@

coords  <- gepaf::decodePolyline(enc_polyline = "cgn|GpporLsWgYvUgg@~Lqz@|~@g{AovAi`Cra@gr@|MnXtMyJ}_@gaAo|@ooBwUyCwr@m{Ael@ylAqd@y_AaQa`@cSyl@_OdQjAbDOpRxBhN}ZnSf@vMwN|LePkOiK|So^rZwH~IqQ`S}|@bv@oi@pe@qj@`_@ub@bh@" )

tram <- st_as_sf(coords, coords = c("lon", "lat"))  %>%# get points
  summarize(., do_union = FALSE) %>% # convert to a single line
  st_cast("LINESTRING") %>%
  st_set_crs(4326)


# get trip data ----

# downlad manuel des trajets application "mon trajet" ville de québec (requires username)
#https://data.world/openalytics/mon-trajet-ville-de-quebec-2014

points_5_au_11 <- sf::read_sf("data/montrajet_5au11.geojson")
points_12_au_18 <- sf::read_sf("data/montrajet_12au18.geojson")
points_24_au_8 <- sf::read_sf("data/montrajet_24au8.geojson")

points <- rbind(points_5_au_11,points_12_au_18 , points_24_au_8 )

lignes <- points %>%
  group_by(TRIP_ID) %>% # créer les lignes trajets à partir des points
  summarize(., do_union = FALSE) %>%
  st_cast("LINESTRING")

trajets <- lignes %>%
  mutate( # ajout des points origine et destination 
    origine = pmap(
      list(geometry), 
      function(.geometry){st_line_sample(.geometry,sample = 0 ) %>% st_set_crs(4326)}) ,
    destination = pmap(
      list(geometry), 
      function(.geometry){st_line_sample(.geometry,sample = 1 ) %>% st_set_crs(4326)}), # st_coordinates( 
    origine_x =  
      pmap(
        list(origine), 
        function(.origine){st_coordinates(.origine) %>% .[1]}) %>% unlist(),
    origine_y =  
      pmap(
        list(origine), 
        function(.origine){st_coordinates(.origine) %>% .[2]}) %>% unlist(),
    destination_x =  
      pmap(
        list(destination), 
        function(.destination){st_coordinates(.destination) %>% .[1]}) %>% unlist(),
    destination_y =  
      pmap(
        list(destination), 
        function(.destination){st_coordinates(.destination) %>% .[2]}) %>% unlist()
    ) 
trajets2 <- trajets %>%  

  mutate( # ajout quartier de origine et destination.  ca mériterait d'être parallélisé avec furrr::future_map, mais j'ai juste 2 coeurs sur ma vieille poubelle anyway..
    ID_quartier_origine = pmap(
      list(origine),
      function(.origine){
        as.numeric(st_within(.origine, prov_quebec))})  %>% unlist(),
    ID_quartier_destination = pmap(
      list(destination),
      function(.destination){
        as.numeric(st_within(.destination, prov_quebec))})  %>% unlist()
  ) %>%
  left_join(nom_quartiers %>% select(ID_quartier_origine = ID, NOM_quartier_origine = NOM) ) %>%
  left_join(nom_quartiers %>% select(ID_quartier_destination = ID, NOM_quartier_destination = NOM) )
  
write_rds(trajets2,"trajets2.rds")


mypalette_origin <- leaflet::colorNumeric(palette = "plasma", domain = c(quebec_levis$ID))
#mypalette_origin <- leaflet::colorFactor(palette = "plasma", domain = c(quebec_levis$NOM))

trajets2 %>% leaflet() %>%
   addTiles %>% 
  addPolygons(data = quebec_levis,
              color = ~mypalette_origin(ID),
              label =~paste0("Quartier: ", NOM, " ID: ", ID)) %>%
  addPolylines(data=tram, color = "red", label = "TCHOU TCHOU!!", opacity = 1, weight = 5)
  # addLabelOnlyMarkers( data = quartiers,
  #   lng = ~lon, 
  #   lat = ~lat,
  #   label =  ~ as.character(NOM)) %>%
  # addCircleMarkers(
  #   lng =~ destination_x, 
  #   lat =~ destination_y, 
  #   color = ~mypalette_origin(ID_quartier_origine),
  #   label =~ paste0("Origine:" , NOM_quartier_origine, " Destination: ", NOM_quartier_destination))
  

dest.xy <- trajets2 %>% 
  st_set_geometry(NULL) %>%
  rename( oX= origine_x,
          oY= origine_y,
          dX = destination_x,
         dY = destination_y) %>%
  mutate(trips = 1)
#http://spatial.ly/2015/03/mapping-flows/
xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)

# carte de tous les trajets
ggplot(dest.xy %>% filter(ID_quartier_origine<=49, ID_quartier_destination <= 49) , aes(oX, oY))+
  #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below 
  geom_segment(aes(x=oX, y=oY,xend=dX, yend=dY), col="white", alpha=0.01)+
  #Here is the magic bit that sets line transparency - essential to make the plot readable
  scale_alpha_continuous(range = c(0.03, 0.3))+
  #Set black background, ditch axes and fix aspect ratio
  theme(panel.background = element_rect(fill='black',colour='black'))+quiet+coord_equal()


p1 <- st_sfc(st_point(c(-71.4, 46.8)), crs = 4326) # 
st_distance(p1, tram)
st_crs(tram)

trajets3 <- trajets2 %>% 
  mutate(
    point_origine = pmap(
      list(origine_x, origine_y),
      function(.x, .y){st_sfc(st_point(c(.x, .y)), crs = 4326)
        }),
    point_destination = pmap(
      list(destination_x, destination_y),
      function(.x, .y){st_sfc(st_point(c(.x, .y)), crs = 4326)
      })
    )
trajets4 <- trajets3 %>% filter(ID_quartier_origine<=49, ID_quartier_destination <= 49) %>%
  mutate(
    distance_origine_tram = map(point_origine, function(.point){st_distance(.point, tram)}) %>% as.numeric(),
    distance_destination_tram = map(point_destination, function(.point){st_distance(.point, tram)}) %>% as.numeric()
    ) 

trajets4 <- trajets4 %>% 
  mutate(
      tramway_accessible = ifelse(distance_origine_tram + distance_destination_tram < 1000, TRUE, FALSE)
  )



dest.xy <- trajets4 %>% 
  mutate(tramway = as.logical(tramway_accessible)) %>%
  st_set_geometry(NULL) %>%
  rename( oX= origine_x,
          oY= origine_y,
          dX = destination_x,
          dY = destination_y) %>%
  mutate(trips = 1)
#http://spatial.ly/2015/03/mapping-flows/
xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)

mean(dest.xy$tramway)
# carte de tous les trajets
ggplot(dest.xy  , aes(oX, oY))+
  #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below 
  geom_segment(aes(x=oX, y=oY,xend=dX, yend=dY, col = tramway),  alpha=0.03)+
  #Here is the magic bit that sets line transparency - essential to make the plot readable
  scale_alpha_continuous(range = c(0.03, 0.3))+
  #Set black background, ditch axes and fix aspect ratio
  theme(panel.background = element_rect(fill='black',colour='black'))+quiet+coord_equal()+
  scale_color_manual(values = c("white", "red"))
