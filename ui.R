library(shiny)
library(sf)
library(leaflet)
library(tidyverse)
library(gepaf)

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
  
  "Visiter ce site pour dessiner votre ligne de tramway: https://developers.google.com/maps/documentation/utilities/polylineutility",
  p(),
  "Définir la ligne en cliquant sur la carte pour ajouter un marker, suivi d'un clic sur le bouton 'add location' pour ajouter d'autres points.",
  p(),
  "Lorsque vous avez terminé copiez le contenu de la boîte 'Encoded polyline' dans la boite ci-bas:",
  p(),
  "L'exemple ci-bas représente la ligne proposée actuellement par l'administration Labeaume.",
  p(),
  textInput("polygone", 
                "Encoded Polyline", 
                "cgn|GpporLsWgYvUgg@~Lqz@|~@g{AovAi`Cra@gr@|MnXtMyJ}_@gaAo|@ooBwUyCwr@m{Ael@ylAqd@y_AaQa`@cSyl@_OdQjAbDOpRxBhN}ZnSf@vMwN|LePkOiK|So^rZwH~IqQ`S}|@bv@oi@pe@qj@`_@ub@bh@"),  
  actionButton("recalc", "Mettre à jour (environ 60 secondes)"),
  p(),
  leafletOutput("mymap"),
  p()
  
  )
  )