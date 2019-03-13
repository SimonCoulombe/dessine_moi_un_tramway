library(shiny)
library(sf)
library(leaflet)
library(tidyverse)
library(gepaf)

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
  "Cette application Shiny utilise les données ouvertes de l'application ", 
  tags$a("Mon trajet", href="https://ici.radio-canada.ca/nouvelle/665247/mon-trajet-telechargement-application"),  
  " afin de tenter de déterminer le pourcentage des parcours qui utiliseront le tramway lorsqu'il sera construit.",
  p(),
  "L'exemple ci-bas représente ",
  tags$a("la ligne de 23 km proposée actuellement par l'administration Labeaume", href="https://ici.radio-canada.ca/nouvelle/1089685/tramway-quebec-transport-en-commun-projet-transport-structurant"),  
  p(),
  "Vous pouvez aussi jouer à l'urbaniste en herbe et voir si vous pouvez mieux faire en définissant votre propre ligne de tramway!   Pour ce faire, visitez ",
  tags$a("la polyline utility de google", href="https://developers.google.com/maps/documentation/utilities/polylineutility"),
  ". Définissez la ligne en cliquant sur la carte pour ajouter un marqueur puis en cliquant sur le bouton 'add location' pour ajouter d'autres points. Lorsque vous avez terminé copiez le contenu de la boîte 'Encoded polyline' dans la boite ci-bas, et appuyez sur le bouton 'mise à jour'.",
  p(),
  " Cette application naïve a évidemment plusieurs limitations.", 
  "La plus importante est qu'il ne tient pas compte de l'ensemble du réseau de transport en commun et ne permet pas de correspondances avec les autobus, ce qui sous-estime la part modale du tramway.",
  " Une autre est l'aspect structurant du tramway.  Une fois le tramway établit, les gens qui souhaitent l'utiliser pourront déménager à proximité. ",
  "Finalement, les gens qui ont installé l'application 'Mon trajet' sont auto-sélectionnés, ce qui implique des biais d'auto-sélections."  ,
  textInput("polygone", 
                "Encoded Polyline", 
                "cgn|GpporLsWgYvUgg@~Lqz@|~@g{AovAi`Cra@gr@|MnXtMyJ}_@gaAo|@ooBwUyCwr@m{Ael@ylAqd@y_AaQa`@cSyl@_OdQjAbDOpRxBhN}ZnSf@vMwN|LePkOiK|So^rZwH~IqQ`S}|@bv@oi@pe@qj@`_@ub@bh@"),  
  actionButton("recalc", "Mettre à jour (environ 60 secondes)"),
  p(),
  leafletOutput("mymap"),
  p(),
  tableOutput("mytable")
  
  )
  )