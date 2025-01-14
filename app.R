library(shiny)
library(rsconnect)


miami <- read.csv("miami.csv", stringsAsFactors = FALSE)


addResourcePath("static", "www")


#### Normaliser les prix pour créer une échelle de couleur continue
miami <- miami %>%
  mutate(
    normalized_price = (prix - min(prix, na.rm = TRUE)) / 
      (max(prix, na.rm = TRUE) - min(prix, na.rm = TRUE))
  )



#### Ajouter du CSS personnalisé pour améliorer le style de l'application
custom_css <- "
body {
  background-color: #f7f7f7; /* Couleur d'arrière-plan claire */
  font-family: 'Arial', sans-serif;
}

.navbar {
  background-color: #0073e6; /* Bleu vibrant à thème Miami */
  color: white;
}

.navbar-brand {
  color: white !important;
}

.navbar-nav > li > a {
  color: black !important; /* Couleur du texte des liens de navigation en noir */
}

h1, h2, h3, h4 {
  text-align: center;
}

.card {
  background: white;
  border-radius: 10px;
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
  padding: 20px;
  margin: 20px;
}

.footer {
  background-color: #0073e6;
  color: white;
  text-align: center;
  padding: 10px 0;
  position: fixed;
  bottom: 0;
  width: 100%;
  font-size: 14px;
}
"


### Interface
ui <- navbarPage(
  "Ventes immobilières à Miami",
  
  # Inclure le CSS personnalisé
  header = tags$head(tags$style(HTML(custom_css))),
  
  # Page d'introduction
  tabPanel(
    "Introduction",
    fluidPage(
      titlePanel("Projet pour le cours de Data-visualisation"),
      
      # Contenu centré dans une carte stylée
      div(
        class = "card",
        h3("Environnement actuariel - S1-24"),
        h4("À l’intention de : Monsieur Kezhan Shi"),
        
        h4("Créateurs de l'application :", align = "left"),
        tags$ul(
          tags$li("Luc Allart"),
          tags$li("Charles Blondeau-Andrews"),
          tags$li("Erwan-Henri Burlisson"),
          tags$li("Thomas Farah")
        ),
        
        br(),
        p("Utilisez le menu ci-dessus pour naviguer entre les pages et explorer les données.")
      ),
      
      # Logos centrés horizontalement
      fluidRow(
        column(12, align = "center",
               tags$img(src = "static/Logo-ISUP.jpg", height = "100px", style = "margin-right: 20px;"),
               tags$img(src = "static/Sciences_SU.png", height = "80px")
        )
      )
    )
  ),
  
  # Page de la carte
  tabPanel(
    "Carte",
    fluidPage(
      titlePanel("Carte des ventes immobilières à Miami en fonction du prix de vente"),
      
      # Ajouter des contrôles de filtrage (exemple : filtre par fourchette de prix)
      sidebarLayout(
        sidebarPanel(
          sliderInput("priceRange", "Filtrer par prix :", 
                      min = 0, max = max(miami$prix), 
                      value = c(0, max(miami$prix)), step = 50000, pre = "$"),
          width = 3
        ),
        mainPanel(
          leafletOutput("map", width = "100%", height = "500px")
        )
      )
    )
  )
)
  



### Serveur
server <- function(input, output) {
  
  # Charger la carte avec les points
  output$map <- renderLeaflet({
    # Filtrer les données en fonction de la fourchette de prix sélectionnée
    filtered_data <- miami %>%
      filter(prix >= input$priceRange[1], prix <= input$priceRange[2])
    
    leaflet(data = filtered_data) %>%
      addTiles() %>%  # Ajouter les tuiles de fond (OpenStreetMap)
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        color = ~colorNumeric("viridis", domain = miami$prix, reverse = TRUE)(prix),  # Couleurs selon le prix
        radius = 3,  # Taille des points
        stroke = FALSE,
        fillOpacity = 0.8,  # Opacité des points
        popup = ~paste0("<b>Prix :</b> $", format(prix, big.mark = ","))  # Infos dans les popups
      ) %>%
      addLegend(
        "bottomright",
        pal = colorNumeric("viridis", domain = miami$prix, reverse = TRUE),  # Palette de couleurs
        values = miami$prix,
        title = "Prix des maisons",
        opacity = 0.8
      )
  })
}


## Application
shinyApp(ui = ui, server = server)




