

library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(shinyWidgets)
library(base64enc)


#### Importation et prétraitement des données
miami <- read.csv("miami.csv")

miami <- miami %>%
  mutate(
    normalized_price = (prix - min(prix, na.rm = TRUE)) / 
      (max(prix, na.rm = TRUE) - min(prix, na.rm = TRUE))
  )

quartiers <- st_read("Miami_Neighborhoods_Shapefile-shp")
zip <- st_read("kx-miami-dade-county-florida-zip-code-SHP")
miami_sf <- st_as_sf(miami, coords = c("longitude", "latitude"), crs = 4326)
quartiers <- st_transform(quartiers, crs = st_crs(miami_sf))
zip <- st_transform(zip, crs = st_crs(miami_sf))

# Agrégation des données
miami_quartiers <- quartiers %>%
  st_join(miami_sf) %>% # Jointure spatiale
  group_by(LABEL) %>% # Groupement par le champ LABEL (Quartier)
  summarise(
    moy_prix = mean(prix, na.rm = TRUE),
    moy_surface_terrain = mean(surface_terrain, na.rm = TRUE),
    moy_surface_plancher = mean(surface_plancher, na.rm = TRUE),
    moy_dist_transport = mean(dist_transport, na.rm = TRUE),
    moy_dist_ocean = mean(dist_ocean, na.rm = TRUE),
    moy_dist_business_district = mean(dist_business_district, na.rm = TRUE),
    moy_age_maison = mean(age_maison, na.rm = TRUE),
    moy_normalized_price = mean(normalized_price, na.rm = TRUE),
    nb_ventes = length(prix)
  )

miami_zip <- zip %>%
  st_join(miami_sf) %>% # Jointure spatiale
  group_by(ZIPCODE) %>% # Groupement par le champ ZIPCODE
  summarise(
    moy_prix = mean(prix, na.rm = TRUE),
    moy_surface_terrain = mean(surface_terrain, na.rm = TRUE),
    moy_surface_plancher = mean(surface_plancher, na.rm = TRUE),
    moy_dist_transport = mean(dist_transport, na.rm = TRUE),
    moy_dist_ocean = mean(dist_ocean, na.rm = TRUE),
    moy_dist_business_district = mean(dist_business_district, na.rm = TRUE),
    moy_age_maison = mean(age_maison, na.rm = TRUE),
    moy_normalized_price = mean(normalized_price, na.rm = TRUE),
    nb_ventes = length(prix)
  )


# Images
ISUP <- base64encode("www/Logo-ISUP.jpeg")
SU <- base64encode("www/Sciences_SU.jpeg")


# Style personnalisé
custom_css <- "
body {
  background-color: #f7f7f7;
  font-family: 'Arial', sans-serif;
}

.navbar {
  background-color: #0073e6;
  color: white;
}

.navbar-brand {
  color: white !important;
}

.navbar-nav > li > a {
  color: black !important;
}

h1, h2, h3, h4 {
  text-align: center;
  color: #333;
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

.leaflet {
  border: 2px solid #0073e6;
  border-radius: 10px;
}

.sidebar {
  background: #fefefe;
  padding: 15px;
  border-radius: 10px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

.btn-primary {
  background-color: #0073e6;
  border-color: #0073e6;
}

.btn-primary:hover {
  background-color: #005bb5;
  border-color: #005bb5;
}

.table-container {
  margin-top: 20px;
  background: #fff;
  padding: 15px;
  border-radius: 10px;
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
}

.table-container table {
  width: 100%;
  border-collapse: collapse;
  margin: 0;
}

.table-container th, .table-container td {
  border: 1px solid #ddd;
  padding: 8px;
  text-align: center; /* Center-align all content */
}

.table-container th {
  background-color: #0073e6;
  color: white;
  text-align: center;
  font-weight: bold;
}

.table-container td {
  font-weight: bold; /* Make values bold */
}

.table-container tr:nth-child(even) {
  background-color: #f2f2f2;
}

.table-container tr:hover {
  background-color: #ddd;
}

a {
  color: #007bff;
  text-decoration: none;
  cursor: pointer;
}

a:hover {
  text-decoration: underline;
  color: #0056b3;
}

a:link, a:visited {
  pointer-events: auto;
  color: inherit;
}

a[disabled] {
  pointer-events: none;
  color: #6c757d;
  text-decoration: none;
}

"




### Interface
ui <- navbarPage(
  "Ventes immobilières à Miami",
  
  header = tags$head(tags$style(HTML(custom_css))),
  
  tabPanel(
    "Introduction",
    fluidPage(
      titlePanel("Data-visualisation : Marché immobilier Miami-Dade"),
      div(
        class = "card",
        h3("Environnement actuariel - S1-24"),
        h4("Créateurs de l'application :"),
        tags$ul(
          tags$li("Luc Allart"),
          tags$li("Charles Blondeau-Andrews"),
          tags$li("Erwan-Henri Burlisson"),
          tags$li("Thomas Farah")
        )),
        div(
          class = "card",
          h3("Mise en contexte"),
          tags$p(
            "Bienvenue dans notre application interactive pour l'analyse du marché immobilier de la région de Miami-Dade. 
        Cette application vise à fournir des informations claires et intuitives pour explorer les données des ventes immobilières, 
        identifier les tendances et aider à la prise de décision."
            ),
          tags$p(
            "Nos données proviennent de Kaggle :",
            tags$a(href = "https://www.kaggle.com/datasets/deepcontractor/miami-housing-dataset", target = "_blank", "Données Kaggle"),
            ". Elles constituent un fichier de 13 900 observations de biens immobiliers vendus en 2016 pour 17 variables. Afin de générer les cartes
            géographiques intéractives aux pages suivantes, nous avons aussi utilisé des données publiques de la ville de Miami
            et du compté de Miami-Dade(",
            tags$a(href = "https://datahub-miamigis.opendata.arcgis.com/datasets/miami-neighborhoods-shapefile/about", target = "_blank", "Coordonnées de quartier"),
            "et",
            tags$a(href = "https://koordinates.com/layer/96304-miami-dade-county-florida-zip-code/", target = "_blank", "Coordonnées de code ZIP"),
            "). Le script pour le nettoyage et le prétraitement des données est disponible sur demande en contactant l'addresse
            suivante : caba.charles@gmail.com. Dans le cadre du projet pour le cours de Data-Visualisation, ils se trouvent
            dans le répertoire GitHub demandé. En voici un résumé bref."),
          tags$p(tags$b("Conversions des unités :"),
            "Toutes les distances sont exprimées en pieds ou en pieds carrés, tandis que les prix sont en dollars.
            Nous avons conservé les prix en dollars, mais nous avons converti les distances de pieds en mètres."),
          tags$p(tags$b("Gestion des valeurs manquantes :"),
                 "Aucune valeur manquante n’a été identifiée dans la base de données."),
          tags$p(tags$b("Traitement des doublons :"),
                 "Nous avons constaté la présence de 156 doublons dans la base de données. 
            Une analyse plus approfondie a montré que, dans plusieurs cas, 
            un même identifiant (“ID”) correspondait à deux ventes réalisées au cours de la même année. 
            Pour résoudre ce problème, nous avons créé un nouvel identifiant en concaténant l’identifiant existant avec le mois de vente. 
            Après cette étape, certains doublons subsistaient. Ces doublons correspondaient à des entrées consécutives, 
            et nous avons choisi de conserver la dernière entrée, en supposant que celle-ci représentait une correction des informations initiales.
            Suite à cette opération, un dernier doublon subsistait, mais il ne partageait que l’identifiant principal. 
            Nous avons donc créé un nouvel identifiant unique pour cet individu afin de préserver un maximum d’informations.")
          ),
      fluidRow(
        column(12, align = "center",
               tags$img(src = paste0("data:image/jpeg;base64,", ISUP), height = "100px", style = "margin-right: 20px;"),
               tags$img(src = paste0("data:image/jpeg;base64,", SU), height = "100px")
        )
      )
    )
  ),
  
  
  tabPanel(
    "Analyse bivariée",
    titlePanel("Graphique interactif des variables continues"),
    
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          inputId = "xvar",
          label = "Sélectionnez la variable X :",
          choices = c(
            "Prix" = "prix",
            "Surface de terrain" = "surface_terrain",
            "Surface de plancher" = "surface_plancher",
            "Distance aux transports" = "dist_transport",
            "Distance à l'océan" = "dist_ocean",
            "Distance au centre d'affaires" = "dist_business_district",
            "Âge de la maison" = "age_maison"
          ),
          selected = "prix"
        ),
        
        radioButtons(
          inputId = "yvar",
          label = "Sélectionnez la variable Y :",
          choices = c(
            "Prix" = "prix",
            "Surface de terrain" = "surface_terrain",
            "Surface de plancher" = "surface_plancher",
            "Distance aux transports" = "dist_transport",
            "Distance à l'océan" = "dist_ocean",
            "Distance au centre d'affaires" = "dist_business_district",
            "Âge de la maison" = "age_maison"
          ),
          selected = "surface_terrain"
        )
      ),
      
      mainPanel(
        plotOutput(outputId = "scatterPlot", height = "600px", width = "100%")
      )
    )
    
  ),
  
  tabPanel(
    "Carte",
    fluidPage(
      titlePanel("Carte des ventes immobilières à Miami"),
      sidebarLayout(
        sidebarPanel(
          selectInput("segment", "Segmentation de la carte", 
                      choices = list(
                        "Aucune" = "none",
                        "Code ZIP" = "zip",
                        "Quartier" = "quartiers",
                        "Ventes individuelles" = "ind"
                      )
          ),
          
          conditionalPanel(
            condition = "input.segment === 'quartiers' || input.segment === 'zip'",
            radioButtons("variable", "Filtrer par :",
                         choices = list(
                           "Prix moyen" = "moy_prix",
                           "Surface moyenne de terrain" = "moy_surface_terrain",
                           "Surface moyenne de plancher" = "moy_surface_plancher"
                         )
            )
          ),
          
          conditionalPanel(
            condition = "input.segment === 'quartiers'",
            div(
              style = "margin-top: 20px; color: #555;",
              tags$p("La segmentation par quartier n'est disponible que pour la ville de Miami.")
            )
          ),
          
          conditionalPanel(
            condition = "input.segment === 'ind'",
            numericRangeInput("priceRange", "Filtrer par prix :",
                              value = c(0, 3000000),
                              separator = "à",
                              min = 0,
                              max = 3000000,
                              step = 100000)
          )
        ),
        
        mainPanel(
          leafletOutput("map", width = "100%", height = "550px")
        )
      )
    )
  ),
  
  tabPanel(
    "Analyse par zone",
    fluidPage(
      titlePanel("Analyse du marché immobilier par zone"),
      sidebarLayout(
        sidebarPanel(
          selectInput("segment2", "Segmentation de l'analyse",
                      choices = list(
                        "Aucune" = "none2",
                        "Code ZIP" = "zip2",
                        "Quartier" = "quartiers2"
                      )),
          
          conditionalPanel(
            condition = "input.segment2 === 'zip2'",
            selectizeInput(
              inputId = "zipname",
              label = "Trouvez votre code ZIP",
              choices = paste(miami_zip$ZIPCODE[which(!is.na(miami_zip$moy_prix))]),
              selected = NULL,
              multiple = FALSE,
              options = list(placeholder = "Tapez pour rechercher")
            )
          ),
          
          conditionalPanel(
            condition = "input.segment2 === 'quartiers2'",
            selectizeInput(
              inputId = "quartiername",
              label = "Trouvez votre quartier",
              choices = paste(miami_quartiers$LABEL[which(!is.na(miami_quartiers$moy_prix))]),
              selected = NULL,
              multiple = FALSE,
              options = list(placeholder = "Tapez pour rechercher")
            )
          ),
          
          conditionalPanel(
            condition = "input.segment2 === 'quartiers2'",
            div(
              style = "margin-top: 20px; color: #555;",
              tags$p("La segmentation par quartier n'est disponible que pour la ville de Miami.")
            )
          )
        ),
        
        mainPanel(
          leafletOutput("map2", width = "100%", height = "350px"),
          tableOutput("zoneTable")
        )
      )
    )
  )
)



### Serveur
server <- function(input, output) {
  
  # Nuage de points
  output$scatterPlot <- renderPlot({
    x <- miami[[input$xvar]]
    y <- miami[[input$yvar]]
    
    labels <- c(
      prix = "Prix",
      surface_terrain = "Surface de terrain",
      surface_plancher = "Surface de plancher",
      dist_transport = "Distance aux transports",
      dist_ocean = "Distance à l'océan",
      dist_business_district = "Distance au centre d'affaires",
      age_maison = "Âge de la maison"
    )
    
    plot(x, y, 
         main = paste("Nuage de points de", labels[input$xvar], "en fonction de", labels[input$yvar]),
         xlab = labels[input$xvar], 
         ylab = labels[input$yvar], 
         pch = 19, col = rgb(0, 0, 1, 0.5), cex = 0.7)
  })
  
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(lng = -80.191788, lat = 25.761681, zoom = 10)
  })
  
  observe({
    leafletProxy("map") %>% clearShapes() %>% clearControls()
    
    
    format_variable <- function(var_name, value) {
      ifelse(
        is.na(value), 
        paste0("Aucune vente"),
        if (var_name == "moy_prix") {
          paste0("<b>Prix moyen:</b> $", format(round(value, 0), big.mark = ","))
        } else if (var_name == "moy_surface_terrain") {
          paste0("<b>Surface terrain moyenne:</b> ", format(round(value, 0), big.mark = ","), " m²")
        } else if (var_name == "moy_surface_plancher") {
          paste0("<b>Surface plancher moyenne:</b> ", format(round(value, 0), big.mark = ","), " m²")
        } else {
          paste0("<b>", variable_names[[var_name]], ":</b> ", format(round(value, 2), big.mark = ","))
        }
        )
    }
    
    
    format_segment <- function(segment_name) {
      if (segment_name == "quartiers") {
        "<b>Quartier :</b>"
      } else if (segment_name == "zip") {
        "<b>Code ZIP :</b>"
      } else {
        ""
      }
    }
    
    variable_names <- list(
      "Prix moyen" = "moy_prix",
      "Surface moyenne de terrain" = "moy_surface_terrain",
      "Surface moyenne de plancher" = "moy_surface_plancher"
    )
    
    
    if (input$segment == "ind") {
      filtered_data <- miami %>%
        filter(prix >= input$priceRange[1], prix <= input$priceRange[2])
      
      leafletProxy("map", data = filtered_data) %>%
        clearMarkers() %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          color = ~colorNumeric("magma", miami$prix, reverse = TRUE)(prix),
          radius = 3, stroke = FALSE, fillOpacity = 0.6,
          popup = ~paste(
            "<b>Prix:</b>", format(prix, big.mark = " "), "$", "<br>",
            "<b>Surface du terrain:</b> ", format(round(surface_terrain, 0), big.mark = " "), " m²<br>",
            "<b>Surface du plancher:</b> ", format(round(surface_plancher, 0), big.mark = " "), " m²"
          )
        ) %>%
        addLegend(
          "bottomright", pal = colorNumeric("magma", miami$prix, reverse = TRUE),
          values = miami$prix, title = "Prix de vente"
        )
      
      
      
    } else if (input$segment == "quartiers") {
      variable_name <- names(variable_names[variable_names == input$variable])
      
      leafletProxy("map", data = miami_quartiers) %>%
        clearMarkers() %>%
        addPolygons(
          fillColor = ~colorNumeric("magma", miami_quartiers[[input$variable]], reverse = TRUE, na.color = "transparent")(get(input$variable)),
          fillOpacity = 0.5, weight = 1, color = "black",
          label = ~lapply(paste(
            format_segment(input$segment), LABEL, "<br>",
            format_variable(input$variable, get(input$variable))
          ), HTML),
          labelOptions = labelOptions(
            style = list("font-size" = "12px", "font-weight" = "normal"),
            direction = "auto",
            textOnly = FALSE
          )
        ) %>%
        addLegend(
          "bottomright", pal = colorNumeric("magma", miami_quartiers[[input$variable]], reverse = TRUE, na.color = "transparent"),
          values = miami_quartiers[[input$variable]], title = variable_name, bins = 5
        )
      
      
      
    } else if (input$segment == "zip") {
      variable_name <- names(variable_names[variable_names == input$variable])
      
      leafletProxy("map", data = miami_zip) %>%
        clearMarkers() %>%
        addPolygons(
          fillColor = ~colorNumeric("magma", miami_zip[[input$variable]], reverse = TRUE, na.color = "transparent")(get(input$variable)),
          fillOpacity = 0.5, weight = 1, color = "black",
          label = ~lapply(paste(
            format_segment(input$segment), ZIPCODE, "<br>",
            format_variable(input$variable, get(input$variable))
          ), HTML),
          labelOptions = labelOptions(
            style = list("font-size" = "12px", "font-weight" = "normal"),
            direction = "auto",
            textOnly = FALSE
          )
        ) %>%
        addLegend(
          "bottomright", pal = colorNumeric("magma", miami_zip[[input$variable]], reverse = TRUE, na.color = "transparent"),
          values = miami_zip[[input$variable]], title = variable_name, bins = 5
        )
    } else if (input$segment == "none") {
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls()
    }
  })
  
  
  
  
  output$map2 <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(lng = -80.191788, lat = 25.761681, zoom = 10)
  })
  
  
  observe({
    leafletProxy("map2") %>% 
      clearShapes() %>% 
      clearControls()
    
    if (input$segment2 == "zip2") {
      bbox <- sf::st_bbox(miami_zip[miami_zip$ZIPCODE == input$zipname, ])
      
      bbox_list <- list(
        lng1 = bbox["xmin"],
        lat1 = bbox["ymin"],
        lng2 = bbox["xmax"],
        lat2 = bbox["ymax"]
      )
      
      leafletProxy("map2", data = miami_zip[miami_zip$ZIPCODE == input$zipname, ]) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          color = "black",
          weight = 2,
          fillColor = "lightblue",
          fillOpacity = 0.5,
          label = ~paste("Code ZIP:", ZIPCODE)
        ) %>%
        fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]], 
                  lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
        
      
    } else if (input$segment2 == "quartiers2") {
      bbox <- sf::st_bbox(miami_quartiers[miami_quartiers$LABEL == input$quartiername, ])
      
      bbox_list <- list(
        lng1 = bbox["xmin"],
        lat1 = bbox["ymin"],
        lng2 = bbox["xmax"],
        lat2 = bbox["ymax"]
      )
      
      leafletProxy("map2", data = miami_quartiers[miami_quartiers$LABEL == input$quartiername, ]) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          color = "black",
          weight = 2,
          fillColor = "lightblue",
          fillOpacity = 0.5,
          label = ~paste("Quartier:", LABEL)
        ) %>%
        fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]], 
                  lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
      
      
    } else if (input$segment2 == "none2") {
      leafletProxy("map2") %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls() %>%
        setView(lng = -80.191788, lat = 25.761681, zoom = 10)
    }
  })
  
  zone_stats <- reactive({
    if (input$segment2 == "zip2") {
      tempo <- miami_zip %>%
        filter(ZIPCODE == input$zipname) %>%
        summarise(
          "Prix moyen" = paste0(format(round(moy_prix, 0), big.mark = "&nbsp;"), "&nbsp;$"),
          "Surface moyenne de terrain" = paste0(format(round(moy_surface_terrain, 0), big.mark = ""), " m²"),
          "Surface moyenne de plancher" = paste0(format(round(moy_surface_plancher, 0), big.mark = " "), " m²"),
          "Distance moyenne aux transports" = paste0(format(round(moy_dist_transport, 0), big.mark = " "), " m"),
          "Distance moyenne à l'océan" = paste0(format(round(moy_dist_ocean, 0), big.mark = " "), " m"),
          "Distance moyenne au centre d'affaires" = paste0(format(round(moy_dist_business_district, 0), big.mark = " "), " m"),
          "Âge moyen des maisons" = paste0(format(round(moy_age_maison, 0), big.mark = " "), " ans"),
          "Nombre de ventes" = nb_ventes
        )
      as.data.frame(tempo)[-length(tempo)]
      
    } else if (input$segment2 == "quartiers2") {
      tempo <- miami_quartiers %>%
        filter(LABEL == input$quartiername) %>%
        summarise(
          "Prix moyen" = paste0(format(round(moy_prix, 0), big.mark = "&nbsp;"), "&nbsp;$"),
          "Surface moyenne de terrain" = paste0(format(round(moy_surface_terrain, 0), big.mark = " "), " m²"),
          "Surface moyenne de plancher" = paste0(format(round(moy_surface_plancher, 0), big.mark = " "), " m²"),
          "Distance moyenne aux transports" = paste0(format(round(moy_dist_transport, 0), big.mark = " "), " m"),
          "Distance moyenne à l'océan" = paste0(format(round(moy_dist_ocean, 0), big.mark = " "), " m"),
          "Distance moyenne au centre d'affaires" = paste0(format(round(moy_dist_business_district, 0), big.mark = " "), " m"),
          "Âge moyen des maisons" = paste0(format(round(moy_age_maison, 0), big.mark = " "), " ans"),
          "Nombre de ventes" = nb_ventes
        )
      as.data.frame(tempo)[-length(tempo)]
        
    } else if (input$segment2 == "none2") {
      miami %>%
        summarise(
          "Prix moyen" = paste0(format(round(mean(prix, na.rm = TRUE), 0), big.mark = "&nbsp;"), "&nbsp;$"),
          "Surface moyenne de terrain" = paste0(format(round(mean(surface_terrain, na.rm = TRUE), 0), big.mark = " "), " m²"),
          "Surface moyenne de plancher" = paste0(format(round(mean(surface_plancher, na.rm = TRUE), 0), big.mark = " "), " m²"),
          "Distance moyenne aux transports" = paste0(format(round(mean(dist_transport, na.rm = TRUE), 0), big.mark = " "), " m"),
          "Distance moyenne à l'océan" = paste0(format(round(mean(dist_ocean, na.rm = TRUE), 0), big.mark = " "), " m"),
          "Distance moyenne au centre d'affaires" = paste0(format(round(mean(dist_business_district, na.rm = TRUE), 0), big.mark = " "), " m"),
          "Âge moyen des maisons" = paste0(format(round(mean(age_maison, na.rm = TRUE), 0), big.mark = " "), " ans"),
          "Nombre de ventes" = length(prix)
        )
    }
  })
  
  
  output$zoneTable <- renderUI({
    stats <- zone_stats()
    
    if (nrow(stats) > 0) {
      HTML(
        paste0(
          "<div class='table-container'>",
          "<table>",
          "<thead><tr>",
          paste0("<th>", names(stats), "</th>", collapse = ""),
          "</tr></thead>",
          "<tbody>",
          paste0(
            apply(stats, 1, function(row) {
              paste0("<tr>",
                     paste0("<td>", row, "</td>", collapse = ""),
                     "</tr>")
            }),
            collapse = ""
          ),
          "</tbody>",
          "</table>",
          "</div>"
        )
      )
    } else {
      HTML("<p>Aucune donnée disponible</p>")
    }
  })
  
}


shinyApp(ui = ui, server = server)

















################################################################################


























