library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(sf)
library(shinydashboardPlus)
library(shinydashboard)
library(terra)
library(tidyterra)
library(ggplot2)
library(sys)
library(bs4Dash)
library(dplyr)
library(fresh)

dir_base <- ifelse (grepl("MONSTRUO", Sys.getenv()["USERDOMAIN"]),
                    "D:/1 Nubes/El Instituto de Ecología/Proyecto Integralidad Gamma - Documentos/", 
                    "C:/Users/equih/1 Nubes/OneDrive - El Instituto de Ecología/Documentos - Proyecto Integralidad Gamma/")
dir_shp <- paste0(dir_base, "03 Documentos en preparación/02 Libro UNAM/mapas/")

mapas_iie <- list.files(paste0(dir_shp, "Mex-IIE-ZVH-Albers/Capas/Estados/"),
                        full.names = TRUE, 
                        pattern = ".tif$")
mapa_iie_muni_r <- 100 * rast(mapas_iie[30])

mapas_iie_muni_v <- list.files(paste0(dir_shp, 
                                      "Mex-IIE-ZVH-Albers/Capas/Estados/"),
                               pattern = ".gpkg$",
                               full.names = TRUE)

bins<-c(0,12.5,25,37.5,50, 62.5,75,87.5, 100,Inf)
cal <- colorBin(palette="RdYlGn", domain = mapa_ver$IIE_2018_mean., bins=bins)

mapa_muni_v |> 
  filter((IIE_2018_mean <= 30) &
         (IIE_2018_mean > 20)) |> 
  plot("IIE_2018_mean")

ggplot(tibble(x = values(mapa_iie_muni_r)), 
       aes(x = x, y = ..density..)) + 
  geom_histogram(fill = cal(seq(0, 100, by=2)), 
                 binwidth = 2, 
                 na.rm = TRUE, 
                 color="grey", 
                 show.legend = FALSE) +
  scale_fill_brewer(palette = "RdYlGn")

edos_lista <- unlist(lapply(mapas_iie_muni_v, function(x) 
  sub(".gpkg", "", basename(x))))

ui = dashboardPage(
      title = "Promedios Municipales",
      header = dashboardHeader(title = "Integridad Ecosistémica"),
      
      sidebar = dashboardSidebar(collapsed = FALSE, width = 350, 
                        selectInput(inputId = "estado", 
                                    "Elige la entidad", 
                                    choices = edos_lista,
                                    selected = "Veracruz de Ignacio de la Llave"),
                        sliderInput(inputId = "iie_min",
                                    "Mínima Integridad Ecosistémica:",
                                    0, 100, value = 10),
                        sliderInput(inputId = "iie_max",
                                    "Máxima Integridad Ecosistémica:",
                                    0, 100, value = 70),
                        box(title = "Lugar", width = 12,
                            verbatimTextOutput("muni")),
                        box(width = 12, height = 150, 
                            plotOutput("iie_h", width = 250, 
                                       height = 150)),
                      
                        tags$head(tags$style("#muni{color: blue;
                                              font-size: 14px;
                                              font-style: bold;}"))),
      
      body = dashboardBody(
                      fluidRow(box(title = textOutput("edo_tit"),
                                   status = "primary", 
                                   solidHeader = TRUE,
                                   width = 12, 
                                   height = 660,
                                   leafletOutput(outputId = "map",
                                                 width = 600, 
                                                 height = 600)))))

server = function(input, output, session) {

  output$map <-  renderLeaflet({
      mapa() |> 
      filter((IIE_2018_mean <= input$iie_max) &
             (IIE_2018_mean > input$iie_min)) |> 
      leaflet() |> 
      addPolygons(fillColor = ~ cal(IIE_2018_mean),
                  fillOpacity = 1,
                  stroke = FALSE,
                  layerId = ~ id,
                  smoothFactor = 0.03) |> 
      addPolygons(data = mapa(),
                  fillOpacity = 0,
                  weight = 0.1,
                  color = "gray",
                  #popup = ~ NOMGEO,
                  layerId = ~ id,
                  opacity = 1,
                  stroke = TRUE,
                  smoothFactor = 0.03)
      })
  
  output$iie_h <- renderPlot({
    ggplot(tibble(x = values(mapa_iie_muni_r)), 
           aes(x = x, y = ..density..)) + 
      geom_histogram(fill = cal(seq(0, 100, by=2)), 
                     binwidth = 2, 
                     na.rm = TRUE, 
                     color="grey", 
                     show.legend = FALSE) +
      scale_fill_brewer(palette = "RdYlGn")})

  # Cambio de mapa
  output$edo_tit <- renderText({input$estado})

  mapa <- reactive({
            mapa <- vect(mapas_iie_muni_v[grepl(input$estado, 
                           mapas_iie_muni_v)]) 
            mapa <- mapa |> 
              project(WGS84) |> 
              mutate(IIE_2018_mean = IIE_2018_mean * 100,
                     id = 1:n()) |>
              st_as_sf()
                      })
                                                   
  #click on polygon
  observeEvent(input$map_shape_click, {
    map_click <- input$map_shape_click
    municipio <- mapa()$NOMGEO[mapa()$id == map_click$id]
    iie_2018 <- format(mapa()$IIE_2018_mean[mapa()$id == map_click$id], 
                       digits = 2, nsmall = 1)
    output$muni <- renderText(paste0("Municipio:\n  ", municipio,
                                     "\nIIE-2018: ", iie_2018, " %"))
  }) 
  
}

shinyApp(ui, server)

