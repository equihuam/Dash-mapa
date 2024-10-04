library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(sf)
library(terra)
library(tidyterra)
library(ggplot2)
library(sys)
library(bs4Dash)
library(shinydashboardPlus)
library(shinydashboard)
library(dplyr)

#dir_base <- ifelse (grepl("MONSTRUO", Sys.getenv()["USERDOMAIN"]),
#                    "D:/1 Nubes/El Instituto de Ecología/Proyecto Integralidad Gamma - Documentos/", 
#                    "C:/Users/equih/1 Nubes/OneDrive - El Instituto de Ecología/Documentos - Proyecto Integralidad Gamma/")
#dir_shp <- paste0(dir_base, "03 Documentos en preparación/02 Libro UNAM/mapas/")
#"Mex-IIE-ZVH-Albers/Capas"

mapas_iie_r <- list.files("./Estados/",
                        full.names = TRUE, 
                        pattern = ".tif$")

mapas_iie_muni_v <- list.files("./Estados/",
                               pattern = ".gpkg$",
                               full.names = TRUE)

mapa_ver <- vect(mapas_iie_muni_v[30])
mapa_ver$IIE_2018_mean <- 100 * mapa_ver$IIE_2018_mean
bins<-c(0,12.5,25,37.5,50, 62.5,75,87.5, 100,Inf)
cal <- colorBin(palette="RdYlGn", domain = mapa_ver$IIE_2018_mean., 
                bins=bins)

edos_lista <- unlist(lapply(mapas_iie_muni_v, function(x) 
  sub(".gpkg", "", basename(x))))

ui = dashboardPage(
      title = "Promedios Municipales",
      header = dashboardHeader(title = "Integridad Ecosistémica",
                               titleWidth = 280),
      
      sidebar = dashboardSidebar(collapsed = FALSE, width = 280, 
                        selectInput(inputId = "estado", 
                                    "Elige la entidad", 
                                    choices = edos_lista,
                                    selected = "Veracruz de Ignacio de la Llave"),
                        sliderInput(inputId = "iie_min",
                                    "Límite mínimo:",
                                    0, 100, value = 10),
                        sliderInput(inputId = "iie_max",
                                    "Límite máximo:",
                                    0, 100, value = 70),
                        box(title = "Lugar", width = 12,
                            verbatimTextOutput("muni")),
                        box(width = 12, height = 170, 
                            plotOutput("iie_h", width = 200, 
                                       height = 150)),
                      
                        tags$head(tags$style("#muni{color: blue;
                                              font-size: 14px;
                                              font-style: bold;}"))),
      
      body = dashboardBody(tabBox(width = 660,
                      tabPanel("Municipios",
                        fluidRow(       
                           box(title = textOutput("edo_tit1"),
                                       solidHeader = TRUE,
                                       width = 12, 
                                       height = 590,
                                       leafletOutput(outputId = "map",
                                                     width = 530, 
                                                     height = 530)))),
                      tabPanel("Raster",
                        fluidRow(
                           box(title = textOutput("edo_tit2"),
                                       solidHeader = TRUE,
                                       width = 12, 
                                       height = 590,
                                       leafletOutput(outputId = "map_r",
                                                     width = 530, 
                                                     height = 530)))))))

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
  
  output$map_r <- renderLeaflet({
    leaflet() |> 
    addRasterImage(mapa_r(), colors = cal)})  
  
  output$iie_h <- renderPlot({
    ggplot(tibble(x = values(mapa_r())), 
           aes(x = x, y = ..density..)) + 
      geom_histogram(fill = cal(seq(0, 100, by=2)), 
                     binwidth = 2, 
                     na.rm = TRUE, 
                     color="grey", 
                     show.legend = FALSE) +
      scale_fill_brewer(palette = "RdYlGn") + 
      geom_vline(xintercept = mapa()$IIE_2018_mean[mapa()$id == input$map_shape_click$id])})

  # Cambio de mapa
  output$edo_tit1 <- renderText({input$estado})
  output$edo_tit2 <- renderText({input$estado})

  mapa <- reactive({
            mapa <- vect(mapas_iie_muni_v[grepl(input$estado, 
                           mapas_iie_muni_v)]) 
            mapa <- mapa |> 
              project(WGS84) |> 
              mutate(IIE_2018_mean = IIE_2018_mean * 100,
                     id = 1:n()) |>
              st_as_sf()})
            
  mapa_r <- reactive({
              mapa_r <- 100 * rast(mapas_iie_r[grepl(input$estado, 
                                                     mapas_iie_r)])})
                                                   
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

