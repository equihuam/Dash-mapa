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

mapas_iie_muni_r <- list.files("./Estados/",
                        full.names = TRUE, 
                        pattern = ".tif$")

mapas_iie_muni_v <- list.files("./Estados/",
                               pattern = ".gpkg$",
                               full.names = TRUE)

bins<-c(0,12.5,25,37.5,50, 62.5,75,87.5, 100,Inf)
cal <- colorBin(palette="RdYlGn", 
                domain = c(0, 100), 
                bins=bins,
                na.color = "#00000000")


WGS84 <- "+init=EPSG:4326"
edos_lista <- tibble(edo = unlist(lapply(mapas_iie_muni_v, function(x) 
                     sub(".gpkg", "", basename(x)))), 
                     vect = mapas_iie_muni_v,
                     rast = mapas_iie_muni_r) |> 
              arrange(edo)

cuantiles_iie <- as.numeric(read.csv("cuantiles_iie.txt"))
datos_edos <- read.csv("datos_edos.txt")

ui = dashboardPage(
      title = "Promedios Municipales",
      header = dashboardHeader(title = "Integridad Ecosistémica",
                               titleWidth = 280),
      
      sidebar = dashboardSidebar(collapsed = FALSE, width = 280, 
                        selectInput(inputId = "estado", 
                                    "Elige la entidad", 
                                    choices = edos_lista$edo,
                                    selected = "Aguascalientes"),
                        sliderInput(inputId = "iie_min",
                                    "Límite mínimo:",
                                    0, 100, value = 0),
                        sliderInput(inputId = "iie_max",
                                    "Límite máximo:",
                                    0, 100, value = 100),
                        box(title = "Lugar", width = 12,
                            verbatimTextOutput("muni")),
                        box(width = 12, height = 170, 
                            plotOutput("iie_h", width = 200, 
                                       height = 150)),
                      
                        tags$head(tags$style("#muni{color: blue;
                                              font-size: 14px;
                                              font-style: bold;}"))),
      
      body = dashboardBody(tabBox(width = 960,
                      tabPanel("Municipios",
                        fluidRow(       
                           box(title = textOutput("edo_tit1"),
                                       solidHeader = TRUE,
                                       width = 12, 
                                       height = 590,
                                       leafletOutput(outputId = "map",
                                                     width = 730, 
                                                     height = 530)))),
                      tabPanel("Raster",
                        fluidRow(
                           box(title = textOutput("edo_tit2"),
                                       solidHeader = TRUE,
                                       width = 12, 
                                       height = 590,
                                       leafletOutput(outputId = "map_r",
                                                     width = 730, 
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
                  weight = 0.2,
                  color = "gray",
                  #popup = ~ NOMGEO,
                  layerId = ~ id,
                  opacity = 1,
                  stroke = TRUE,
                  smoothFactor = 0.03)
    })
  
  output$map_r <- renderLeaflet({
    leaflet() |> 
    addRasterImage(mapa_r(), colors = cal) |> 
    addLegend(position = "topright",
                pal = cal, values = values(mapa_r()),
                opacity = 1)
    })  
  
  output$iie_h <- renderPlot({
    ggplot(tibble(x = values(mapa_r(), na.rm = T)), 
           aes(x = x, y = after_stat(density))) + 
      geom_histogram(fill = cal(seq(0, 100, by=2)), 
                     binwidth = 2, 
                     na.rm = TRUE, 
                     color="grey", 
                     show.legend = FALSE) +
      ylab(label = "d(frecuencia)") +
      xlab(label = "Condición ecosistémica") +
      scale_fill_brewer(palette = "RdYlGn") + 
      geom_vline(data = tibble(valores()), 
                 aes(xintercept = valores),
                 color = colores(), 
                 linewidth = 0.5, 
                 linetype = tipo_l())
    }) 

  # Cambio de mapa
  output$edo_tit1 <- renderText({input$estado})
  output$edo_tit2 <- renderText({input$estado})
  
  mapa <- reactive({
            mapa <- st_read(edos_lista$vect[grepl(input$estado, 
                                         edos_lista$edo)][1], 
                            quiet = TRUE) 
            mapa <- mapa |> 
              st_transform(WGS84) |> 
              mutate(IIE_2018_mean = IIE_2018_mean * 100,
                     id = 1:n()) |> 
              st_simplify(preserveTopology = TRUE, dTolerance = 100)})
            
  mapa_r <- reactive({
              mapa_r <- 100 * rast(edos_lista$rast[grepl(input$estado, 
                                                   edos_lista$edo)][1])})
  #cuantiles <- reactive ({quantile(values(mapa_r(),
  #                                        na.rm = T),
  #                                 c(0.333, 0.667),
  #                                 na.rm=TRUE)})
  
  vals_ini <- reactive({
      valores <- tibble(valores = cuantiles_iie)
     })
  
  valores <- reactive({
               if(length(input$map_shape_click) == 0) return(vals_ini())
                 else {
                   iie <- mapa()$IIE_2018_mean[mapa()$id ==input$map_shape_click$id]
                   valores <- tibble(valores = c(cuantiles_iie, iie))
                 } 
              })
  
  color_ini <- reactive(colores <- c("red", "red"))

  colores <- reactive({ 
                if (length(input$map_shape_click) > 0) 
                  c("red", "red", "blue")
                else return(color_ini())
                })
  
  tipo_ini <- reactive(c("dotted", "dashed"))
  
  tipo_l <- reactive({
              if(length(input$map_shape_click) == 0) 
                return(tipo_ini())
              else c("dotted", "dashed", "solid")})

  output$muni <- reactive({
                        iie_2018 <- datos_edos$`iie.2018_mean`[
                                                grepl(input$estado, 
                                                datos_edos$NOMGEO)] 
                        iie_2018 <- format(iie_2018, digits = 2, nsmall = 1)
                        output$muni <- renderText(paste0("Estado:\n  ", 
                                                         input$estado,
                                                         "\nIIE-2018: ", 
                                                         iie_2018, " %"))
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

