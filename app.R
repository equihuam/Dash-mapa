library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(sf)
library(terra)
library(tidyterra)
library(ggplot2)
library(sys)
library(bs4Dash)
library(dplyr)
library(fresh)
library(stringr)
library(rmapshaper)


mapas_iie_muni_r <- list.files("./Estados/",
                        full.names = TRUE, 
                        pattern = ".tif$")

mapas_iie_muni_v <- list.files("./Estados/",
                               pattern = ".gpkg$",
                               full.names = TRUE)

bins<-c(0,12.5,25,37.5,50, 62.5,75,87.5, 95, 100)
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
datos_edos <- read.csv("datos_edos.txt", header = TRUE) |> 
              mutate(NOMGEO = if_else(NOMGEO == "México",
                                      "Estado de México", NOMGEO))

#as_tibble(search_vars_bs4dash("navbar"))

# Tema
tema <- bs4Dash_theme(
    primary = "lightblue",
    warning = "#272c30",
    success = "#A3BE8C",
    danger = "#BF616A",
    "sidebar-light-bg" = "#3B4252",
    "sidebar-light-color" = "#FFF",
    "main-bg" = "lightyellow",
    "body-color" = "#ECEFF4",
    "card-bg" = "#4C566A", # bs4Card() background
    "white" = "#234364",
    "info-box-bg" = "#4C566A",  # bs4InfoBox() background
    dark = "#272c30", #  bs4DashNavbar(status = "dark") background,
    "gray-600" = "#FFF",
    "gray-900" = "#FFF",
    "navbar_light_color" = "#FFF"
  )

i_gamma <- dashboardBrand(
  title = "Integridad Ecosistémica",
  href = "http://i-gamma.net/",
  image = "https://github.com/equihuam/Dash-mapa/raw/bs4d/i-Gamma-3.png",
  opacity = 1.0)



ui <- bs4DashPage(
      freshTheme = tema,
      title = "Índice de Integridad Ecosistémica - México",
      dark =  FALSE,
      
      header = bs4DashNavbar(
        title =i_gamma,
        border = TRUE,
        fixed = TRUE,
        tags$style(
          type = 'text/css', 
          '.brand-link {color: white!important;
                        font-size: 1rem;}',
          '.nav-link {color: white!important;}',
          'pre {background-color: white!important;}',
          '.navbar-white {background-color: #3B4252; }',
          '.card-body {line-height: 1.2;}'
        )),
      
      sidebar = bs4DashSidebar(
        minified = FALSE,
        collapsed = FALSE,
        elevation = 5,
        width = "17%",
        overlay = FALSE,
        selectInput(
          inputId = "estado",
          "Elige la entidad",
          choices = edos_lista$edo,
          selected = "Aguascalientes"),
        box(
          title = "Rangos de IIE",
          collapsed = TRUE,
          width = 12,
          id = "acordeon_1",
            sliderInput(
              inputId = "iie_min",
              "Límite mínimo:",0, 100, value = 0),
            sliderInput(
              inputId = "iie_max",
              "Límite máximo:",
              0, 100, value = 100)),
        box(
          title = "Estadística", 
          width = 12,
          collapsed = TRUE,
          verbatimTextOutput("muni"),
          plotOutput("iie_h", height = 135)),
        box(
          title = "Explicación",
          width = 12,
          maximizable = TRUE,
          uiOutput(
            outputId = "intro",
            container = tags$small)
      )),
      
      controlbar = dashboardControlbar(
        id = "controlbar",
        collapsed = FALSE,
        pinned =TRUE,
        width = "15%",
        bs4Card(
          title = "Leyenda",
          width = 12,
          height = 290,
          solidHeader = TRUE,
          leafletOutput(
              outputId = "leyenda",
              width = "95%",
              height = "95%"))
      ),
      
      body = bs4DashBody(
        use_theme(tema),
        fluidRow(
          box(
          title = textOutput("edo_tit0"),
                                 boxToolSize = "sm",
                                 width = 9,
                                 height = 600,
          tabsetPanel(
            tabItem(
              "municipios",
              title =  "Vectores",    # textOutput("edo_tit1"),
              solidHeader = TRUE,
              leafletOutput(
                outputId = "map_v",
                width = 760,
                height = 520)),
            tabItem(
              "pixeles",
              title = "pixeles",      # textOutput("edo_tit2"),
              solidHeader = TRUE,
              leafletOutput(
                outputId = "map_r",
                width = 760,
                height = 520))
            )))))

server <- function(input, output, session) {
  output$intro <- renderUI(markdown(readLines("explicación.txt")),
                           outputArgs = )
    
  output$map_v <-  renderLeaflet({
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
                  layerId = ~ id,
                  opacity = 1,
                  stroke = TRUE,
                  smoothFactor = 0.03)
    })
  
  output$map_r <- renderLeaflet({
    pixeles <- mapa_r()
    names(pixeles) <- "iie"
    pixeles <- pixeles |> 
      mutate(iie = if_else((iie <= input$iie_max) &
                           (iie >= input$iie_min), iie, NA), 
             .keep = "none")
    
    leaflet() |> 
    addRasterImage(pixeles, colors = cal) |> 
    addPolygons(data = mapa(),
                  fillOpacity = 0,
                  weight = 0.5,
                  color = "darkblue",
                  # popup = ~ NOMGEO,
                  layerId = ~ id,
                  opacity = 1,
                  stroke = TRUE,
                  smoothFactor = 0.03)
  })
  
  output$leyenda <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) |> 
      setView(lng = -71.0589, lat = 42.3601, zoom = 12) |> 
      addLegend(position = "topleft",
                title = "IIE (%)",
                pal = cal, 
                values = values(mapa_r()),
                opacity = 1,
      )})  
  
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
                 aes(xintercept = cotas_iie),
                 color = colores(), 
                 linewidth = 0.5, 
                 linetype = tipo_l())
  }) 
  
  
  # Cambio de mapa
  output$edo_tit0 <- renderText({input$estado})
#  output$edo_tit1 <- renderText({input$estado})
#  output$edo_tit2 <- renderText({input$estado})
  
  mapa <- reactive({
    colores(c("red", "red", "darkgreen"))
    mapa <- st_read(edos_lista$vect[grepl(input$estado, 
                                 edos_lista$edo)][1], 
                    quiet = TRUE) |> 
      st_transform(WGS84) |> 
      mutate(IIE_2018_mean = IIE_2018_mean * 100,
             id = 1:n()) |> 
      ms_simplify()})
            
  mapa_r <- reactive({
    mapa_r <- 100 * rast(edos_lista$rast[grepl(input$estado, 
                                               edos_lista$edo)][1])
  })

  iie_2018_ini <- reactive({
    iie_2018 <- datos_edos |> 
      filter(input$estado == str_replace_all(NOMGEO, " ", "_")) |> 
      mutate(iie = format(iie.2018_mean, digits = 2, 
                          nsmall = 1), .keep = "used")
  })

  vals_ini <- eventReactive(input$estado, {
      tibble(cotas_iie = c(cuantiles_iie, iie_2018_ini()$`iie.2018_mean`))
     })
  
  valores <- reactive({
    if(is.null(input$map_shape_click)) return(vals_ini())
       else {
         iie <- mapa()$IIE_2018_mean[mapa()$id ==input$map_shape_click$id]
         return(tibble(cotas_iie = c(cuantiles_iie, iie)))
       } 
    })
  

  color_ini <- reactiveVal()
  
  colores <- reactiveVal(c("red", "red", "darkgreen"))
  
  tipo_l <- eventReactive(input$estado, {
              if(is.null(input$map_shape_click)) 
                return(c("dotted", "dashed", "solid"))
              else c("dotted", "dashed", "solid")})

  observeEvent(input$estado, {
   output$muni <- renderText(paste0("Estado:\n  ",
                             input$estado,
                             "\nIIE-2018: ",
                             iie_2018_ini()$iie, " %"))
                    })
  
  #click on polygon
  
  observeEvent(input$map_v_shape_click, {
    colores(c("red", "red", "blue"))
    map_click <- input$map_v_shape_click
    municipio <- mapa()$NOMGEO[mapa()$id == map_click$id]
    iie_2018 <- format(mapa()$IIE_2018_mean[mapa()$id == map_click$id], 
                       digits = 2, nsmall = 1)
    output$muni <- renderText(paste0("Municipio:\n  ", municipio,
                                     "\nIIE-2018: ", iie_2018, " %"))
    return("municipio")
  }) 

  observeEvent(input$map_r_shape_click, {
    colores(c("red", "red", "blue"))
    map_click <- input$map_r_shape_click
    municipio <- mapa()$NOMGEO[mapa()$id == map_click$id]
    iie_2018 <- format(mapa()$IIE_2018_mean[mapa()$id == map_click$id], 
                       digits = 2, nsmall = 1)
    output$muni <- renderText(paste0("Municipio:\n  ", municipio,
                                     "\nIIE-2018: ", iie_2018, " %"))
    return("municipio")
  }) 
  
    
}

shinyApp(ui, server)

