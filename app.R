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
library(flexdashboard)
library(stringr)
library(rmapshaper)
library(plotly)



# Preparación de funciones y datos preliminares -------------------------

# función para dibujar dial que muestra valor de IIE----
dial_iie <- function (lugar, valor_iie = 0, n_anp = 0, n_cue = 0)
{
  if (n_anp < 0 )
    n_anp_tx <- ""
  else
    n_anp_tx <- paste0("\n  <b>Número de ANP:</b> ", n_anp)
  
  n_cue_tx <- paste0("\n  <b>Número de cuencas:</b> ", n_cue)
  
  lugar_tx <- str_wrap(lugar, 
                       width = 32, 
                       indent = 1, 
                       exdent = 3,
                       whitespace_only = TRUE ) 
  
  fig <- plot_ly(
    type = "indicator",
    title = list(text = "IIE 2018", font = list(size = 16)),
    value = valor_iie,
    number = list(suffix = "%", font= list(size = 12)),
    gauge = list(
      axis =list(range = list(NULL, 100)),
      bar = list(color = "black"),
      steps = list(
        list(range = c(0, 33.3), color = "red"),
        list(range = c(33.3, 66.6), color = "yellow"),
        list(range = c(66.6, 100), color = "lightgreen"))),
    mode = "gauge+number") |> 
    layout(margin = list(l=25,r=35, b = 45, t = 25), 
           annotations = list(x = -0.2, y = -0.35,
                              text = paste0(lugar_tx, n_anp_tx, n_cue_tx),
                              showarrow = F, xref='container', yref='container',
                              xanchor ='left', yanchor='auto',
                              xshift = 0,
                              yshift=0,
                              font=list(size=12, color="gray40")))
    
    return(fig)
}

# lista de archivos de mapas de estados: raster y vectoriales ----
mapas_iie_muni_r <- list.files("./Estados/",
                        full.names = TRUE, 
                        pattern = ".tif$")

mapas_iie_muni_v <- list.files("./Estados/",
                               pattern = ".gpkg$",
                               full.names = TRUE)

WGS84 <- "+init=EPSG:4326"  # proyección reqqerida por leaflet

# arreglo del histograma y paleta de colores ----
bins<-c(0,12.5,25,37.5,50, 62.5,75,87.5, 95, 100)
cal <- colorBin(palette="RdYlGn", 
                domain = c(0, 100), 
                bins=bins,
                na.color = "#00000000")

# datos tabulares de referencia ----------------------------

anp_tipos <- read.csv("./tablas/categorías_de_manejo.csv")

# lista de estados y nombre de archivos vectoriales y raster
edos_lista <- tibble(edo = unlist(lapply(mapas_iie_muni_v, function(x) 
  sub(".gpkg", "", basename(x)))), 
  vect = mapas_iie_muni_v,
  rast = mapas_iie_muni_r) |> 
  mutate(edo = str_replace_all(edo, "_", " ")) |> 
  arrange(edo)

# La tabla de cuantiles inicia con el año del mapa de iie
cuantiles_iie <- as.numeric(read.csv("./tablas/cuantiles_iie.csv")[,2:3])

datos_edos <- read.csv("./tablas/datos_edos.csv", header = TRUE) |> 
              mutate(NOMGEO = if_else(NOMGEO == "México",
                                      "Estado de México", NOMGEO))

datos_anp <- read.csv("./tablas/datos-anp-federales.csv")

datos_cue <- read.csv("./tablas/datos_cuencas.csv")

# Tema y arrreglos visuales ----
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
  image = "https://github.com/equihuam/Dash-mapa/raw/main/i-Gamma-3.png",
  opacity = 1.0)

# fin de preparación ----------------------------------------------------

# Interfaz --------------------------------------------------------------
ui <- bs4DashPage(
      freshTheme = tema,
      title = "Índice de Integridad Ecosistémica - México",
      dark = NULL,
      help = NULL,
      
      header = bs4DashNavbar(
        title =i_gamma,
        border = TRUE,
        fixed = TRUE,
        tags$style(
          type = 'text/css', 
          '#muni {text-wrap: wrap;}',
          '.brand-link {color: white!important;
                        font-size: 1rem;}',
          '.nav-link {color: white!important;}',
          'pre {background-color: white!important;}',
          '.navbar-white {background-color: #3B4252; }',
          '.card-body {line-height: 1.15;}'
        ), 
        sidebarIcon = icon("map")
      ),
      
      sidebar = bs4DashSidebar(
        minified = FALSE,
        collapsed = FALSE,
        elevation = 5,
        width = "17%",
        overlay = FALSE,
        
        selectInput(
          inputId = "estado",
          label = "Elige la entidad",
          choices = edos_lista$edo,
          selected = "Aguascalientes"),
    
        box(title = "Rangos de IIE",
          collapsed = TRUE,
          width = 12,
          icon = shiny::icon("signal"),
          id = "bloque_1",
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
          collapsed = FALSE,
          
          plotlyOutput(outputId = "iie_dial",
                       width = "100%",
                       height = "12em"
          ),
          
          selectInput(
            inputId = "tipo_anp",
            label = "Tipo ANP:",
            choices = c("APFF", "APRN","MN","PN","RB","SANT"),
            multiple = TRUE,
            selected = NULL),
          
          plotOutput("iie_h", height = 135))
      ),
      
      controlbar = dashboardControlbar(
        id = "controlbar",
        collapsed = FALSE,
        pinned =TRUE,
        width = "15%",
        bs4Card(
          title = "Leyenda",
          collapsed = TRUE, 
          width = 12,
          height = 290,
          solidHeader = TRUE,
          leafletOutput(
              outputId = "leyenda",
              width = "95%",
              height = "95%")),
        box(
          title = "Explicación",
          width = 12,
          elevation = 5,
          maximizable = TRUE,
          uiOutput(
            outputId = "intro",
            container = tags$small)
        )
      ),
      
      body = bs4DashBody(
        use_theme(tema),
        fluidRow(
          column(width = 12,
            box(
              title = textOutput("edo_tit0"),
              boxToolSize = "sm",
              width = 9,
              height = 600,
              maximizable = TRUE,
              
            tabsetPanel(
              tabItem(
                tabName = "municipios",
                title =  "Mapa",    # textOutput("edo_tit1"),
                solidHeader = TRUE,
                leafletOutput(
                  outputId = "map_v",
                  width = 760,
                  height = 520)),
              # tabItem(
              #   tabName = "datos",
              #   title = "datos",      # textOutput("edo_tit2"),
              #   solidHeader = TRUE),
                # leafletOutput(
                #   outputId = "map_r",
                #   width = 760,
                #   height = 520)),
              tabItem(
                tabName = "modelo_3c",
                title = "Modelo",
                solidHeader = TRUE,
                imageOutput(outputId = "modelo")
                )
              )
            )
          )
        )
      )
) # fin interfaz --------------------------------------------------------

# Implementación del servidor ----------------------------------------
server <- function(input, output, session) {
  
  # construcción de la interaz
  output$intro <- renderUI(markdown(readLines("explicación.txt")),
                           outputArgs = )
  output$modelo <-  renderImage({
    list(src = "Modelo de tres capas.png",
         alt = "Modelo de Integridad ecosisttémica de tres capas",
         width="800",
         height="400",
         style="vertical-align:middle;margin:50px 10px")},
    deleteFile = FALSE)
    
  output$map_v <-  renderLeaflet({
      pixeles <- mapa_r()
      names(pixeles) <- "iie"
      pixeles <- pixeles |>
        mutate(iie = if_else((iie <= input$iie_max) &
                               (iie >= input$iie_min), iie, NA),
               .keep = "none")
      mapa_v_filtrado <- mapa_v() |>
        filter((IIE_2018_mean <= input$iie_max) &
               (IIE_2018_mean > input$iie_min))
        
      if (anp_edo()$anp_id[1] != "sin datos")
      {
          leaflet() |> 
          addMapPane(name = "muni", zIndex = 5) |> 
          addMapPane(name = "anp", zIndex = 4) |> 
          addMapPane(name = "iie_anp", zIndex = 3) |> 
          addMapPane(name = "iie_raster", zIndex = 2) |> 
          addMapPane(name = "iie_muni", zIndex = 1) |> 
          addPolygons(
            data = mapa_v(),
            group = "municipios",
            options = pathOptions(pane = "muni"),
            fillOpacity = 0,
            weight = 1,
            color = "darkblue",
            layerId = ~ id,
            opacity = 1,
            stroke = TRUE,
            smoothFactor = 0.03) |> 
          addPolygons(
            data = mapa_v_anp(),
            group = "ANP",
            options = pathOptions(pane = "anp"),
            fillOpacity = 0,
            weight = 1,
            color = "cyan",
            layerId = ~ id_anp,
            opacity = 1,
            stroke = TRUE,
            smoothFactor = 0.03) |>
          addPolygons(
            data = mapa_v_anp(),
            group = "IIE ANP",
            layerId = ~ id_anp,
            options = pathOptions(pane = "iie_anp"),
            fillColor = ~ cal(IIE_2018_mean),
            fillOpacity = 1,
            stroke = FALSE,
            smoothFactor = 0.03) |> 
          addRasterImage( 
            group = "IIE raster",
            options = pathOptions(pane = "iie_raster"),
            x = pixeles, 
            colors = cal) |> 
          addPolygons(
            data = mapa_v_filtrado,
            fillColor = ~ cal(IIE_2018_mean),
            group = "IIE municipal",
            options = pathOptions(pane = "iie_muni"),
            fillOpacity = 1,
            stroke = FALSE,
            layerId = ~ id,
            smoothFactor = 0.03) |> 
          addLayersControl(
            overlayGroups = c(
              "municipios",
              "ANP",
              "IIE ANP",              
              "IIE raster",
              "IIE municipal"),
            options = layersControlOptions(collapsed = FALSE)) |> 
          hideGroup(group = c("IIE municipal", "IIE ANP"))
        
      } else
      {
          leaflet() |> 
          addMapPane(name = "muni", zIndex = 4) |> 
          addMapPane(name = "anp", zIndex = 3) |> 
          addMapPane(name = "iie_muni", zIndex = 2) |> 
          addPolygons(
            data = mapa_v(),
            options = pathOptions(pane = "muni"),
            group = "municipios",
            fillOpacity = 0,
            weight = 1,
            color = "darkblue",
            layerId = ~ id,
            opacity = 1,
            stroke = TRUE,
            smoothFactor = 0.03) |>
          addRasterImage( 
            options = pathOptions(pane = "anp"),
            group = "IIE raster",
            x = pixeles, 
            colors = cal) |>
          addPolygons(
            data = mapa_v_filtrado,
            options = pathOptions(pane = "iie_muni"),
            fillColor = ~ cal(IIE_2018_mean),
            group = "IIE municipal",
            fillOpacity = 1,
            stroke = FALSE,
            layerId = ~ id,
            smoothFactor = 0.03) |> 
          addLayersControl(
            overlayGroups = c("municipios",
                              "IIE raster", 
                              "IIE municipal"),
            options = layersControlOptions(collapsed = FALSE)) |> 
          hideGroup(group = c("IIE municipal"))
      }
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
  
  
  # Información por desplegar
  output$edo_tit0 <- renderText({input$estado})
  
  # componentes reactive y reactiveVal ----------------

  # Cambio de mapa
  anp_edo_gral <- reactive({
    edo <- datos_edos$ID_ENT[input$estado == datos_edos$NOMGEO]
    
    anp_edo_gral <- datos_anp |>
        filter(grepl(edo, ESTADOS_LST)) |>
        select(anp_id) |> 
        nrow()
  })

  cue_edo_gral <- reactive({
    edo <- datos_edos$ID_ENT[input$estado == datos_edos$NOMGEO]
    
    cue_edo_gral <- datos_cue |>
      filter(grepl(edo, ESTADOS_LST)) |>
      select(CVE_CUE) |> 
      nrow()
  })
    
  anp_edo <- reactive({
    edo <- datos_edos$ID_ENT[input$estado == datos_edos$NOMGEO]
    
    anp_edo <- data.frame(anp_id = "sin datos")
    if (!is.null(input$tipo_anp))
    {
      anp_edo <- datos_anp |>
        filter(grepl(edo, ESTADOS_LST) & 
                 str_detect(CAT_MANEJO, paste0(input$tipo_anp, collapse = "|"))) |>
        select(anp_id)
      
      if(nrow(anp_edo) == 0) anp_edo <- data.frame(anp_id = "sin datos")
    }
    anp_edo
  })
  

  mapa_v <- reactive({
    colores(c("red", "red", "darkgreen"))
    edo_v <- str_replace_all(
      edos_lista$vect[grepl(input$estado, edos_lista$edo)][1], " ", "_")
    mapa_v <- st_read(edo_v, quiet = TRUE) |>
      st_transform(WGS84) |> 
      mutate(IIE_2018_mean = IIE_2018_mean * 100,
             id = 1:n())
    })

  mapa_v_anp <- reactive({
    if (anp_edo()$anp_id[1] != "sin datos")
    {
      for (m in anp_edo()$anp_id)
      {
        if (m == anp_edo()$anp_id[1])
          mapa <- vect(paste0("ANP/anp_id_", m, ".gpkg"))
        else
          mapa <- rbind(mapa, vect(paste0("ANP/anp_id_", m, ".gpkg")))
      }
      mapa_v_anp <- st_as_sf(mapa) |> 
        st_transform(WGS84) |> 
        mutate(IIE_2018_mean = iie_anp_mean * 100,
               id_anp = 1:n())
    }
  })
  
  # mapa_v_anp_buf <- reactive({
  #   if (anp_edo()$anp_id[1] != "sin datos")
  #   {
  #     for (m in anp_edo()$anp_id)
  #     {
  #       if (m == anp_edo()$anp_id[1])
  #         mapa <- vect(paste0("ANP/", m, ".gpkg"))
  #       else
  #         mapa <- rbind(mapa, vect(paste0("ANP/", m, ".gpkg")))
  #     }
  #     mapa_v_anp <- st_as_sf(mapa) |> 
  #       st_transform(WGS84) |> 
  #       mutate(IIE_2018_mean = iie_anp_mean * 100,
  #              id_anp = 1:n())
  #   }
  # })
  
          
  mapa_r <- reactive({
    edo_r <- str_replace_all(edos_lista$rast[grepl(input$estado,
                                                   edos_lista$edo)][1],
                           " ", "_")
    mapa_r <- 100 * rast(edo_r)
    
    if (anp_edo()$anp_id[1] != "sin datos")
    {
      mapa_r_anp <- sapply(anp_edo()$anp_id,
                           function (x) 100 * rast(paste0("ANP/",
                                                          x, ".tif")))

      mapa_lst <- sprc(mapa_r_anp)
      mapa_r_anp <- merge(mapa_lst)
      mapa_r <- merge(mapa_r, mapa_r_anp)
    }
    
    return(mapa_r)
  })

  iie_2018 <- reactive({
    iie_2018 <- datos_edos |> 
      filter(input$estado == NOMGEO) |> 
      mutate(iie = format(iie.2018_mean, digits = 2, 
                          nsmall = 1), .keep = "used")
    return(iie_2018)
  })

  vals_ini <- eventReactive(input$estado, {
      tibble(cotas_iie = c(cuantiles_iie, iie_2018()$`iie.2018_mean`))
     })
  
  valores <- reactive({
    if(is.null(input$map_shape_click)) return(vals_ini())
    else 
      {
        iie <- mapa_v()$IIE_2018_mean[mapa_v()$id == input$map_shape_click$id]
        return(tibble(cotas_iie = c(cuantiles_iie, iie)))
       } 
    })
  

  color_ini <- reactiveVal(c("red", "red", "darkgreen"))
  
  colores <- reactiveVal(c("red", "red", "darkgreen"))
  
  tipo_l <- eventReactive(input$estado, {
              if(is.null(input$map_shape_click)) 
                return(c("dotted", "dashed", "solid"))
              else c("dotted", "dashed", "solid")})
  
  # componentes observe y observeEvent ------------------- 
  
  observeEvent(anp_edo(),{
    if (anp_edo()$anp_id[1] == "sin datos")
      num_anp <- 0
    else
      num_anp <- nrow(anp_edo())

    num_cue = 100
        
    output$iie_dial <- renderPlotly(
      dial_iie(lugar = paste0("<b>Estado:</b> ",
                              input$estado),
               valor_iie = iie_2018()$iie,
               n_anp = anp_edo_gral(),
               n_cue = cue_edo_gral()))
  })

  observeEvent(input$tipo_anp, {
    if (anp_edo()$anp_id[1] == "sin datos")
      num_anp <- 0
    else
      num_anp <- nrow(anp_edo())
    
    output$iie_dial <- renderPlotly(
      dial_iie(lugar = paste0("<b>Estado:</b> ",
                              input$estado),
               valor_iie = iie_2018()$iie,
               n_anp = num_anp))
  })
  
  #click on polygon
  
  observeEvent(input$map_v_shape_click, {
    colores(c("red", "red", "blue"))
    mapa_click <- input$map_v_shape_click
    
    if (anp_edo()$anp_id[1] == "sin datos")
      num_anp <- 0
    else
      num_anp <- nrow(anp_edo())

    if (str_detect(mapa_click$group, "muni"))
    {
      iie <- mapa_v()$IIE_2018_mean[mapa_v()$id == mapa_click$id]
      iie_2018 <- format(iie, digits = 2, nsmall = 1)
      municipio <- mapa_v()$NOMGEO[mapa_v()$id == mapa_click$id]
      output$iie_dial <- renderPlotly(
        dial_iie(lugar =paste0("<b>Municipio:</b> ", municipio), 
                 valor_iie = iie_2018,
                 n_anp = -1,
                 n_cue = cue_edo_gral()))  
    } else
    {
      if (str_detect(mapa_click$group, "ANP"))
      {
        iie <- mapa_v_anp()$IIE_2018_mean[mapa_v_anp()$id_anp == mapa_click$id]
        iie_2018 <- format(iie, digits = 2, nsmall = 1)
        anp <- mapa_v_anp()$NOMBRE[mapa_v_anp()$id_anp == mapa_click$id]

        output$iie_dial <- renderPlotly(
          dial_iie(lugar =paste0("<b>ANP:</b> ", anp), 
                   valor_iie = iie_2018, 
                   n_anp = num_anp,
                   n_cue = cue_edo_gral()))
      }
    }
  }) 
} # Fin de implementación del servidor ----------------------------------

# Ejecución de la aplicación --------------------------------------------
shinyApp(ui, server)

