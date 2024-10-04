library(terra)
library(leaflet)
library(viridis)

dir_shp <- paste0("C:/Users/equih/1 Nubes/OneDrive - El Instituto de Ecología/",
                  "Documentos - Proyecto Integralidad Gamma/",
                  "03 Documentos en preparación/02 Libro UNAM/mapas/")
mapas_iie <- list.files(paste0(dir_shp, "Mex-IIE-ZVH-Albers/Capas/Estados/"),
                        full.names = TRUE, pattern = ".tif$")

mapas_iie_mun <- list.files(paste0(dir_shp, "Mex-IIE-ZVH-Albers/Capas/"),
                           pattern = "IIE-2018-Municipal-Albers.shp",
                           full.names = TRUE)
mapa_muni_v <- vect(mapas_iie_mun_v[30]) 
mapa_muni$`_mean` <- mapa_muni$`_mean` * 100

WGS84 <- "+init=EPSG:4326"
nfld <- project(mapa_muni_v, WGS84)
plot(nfld, "_mean")

#r <- 100 * rast(mapas_iie[9])

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), mapa_muni$`_mean`,
                    na.color = "transparent")

leaflet() %>% 
  addTiles() %>%
  addPolygons(data = nfld, layerId = "_mean",fillColor = "yellow")


#  addLegend(pal = pal, values = nfld$`_mean`,
#            title = "Integridad Ecosistémica")





