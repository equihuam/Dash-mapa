pal = colorNumeric("RdYlBu", domain = cycle_hire$nbikes)

r <- 100 * rast(mapas_iie[9])

leaflet(data = cycle_hire) |> 
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircles(col = ~pal(nbikes), opacity = 0.9) |> 
  addPolygons(data = lnd, fill = FALSE) |> 
  addLegend(pal = pal, values = ~nbikes) |> 
  setView(lng = -0.1, 51.5, zoom = 12) |> 
  addMiniMap()

nz_elev = rast(system.file("raster/nz_elev.tif", package = "spDataLarge"))

library(tidyterra)
library(terra)
library(ggplot2)
library(sys)
dir_base <- ifelse (grepl("MONSTRUO", Sys.getenv()["USERDOMAIN"]),
                    "D:/1 Nubes/El Instituto de Ecología/Proyecto Integralidad Gamma - Documentos/", 
                    "C:/Users/equih/1 Nubes/OneDrive - El Instituto de Ecología/Documentos - Proyecto Integralidad Gamma/")
dir_shp <- paste0(dir_base, "03 Documentos en preparación/02 Libro UNAM/mapas/")
mapas_iie <- list.files(paste0(dir_shp, "Mex-IIE-ZVH-Albers/Capas/Estados/"),
                        full.names = TRUE, 
                        pattern = ".tif$")

mapa_iie_mun_r <- 100 * rast(mapas_iie[30])


ggplot() + 
  geom_spatraster(data = mapa_iie_mun_r) +
  scale_fill_grass_c(palette = "ryg", 
                     direction = 1,
                     labels = scales::label_number(suffix = "%"),
                     n.breaks = 9) +
  labs(fill = "IIE",
       title = "Índice de integridad ecosistémica",
       subtitle = "2018 Veracruz (México)")

mapa_iie_mun_v <- list.files(paste0(dir_shp, "Mex-IIE-ZVH-Albers/Capas/Estados/"),
                           pattern = ".gpkg$",
                           full.names = TRUE)

mapa_muni_v <- vect(mapa_iie_mun_v[30]) 
names(mapa_muni_v)
mapa_muni_v$IIE_2018_mean <- mapa_muni_v$IIE_2018_mean * 100

ggplot(data = mapa_muni_v) +
  geom_spatvector(aes(fill = IIE_2018_mean)) +
  scale_fill_grass_c(palette = "ryg", direction = 1,
                     labels = scales::label_number(suffix = "%"),
                     n.breaks = 9) + 
  labs(fill = "IIE",
       title = "Índice de integridad ecosistémica",
       subtitle = "2018 - municipa, Veracruz (México)")
