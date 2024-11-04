map_anp <- st_read("IIE-2018-ANP_Albers_continental_2024.gpkg")

ver <- map_anp |> 
  as_tibble() |> 
  select(ESTADOS_LST) |> 
  mutate(str_detect(str_to_lower(ESTADOS_LST), "veracruz"))

edos <- read.csv("datos_edos.csv") |> 
  mutate(NOM_USUAL = if_else(str_detect(NOMGEO, "Ver") |
                             str_detect(NOMGEO, "Coah") |
                             str_detect(NOMGEO, "Micho"), 
                             str_split_i(NOMGEO, " ", 1), NOMGEO),
         NOMGEO = if_else(str_detect(NOM_CAP, "Toluca"), 
                          "Estado de México", NOMGEO),
         NOMGEO = if_else(is.na(NOM_CAP), 
                          "Ciudad de México", NOMGEO)) |>
  relocate(NOM_USUAL, .after = NOMGEO)

write.csv(edos, "datos_edos.csv", quote = FALSE, row.names = FALSE)

edos
