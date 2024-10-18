### mapa_iie_r_f <- "C:/Users/equih/1 Nubes/OneDrive - El Instituto de Ecología/Documentos - Proyecto Integralidad Gamma/03 Documentos en preparación/02 Libro UNAM/mapas/Mex-IIE-ZVH-Albers/Capas/IIE-2018-INEGI-SEEA.tif"
### mapa_iie_v_f <- "C:/Users/equih/1 Nubes/OneDrive - El Instituto de Ecología/Documentos - Proyecto Integralidad Gamma/03 Documentos en preparación/02 Libro UNAM/mapas/Mex-IIE-ZVH-Albers/Capas/IIE-2018-Estados-Albers.gpkg"

mapa_iie_r <- rast(mapa_iie_r_f)
hist(values(mapa_iie_r),freq = FALSE )

mapa_iie_v <- vect(mapa_iie_v_f)
plot(mapa_iie_v, "iie-2018_mean")
iie_edo <- as.data.frame(mapa_iie_v)
iie_edo$`iie-2018_mean` <- iie_edo$`iie-2018_mean` * 100
write.csv(iie_edo, "datos_edos.txt", row.names = F, quote = FALSE)

plot(mapa_iie_r)
cuantiles_iie_r <- quantile(100 * values(mapa_iie_r,
                                 na.rm = T),
                          c(0.333, 0.667),
                          na.rm=TRUE)
names(cuantiles_iie) <-  c("q33", "q66")
write.csv(t(cuantiles_iie), "cuantiles_iie.txt", row.names = F)
read.csv("cuantiles_iie.txt")

ggplot(tibble(x = values(mapa_iie_r, na.rm = T)), 
       aes(x = x, y = after_stat(density))) + 
  geom_histogram(fill = cal(seq(0, 100, by=2)), 
                 binwidth = 2, 
                 na.rm = TRUE, 
                 color="grey", 
                 show.legend = FALSE) +
  ylab(label = "d(frecuencia)") +
  xlab(label = "Condición ecosistémica") +
  scale_fill_brewer(palette = "RdYlGn") + 
  geom_vline(data = tibble(valores = cuantiles_iie), 
             aes(xintercept = valores),
             color = "red", 
             linewidth = 0.5, 
             linetype = "solid")
