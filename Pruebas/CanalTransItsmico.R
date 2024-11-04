library(ggplot2)

poblacion <- data.frame(personas =c( 7.98, 8.65, 9.06, 9.32, 10.03, 10.65,
                                     11.51, 11.75), 
                        tiempo = c(seq(1990, 2010, by = 5), 2020, 2025, 2030))
barplot(height = poblacion$personas)
ggplot(poblacion, aes(x = tiempo, y = personas, fill = tiempo)) + 
  geom_col() +
  theme_classic()
