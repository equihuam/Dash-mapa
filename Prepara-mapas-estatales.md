## Preparación de cartografía con foco estatal

- Unidades de reporte - unidades mínimas de cálculo de estadísticos de resumen (municipio, subcuenca, etc.)
- Unidad de concentración - unidad que engloba la región de interés (estado, cuenca, anp, etc.)

1. Reproyección a Albers 
2. Calcular estadísticas zonales (Estadísticas de zona) según unidad de reporte (media, número, varianza)
3. Dividir los polígonos según unidad de concentración del reporte y guardar los recortes
4. Recortar capa de condición raster por capa vectorial de unidades de concentración
	- Función extracción raster con la opción de iterar sobre la capa vectorial para producir un archivo separado por cada objeto