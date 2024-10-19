library(rgee)

#collection <- ee$ImageCollection("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS")$
#  select("stable_lights")$map(createTimeBand)

#col_reduce <- collection$reduce(ee$Reducer$linearFit())
#col_reduce <- col_reduce$addBands(
#  col_reduce$select("scale")
#)

#ee_print(col_reduce)

#Map$setCenter(9.08203, 47.39835, 3)
#Map$addLayer(
#  eeObject = col_reduce,
#  visParams = list(
#    bands = c("scale", "offset", "scale"),
#    min = 0,
#    max = c(0.18, 20, -0.18)
#  ),
#  name = "stable lights trend"
#)

#ee$Authenticate(auth_mode='notebook')
#ee$Initialize(project='ee-miequiz')
#ee$String('Hello from the Earth Engine servers!')$getInfo()


library(rgee)
ee_Initialize()
ee_path <- path.expand("~/.config/earthengine/credentials")
ee_path <- path.expand("~/.config/earthengine/rgee_sessioninfo.txt")


image <-  ee$Image('projects/ee-miequiz/assets/iie-mex-2018');
# Center the Map.
Map$setCenter(-101, 23, zoom = 5);
# Display the image.
Map$addLayer(eeObject = image,
visParams = list(
                  bands = "b1",
                  min = 0,
                  max = 1,
                  palette = c("red", "yellow", "green")),
name = "stable lights trend")

