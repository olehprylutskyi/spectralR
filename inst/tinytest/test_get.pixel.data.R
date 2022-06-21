## Tinytest tests for the `get.pixel.data` function

## NB. rgee requires authorization at Google’s Earth Engine service,
##     therefore GEE API should be initialized prior running automatic tests
# ee_Initialize()

## Load the testing dataset
expect_silent( 
  sf_df <- prepare.vector.data(
    shapefile_name = system.file("extdata/test_shapefile.shp", package = "spectralR"),
    label_field = "veget_type")
  )

## Convert sf data into ee featureCollection object
# rgee::sf_as_ee(sf_df)

# reflectance <- get.pixel.data(
#   sf_data = sf_df,
#   startday = "2019-05-15",
#   endday = "2019-06-30",
#   cloud_threshold = 10,
#   scale_value = 100)

