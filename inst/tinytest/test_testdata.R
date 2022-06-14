## Tinytest test of the spectralR package data


####### Shapefiles

## Testing shapefile
shapefile <- system.file("extdata/test_shapefile.shp", package = "spectralR")

## Test if the file exists
expect_false(shapefile %in% "")

## Load shapefile
## Test if the file loaded succefully
expect_stdout( nc <- sf::st_read(shapefile, quiet = FALSE) )

## Test the data class
expect_inherits(nc, c("sf", "data.frame"))

## Test for the number of records in the shapefile
expect_equal(dim(nc), c(8, 2))

## Check CRS
expect_equal(sf::st_crs(nc)$input, "WGS 84")

## Check if vegetation data column is present in the data
expect_true("veget_type" %in% colnames(nc))

## Check if all vegetation types are present
veg_types <- c("coniferous_forest", "water", "reed", "cropland", "meadow")
veg_types_presence <- veg_types %in% unique(nc$veget_type)
expect_true( sum(veg_types_presence) == length(veg_types) )


