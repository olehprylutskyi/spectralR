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


## Clean up
rm(veg_types, veg_types_presence, nc, shapefile)


####### Reflectance - small (reflectance_test_data.RData)

## Testing reflectance data
rfl <- system.file("testdata/reflectance_test_data.RData", package = "spectralR")

## Test if the file exists
expect_false(rfl %in% "")

## Load the file
expect_silent( load(rfl) )
expect_true( "reflectance" %in% ls() )

## Test the data class
expect_inherits(reflectance, "data.frame")

## Test for the number of rows and columns
expect_equal(dim(reflectance), c(2060, 11))

## Check if a column with vegetation data is present in the data
expect_true("label" %in% colnames(reflectance))

## Check for the expected vegetation types
veg_types <- c("coniferous_forest", "cropland", "meadow", "reed", "water")
veg_types_presence <- veg_types %in% unique(reflectance$label)
expect_true( sum(veg_types_presence) == length(veg_types) )

## Check if all Sentinel optical bands are present in the data
bands <- c("B11", "B12", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A")
bands_presence <- bands %in% colnames(reflectance)
expect_true( sum(bands_presence) == length(bands) )

## Check data types - all bands should be numeric
expect_true( all(sapply(reflectance[, bands], is.numeric)) )

## Check for missing data
expect_false( any(is.na(reflectance)) )


## Clean up
rm(reflectance, rfl, veg_types, veg_types_presence, bands, bands_presence)
