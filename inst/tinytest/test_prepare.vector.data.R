## Tinytest tests for the `prepare.vector.data` function

## Run the function on the example shapefiles, no errors are expected
expect_silent( 
  sf_df <- prepare.vector.data(
    shapefile_name = system.file("extdata/test_shapefile.shp", package = "spectralR"),
    label_field = "veget_type")
  )

## Test the output data class
expect_inherits(sf_df, c("sf", "data.frame"))

## Test for the number of records in the shapefile
expect_equal(dim(sf_df), c(8, 3))

## Check CRS
expect_equal(sf::st_crs(sf_df)$input, "WGS 84")

## Check if all columns are present in the data
cols <- c("label", "class", "geometry")
cols_presence <- cols %in% colnames(sf_df)
expect_true( sum(cols_presence) == length(cols) )

## Test data types
expect_true( is.numeric(sf_df$class) )

## Check for missing data
expect_false( any(is.na(sf_df$label) ))

## Check that parameter `label_field` functions correctly (wrong column name)
# Expected error: Error in `sf_column %in% names(g)`: Join columns must be present in data. Problem with `label`.
expect_error( 
  prepare.vector.data(
    shapefile_name = system.file("extdata/test_shapefile.shp", package = "spectralR"),
    label_field = "abcd")
  )

## Check that parameter `label_field` functions correctly (column name not specified)
# Expected error:  argument "label_field" is missing, with no default
expect_error( 
  prepare.vector.data(
    shapefile_name = system.file("extdata/test_shapefile.shp", package = "spectralR"))
  )

## Check that parameter `shapefile_name` functions correctly (wrong shapefile name)
# Expected error:  Cannot open "abcd.shp"; The file doesn't seem to exist.
expect_error( 
  prepare.vector.data(
    shapefile_name = "abcd.shp",
    label_field = "veget_type")
  )

# Expected error:  Error: `dsn` must point to a source, not an empty string.
expect_error( 
  prepare.vector.data(
    shapefile_name = system.file("extdata/abcd.shp", package = "spectralR"),
    label_field = "veget_type")
  )

## Check that parameter `shapefile_name` functions correctly (missing shapefile name)
# Expected error:  argument "shapefile_name" is missing, with no default
expect_error( 
  prepare.vector.data(
    label_field = "veget_type")
  )

