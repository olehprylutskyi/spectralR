## Tinytest test for the `violin.plot` function

## Load the small test dataset
expect_silent( 
  load(system.file("testdata/reflectance_test_data.RData", package = "spectralR"))
  )

## Execute the function to make a ggplot object with violin plots
## No errors or warnings expected
expect_silent(
  p1 <- violin.plot(data = reflectance)
  )

## Test the output data class
expect_inherits(p1, c("gg", "ggplot"))

## The plot should not be empty
expect_true( nrow(p1$data) > 0 )

## Any violins present?
expect_inherits( p1$layers[[1]]$geom, "GeomViolin" )



