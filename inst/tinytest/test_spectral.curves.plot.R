## Tinytest test for the `spectral.curves.plot` function

## Load the small test dataset
expect_silent( 
  load(system.file("testdata/reflectance_test_data.RData", package = "spectralR"))
  )


## Execute the function to make a ggplot object, use default target_classes
## No errors or warnings expected
expect_silent(
  p1 <- spectral.curves.plot(reflectance, target_classes = NULL)
  )

## Test the output data class
expect_inherits(p1, c("gg", "ggplot"))

## The plot should not be empty
expect_true( nrow(p1$data) > 0 )

## Any smoothers present?
expect_inherits( p1$layers[[1]]$geom, "GeomSmooth" )

## Function should fail if no input data is specified
# Expected error:   argument "data" is missing, with no default
expect_error( spectral.curves.plot() )

## Function should fail if incorrect data is provided
# Expected error:   id variables not found in data: label
expect_error( spectral.curves.plot(rnorm(10)) )


###### Test `target_classes` argument

## Non-existent target classes
p2 <- spectral.curves.plot(reflectance, target_classes = list("Q", "W", "Z"))
expect_true( nrow(p2$layers[[2]]$data) == 0 )


## Correctness of background and target classes
trg <- list("meadow", "coniferous_forest")
p3 <- spectral.curves.plot(reflectance, target_classes = trg)

all_classes <- unique(reflectance$label)
target <- unlist(trg)
non_target <- all_classes[ ! all_classes %in% target ]

## There are highlighted and non-highlighted classes
expect_true( nrow(p3$layers[[1]]$data) > 0 )
expect_true( nrow(p3$layers[[2]]$data) > 0 )

## There are no non-target classes highlighted
expect_false( any(! unique(p3$layers[[2]]$data$label) %in% target ) )

## There are no target classes not-highlighted
expect_false( any( unique(p3$layers[[1]]$data$label) %in% target ) )

## All targets should be highlighted
expect_true( 
  length(target) == length(unique(p3$layers[[2]]$data$label))
  )

## All non-targets should be as background curves
expect_true( 
  length(non_target) == length(unique(p3$layers[[1]]$data$label))
  )

