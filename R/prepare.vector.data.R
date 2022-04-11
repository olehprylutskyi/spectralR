#' Prepare vector data for further reflectance data sampling
#'
#'Function takes shapefile with polygons of different classes of surface (habitats, crops,
#'vegetation, etc.), and retrieves ready-for-sampling sf object.

#' @param shapefile_name shapefile name (should lie within working directory, using absolute
#' paths were not tested)
#' @param label_field name of the field which contains class labels
#'
#' @return Result of calculation
#' @export
#'
#' @examples sf_df <- prepare.vector.data("test_shapefile.shp", "veget_type")
prepare.vector.data <- function(shapefile_name, label_field){
  # Upload a shapefile with polygons of knowing surface classes.
  # The shapefile must contain a text field with classes labels
  nc <-  st_read(shapefile_name, quiet = TRUE)

  # Specify which field contains classes labels
  # label <-  label_field

  # Create a 'class' variable for integer class IDs
  names(nc)[names(nc) == label_field] <- 'label'

  # make a list of label values (types of surface) and its numerical IDs
  classes_cheatsheet <- as.data.frame(levels(factor(nc$label)))
  classes_cheatsheet$class <- rownames(as.data.frame(levels(factor(nc$label))))
  colnames(classes_cheatsheet) <- c("label", "class")
  classes_cheatsheet <-  classes_cheatsheet %>%
    mutate(across(label, as.factor)) %>%
    mutate(across(class, as.numeric))

  # Add class IDs
  nc <- left_join(nc, classes_cheatsheet, by = "label")
  nc$class <- as.numeric(nc$class)
  # delete objects with NA in the target variable
  nc <- nc[!is.na(nc$label) ,]
}

