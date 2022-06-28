#' Make spectral reflectance curves for defined classes of surface
#'
#' @param data reflectance data as dataframe with pixel values for Sentinel optical bands
#' B2, B3, B4, B5, B6, B7, B8, B8A, B11, B12
#'
#' @param target_classes list of the classes of surface which should be highlighted, others
#' will be turned in gray, as a background. Defaults is NULL.
#'
#' @return ggplot2 object with basic visual aesthetics, represents smoother lines with confidence
#' intervals for each surface class. Default aesthetic is smoother curve (geom_smooth).
#' May be time-consuming depending on input dataframe size.
#' See https://ggplot2.tidyverse.org/reference/geom_smooth.html for more details.
#'
#' @export
#' @import tibble reshape2 dplyr ggplot2
#' @importFrom rlang .data
#'
#' @examples
#' # Load example data
#' load(system.file("testdata/reflectance_test_data.RData", package = "spectralR"))
#' 
#' # Create a plot
#' p <- spectral.curves.plot(data = reflectance)
#'
#' # Customize a plot
#' p +
#'   ggplot2::labs(x = 'Wavelength, nm', y = 'Reflectance',
#'       colour = "Surface classes",
#'       fill = "Surface classes",
#'       title = "Spectral reflectance curves for different classes of surface",
#'       caption = 'Data: Sentinel-2 Level-2A')+
#'   ggplot2::theme_minimal()
#'
#' # Highlight only specific target classes
#' spectral.curves.plot(
#'   data = reflectance,
#'   target_classes = list("meadow", "coniferous_forest")
#'   )
#'
spectral.curves.plot <- function(data, target_classes = NULL){
  # Create "dummy" wavelength object, containing mean wavelengths (nm) for Sentinel 2A
  # (https://en.wikipedia.org/wiki/Sentinel-2), for bands 2-12

  dummy_wavelength <-  c(492.4, 559.8, 664.6, 704.1, 740.5, 782.8, 832.8, 864.7, 1613.7, 2202.4)
  bands <-  c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12")
  waves <-  cbind(bands, dummy_wavelength)
  colnames(waves)[1] <-  "variable"

  # Reshape the dataframe to make it appropriate to ggplot2 syntax
  df <- tibble::as_tibble(data) %>%
    reshape2::melt(id = "label") %>%
    left_join(as.data.frame(waves)) %>%
    mutate(across(.data$label, as.factor)) %>%
    mutate(across(dummy_wavelength, as.numeric)) %>%
    mutate(across(.data$variable, as.factor)) %>%
    mutate(across(.data$value, as.numeric)) %>%
    mutate(variable = factor(.data$variable,
                             levels = c("B2","B3","B4","B5","B6","B7","B8","B8A","B11","B12"))) %>%
    na.omit()

  if (is.null(target_classes)) {
    target_classes = as.list(levels(df$label))
  } else {
    target_classes = target_classes
  }

  if (length(target_classes) < length(levels(df$label))) {
    # Create a subset for target classes only
    target <- df %>%
      filter(.data$label %in% target_classes)
    # Create a subset for the rest of the classes
    background <- df %>%
      filter(!.data$label %in% target_classes)
    # Make a plot
    p <- ggplot()+
      geom_smooth(data = background,
        aes_string(x = "dummy_wavelength", y = "value", group = "label"),
        colour = "gray", fill = "gray",
        method = 'gam', formula = y ~ s(x, bs = "cs"), se = TRUE) +
      geom_smooth(data = target,
        aes_string(x = "dummy_wavelength", y = "value", colour = "label", fill = "label"),
        method = 'gam', formula = y ~ s(x, bs = "cs"), se = TRUE)
  } else {
    p <- ggplot(data = df, aes_string(x = "dummy_wavelength", y = "value", colour = "label"))+
      geom_smooth(aes_string(fill = "label"),
        method = 'gam', formula = y ~ s(x, bs = "cs"), se = TRUE)
  }

  return(p)
}
