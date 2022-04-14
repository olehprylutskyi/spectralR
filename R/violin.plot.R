#' Reflectance per bands as violin plots for each surface class
#'
#' @param data reflectance data as dataframe with pixel values for Sentinel optical bands
#' B2, B3, B4, B5, B6, B7, B8, B8A, B11, B12
#'
#' @return ggplot2 object with basic visual aesthetics. Default aesthetics is
#' violin plot for each satellite band (geom_violin).
#' See https://ggplot2.tidyverse.org/reference/geom_violin.html for more details.
#' @export
#'
#' @examples
#' p3 <- violin.plot(
#'   data = reflectance
#'   )
#'
#' p3
#'
#' p3 +
#'   labs(x='Surface class',y='Reflectance',
#'       fill="Surface classes",
#'       title = "Reflectance for different surface classes",
#'       caption='Data: Sentinel-2 Level-2A')+
#'    theme_minimal()
violin.plot <- function(data){
  # Create "dummy" wavelength object, containing mean wavelengths (nm) for Sentinel 2A
  # (https://en.wikipedia.org/wiki/Sentinel-2), for bands 2-12

  dummy_wavelength <-  c(492.4, 559.8, 664.6, 704.1, 740.5, 782.8, 832.8, 864.7, 1613.7, 2202.4)
  bands <-  c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12")
  waves <-  cbind(bands, dummy_wavelength)
  colnames(waves)[1] <-  "variable"

  # Reshape the dataframe to make it appropriate to ggplot2 syntax
  p <- tibble::as_tibble(data) %>%
    reshape2::melt(id = "label") %>%
    left_join(as.data.frame(waves)) %>%
    mutate(across(label, as.factor)) %>%
    mutate(across(dummy_wavelength, as.numeric)) %>%
    mutate(across(variable, as.factor)) %>%
    mutate(across(value, as.numeric)) %>%
    mutate(variable = factor(variable,
                             levels = c("B2","B3","B4","B5","B6","B7","B8","B8A","B11","B12"))) %>%
    na.omit() %>%
    ggplot(aes(x=label, y= value, fill = label))+
    geom_violin(trim = FALSE)+
    facet_wrap( ~ variable, ncol = 2)
}
