#' Make a plot with statistical summary of reflectance values (mean, mean-standard deviation,
#' mean+standard deviation) for defined classes of surface
#'
#' @param data reflectance data as dataframe with pixel values for Sentinel optical bands
#' B2, B3, B4, B5, B6, B7, B8, B8A, B11, B12
#'
#' @return ggplot2 object with basic visual aesthetics. Default aesthetics are
#' line with statistical summary for each satellite band (geom_line + geom_pointrange),
#' See https://ggplot2.tidyverse.org/reference/geom_linerange.html
#' https://ggplot2.tidyverse.org/reference/geom_path.html for more details.
#' Wavelengths values (nm) acquired from mean known value for each optical band of Sentinel 2 sensor
#' https://en.wikipedia.org/wiki/Sentinel-2
#'
#' @export
#' @import tibble reshape2 dplyr ggplot2 stats
#'
#' @examples
#' p2 <- stat.summary.plot(
#'   data = reflectance
#'   )
#'
#' p2
#'
#' p2 +
#'   labs(x = 'Sentinel-2 bands', y = 'Reflectance',
#'       colour = "Surface classes",
#'       title = "Reflectance for different surface classes",
#'       caption='Data: Sentinel-2 Level-2A\nmean ± standard deviation')+
#'    theme_minimal()
stat.summary.plot <- function(data){
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
    drop_na() %>%
    mutate(across(variable, as.factor)) %>%
    mutate(across(value, as.numeric)) %>%
    group_by(variable, label) %>%
    summarise(mean_refl = mean(value), min_refl = mean(value)-sd(value), max_refl = mean(value)+sd(value)) %>%
    left_join(as.data.frame(waves)) %>%
    mutate(across(dummy_wavelength, as.numeric)) %>%
    rename(band = variable, wavelength = dummy_wavelength) %>%
    mutate(band = factor(band,
                         levels = c("B2","B3","B4","B5","B6","B7","B8","B8A","B11","B12"))) %>%
    ggplot(aes(x=band, y=mean_refl, colour = label))+
    geom_line(aes(group = label))+
    geom_pointrange(aes(ymin = min_refl, ymax = max_refl), width = 0.2)
}
