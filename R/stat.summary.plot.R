#' Make a plot with statistical summary of reflectance values (mean, mean-standard deviation,
#' mean+standard deviation) for defined classes of surface
#'
#' @param data reflectance data as dataframe with pixel values for Sentinel optical bands
#' B2, B3, B4, B5, B6, B7, B8, B8A, B11, B12
#'
#' @param target_classes list of the classes of surface which should be highlighted, others
#' will be turned in gray, as a background. Defaults is NULL.
#'
#' @return ggplot2 object with basic visual aesthetics. Default aesthetics are
#' line with statistical summary for each satellite band (geom_line + geom_pointrange),
#' See https://ggplot2.tidyverse.org/reference/geom_linerange.html
#' https://ggplot2.tidyverse.org/reference/geom_path.html for more details.
#' Wavelengths values (nm) acquired from mean known value for each optical band of Sentinel 2 sensor
#' https://en.wikipedia.org/wiki/Sentinel-2
#'
#' @export
#' @import tibble reshape2 dplyr ggplot2
#' @importFrom stats na.omit
#' @importFrom stats sd
#'
#' @examples
#' \dontrun{
#' p <- stat.summary.plot(
#'   data = reflectance
#'   )
#'
#' p
#'
#' p +
#'   labs(x = 'Sentinel-2 bands', y = 'Reflectance',
#'       colour = "Surface classes",
#'       title = "Reflectance for different surface classes",
#'       caption='Data: Sentinel-2 Level-2A\nmean ± standard deviation')+
#'    theme_minimal()
#'
#'  p <- stat.summary.plot(
#'    data = reflectance,
#'    target_classes = list("C2.2", "C2.3", "J5")
#'   )
#' }
stat.summary.plot <- function(data, target_classes = NULL,
  point_size = 0.6, fatten = 4, x_dodge = 0.2){
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
    mutate(across(label, as.factor)) %>%
    mutate(across(dummy_wavelength, as.numeric)) %>%
    mutate(across(variable, as.factor)) %>%
    mutate(across(value, as.numeric)) %>%
    na.omit() %>%
    mutate(variable = factor(variable, ordered = TRUE,
                             levels = c("B2","B3","B4","B5","B6","B7","B8","B8A","B11","B12"))) %>%
    group_by(variable, label) %>%
    summarise(mean_refl = mean(value), min_refl = mean(value)-sd(value), max_refl = mean(value)+sd(value)) %>%
    left_join(as.data.frame(waves)) %>%
    mutate(across(dummy_wavelength, as.numeric)) %>%
    rename(band = variable, wavelength = dummy_wavelength) %>%
    mutate(band = factor(band,
                         levels = c("B2","B3","B4","B5","B6","B7","B8","B8A","B11","B12")))

  if (is.null(target_classes)) {
    target_classes = as.list(levels(df$label))
  } else {
    target_classes = target_classes
  }

  if (length(target_classes) < length(levels(df$label))) {
    # Create a subset for target classes only
    target <- df %>%
      filter(label %in% target_classes)

    # Create a subset for the rest of the classes
    background <- df %>%
      filter(!label %in% target_classes)

    # Make a plot
    p <- ggplot()+
      geom_line(
        data = background,
        aes(x=band, y=mean_refl, group = label),
        colour = "gray",
        position = position_dodge(width = x_dodge))+
      geom_pointrange(
        data = background,
        aes(x=band, y=mean_refl, ymin = min_refl, ymax = max_refl),
        colour = "gray",
        size = point_size, fatten = fatten,
        position = position_dodge(width = x_dodge)) +
      geom_line(
        data = target,
        aes(x=band, y=mean_refl, colour = label, group = label),
        position = position_dodge(width = x_dodge))+
      geom_pointrange(
        data = target,
        aes(x=band, y=mean_refl, colour = label, ymin = min_refl, ymax = max_refl),
        size = point_size, fatten = fatten,
        position = position_dodge(width = x_dodge))

  } else {
    p <- ggplot(df, aes(x=band, y=mean_refl, colour = label))+
      geom_line(aes(group = label), position = position_dodge(width = x_dodge))+
      geom_pointrange(
        aes(ymin = min_refl, ymax = max_refl),
        size = point_size, fatten = fatten,
        position = position_dodge(width = x_dodge))
  }

  return(p)
}
