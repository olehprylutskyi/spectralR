#' Visual tests for classes separability
#'
#' @description Classes separability is a keystone of supervised image classification.
#' To evaluate this, {sprectralR} provides Principal Component Analysis (PCA) for
#' reflectance data, showing the variability, separability, and compactness
#' of defined classes. Well separated, compact clouds of points reflect optically
#' distinguished classes. Whereas spread, split, or overlapped clouds of points
#' indicate that user's classes are possibly intermixed or, maybe, just cannot be
#' separated with the present remote sensing data. User can also make and write to
#' the working directory a series of scatter plots for each possible band combination,
#' to find out in which band classes are intermixed.
#'
#' @param data a dataframe with reflectance data containing pixel values for Sentinel optical bands
#' B2, B3, B4, B5, B6, B7, B8, B8A, B11, B12
#'
#' @param target_classes a list of any number of classes of surface to analyse.
#' All classes available in data are taken by default.
#'
#' @param scatterplot logical. If TRUE, band-to-band scatter plots will be written
#' to the working directory. FALSE by default.
#'
#' @return list of four plots: three PCA for different combination of PCs, a screeplot
#' to PCA diagnostics.
#'
#' @export classes.separability
#' @import tibble reshape2 dplyr ggplot2 FactoMineR factoextra
#' @importFrom RactoMineR PCA
#' @importFrom factoextra fviz_pca_biplot
#' @importFrom factoextra fviz_eig
#' @importFrom utils combn
#' @importFrom rlang .data
#'
#' @examples
#' # Load example data
#' load(system.file("testdata/reflectance_test_data.RData", package = "spectralR"))
#'
#' # Check available class names
#' levels(reflectance$label)
#'
#' # Explore separability in all classes
#' separability_all <- classes.separability(reflectance)
#'
#' # Plot data values in a gradient of two main Principal Components
#' separability_all
#'
#' # Another way to plot Principal Components 1 and 2
#' separability$PC1.PC2
#'
#' # Plot PC2 and PC3
#' separability$PC2.PC3
#'
#' # Scree plot (Fraction of total variance covered by each Principal Component)
#' separability$screeplot
#'
#' # Explore separability for three selected classes, and write a pand-to-band plots
#' # into the working directory
#' separability_some_classes <- classes.separability(data = reflectance,
#'                                      target_classes = list("cropland", "meadow", "reed"),
#'                                      scatterplot = TRUE)
#'
#' separability_some_classes
#'
classes.separability <- function(data,
                       target_classes = NULL,
                       scatterplot = FALSE) {

  mytheme <- theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  # Define which classes include (all or some user-specified ones)
  if(is.null(target_classes)) {
    data <- data
  } else {
    data <- data %>%
      filter(label %in% target_classes)
  }

  # Calculate PCA
  res.pca <- PCA(data %>% select(-label), scale.unit = TRUE, ncp = 5, graph = FALSE)

  # Make a biplot of individuals and variables.
  p1.2 <- fviz_pca_biplot(res.pca,
                          axes = c(1, 2),
                          geom.ind = "point",       # show points only (but not "text")
                          col.ind = data$label, # color by groups
                          legend.title = "Surface\nclass",
                          mean.point = FALSE,       # remove the group mean point
                          repel = TRUE              # Avoid text overlapping
  ) +
    labs(title = "Principal Components 1 and 2",
    ) +
    mytheme

  p1.3 <- fviz_pca_biplot(res.pca,
                          axes = c(1, 3),
                          geom.ind = "point",
                          col.ind = data$label,
                          legend.title = "Surface\nclass",
                          mean.point = FALSE,
                          repel = TRUE
  ) +
    labs(title = "Principal Components 1 and 3"
    ) +
    mytheme

  p2.3 <- fviz_pca_biplot(res.pca,
                          axes = c(2, 3),
                          geom.ind = "point",
                          col.ind = data$label,
                          legend.title = "Surface\nclass",
                          mean.point = FALSE,
                          repel = TRUE
  ) +
    labs(title = "Principal components 2 and 3",
    ) +
    mytheme

  # Scree plot
  scree.plot <- fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

  outputs <- list(
    scree.plot,
    p2.3,
    p1.3,
    p1.2
  )

  names(outputs) <- c(
    "screeplot",
    "PC2.PC3",
    "PC1.PC3",
    "PC1.PC2"
  )

  # Plot band-to-band scatterplots
  if(scatterplot == TRUE) {
    # All possible combinations of bands
    band.combinaions <- combn(colnames(data[-ncol(data)]), 2)

    # Print and save 2-band scatterplots for each possible band combination
    for (i in 1:ncol(band.combinaions)) {
      # First band in pair
      band1 <- band.combinaions[1, i]
      # Second band in pair
      band2 <- band.combinaions[2, i]

      # Make band-to-band scatterplot
      myPlot <- ggplot(data = data,
                       aes(x = .data[[band1]], y = .data[[band2]],
                           colour = label)
      ) +
        geom_point(alpha = 0.4) +
        labs(colour = "Surface\nclass") +
        mytheme

      print(myPlot)

      # Save results as a set of local *.png files

      ggsave(paste0("scatterplot_bands_", band1, "_", band2, ".png"),
             plot = myPlot,
             height = 8, width = 12, units = "cm", dpi = 150)
    }
  }

  return(outputs)
}
