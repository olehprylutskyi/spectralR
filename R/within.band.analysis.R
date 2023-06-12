#' Per-band visual exploration of spectral profiles of two user-specified classes
#'
#' @description Visual per-band analysis of the pair of user-specified classes.
#' Returns two types of plots, each accompanied with information on statistical differences
#' in reflectance values in pair of classes per band. Three methods for exploring
#' differences are used: (i) either parametric Student's t-Test or non-parametric
#' Wilcoxon test, (ii) level of differences in class means, and (iii) confidence intervals
#' for a differences in class means.
#'
#' @param data reflectance data as dataframe with pixel values for Sentinel optical bands
#' B2, B3, B4, B5, B6, B7, B8, B8A, B11, B12
#'
#' @param target_classes list of two classes of surface to analyse.
#'
#' @param method available statistical method for significance in differences
#' ("wilcoxon", or "t-test")
#'
#' @return list of four: .$boxplot, .$bandplot, .$confidenceIntervals, .$significantDifferences
#'
#' @export within.band.analysis
#'
#' @import tibble reshape2 dplyr ggplot2 DescTools
#' @importFrom stats t.test
#' @importFrom stats wilcox.test
#'
#' @examples
#' # Load example data
#' load(system.file("testdata/reflectance_test_data.RData", package = "spectralR"))
#'
#' # Check available class names
#' levels(reflectance$label)
#'
#' # Analyze differences in spectral profiles of selected classes per band
#' two.class.difs <- within.band.analysis(data = reflectance,
#'                                        target_classes = list("deciduous_forest", "coniferous_forest"),
#'                                        method = "t-test")
#' # Boxplot across all bands
#' two.class.difs$boxplot
#'
#' # Statistical summary plot across bands
#' two.class.difs$bandplot
#'
#' # Differences in means, lower, upper, ad full confidence intervals for
#' a differences in class means.
#' two.class.difs$confidenceIntervals
#'
#' # Significance in differences between selected classes per band, according to
#' selected statistical test.
#' two.class.difs$significantDifferences
#'
within.band.analysis <- function(data,
                                 target_classes,
                                 method = list("wilcoxon", "t-test")) {

  mytheme <- theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  if(is.null(target_classes)) {
    print("Error: No classes provided. You shall explicitly specify a list of two classes")
  } else if (length(target_classes == 2)) {
    # Calculate 95% confidence intervals for a difference in means
    # Create an empty list
    ci_dif <- list()

    # Populate this list
    for (i in (1:(ncol(reflectance) - 1))) {
      # Compose a formula linked band name and grouping variable with class names
      formula <- as.formula(paste0(colnames(reflectance[i]), " ~ label"))
      # 95% confidence intervals for a difference in means
      test.res <- DescTools::MeanDiffCI(formula = formula, data = reflectance, subset = label %in% target_classes)
      # Store meandiff, lower CI, and upper CI for each band separately
      ci_dif[[i]] <- c(colnames(reflectance[i]), test.res[[1]], test.res[[2]], test.res[[3]])
    }

    # Convert confidence intervals into a dataframe
    ci_dif_df <- ci_dif  %>%
      as.data.frame() %>% t() %>%  as_tibble(.name_repair = 'unique') %>%
      rename(band = 1,
             meandiff = 2,
             lwr.ci = 3,
             upr.ci = 4
      ) %>%
      mutate(band = as.factor(band)) %>%
      mutate_if(is.character, as.numeric) %>%
      mutate(full.ci = upr.ci - lwr.ci) %>%
      arrange(factor(band,
                     levels = c("B2","B3","B4","B5","B6","B7","B8","B8A","B11","B12")))


    # Greatest difference
    # in means
    greatest.dif.means <- ci_dif_df %>%
      arrange(abs(meandiff)) %>%
      slice_tail(n = 4)

    # in width of confidcence intervals for a difference in means
    greatest.ci.means <- ci_dif_df %>%
      arrange(full.ci) %>%
      slice_tail(n = 4)


    # Significance of difference between surface classes per band
    # Mann-Whitney (Wilcoxon) test / Student's t-test
    # Create an empty list to populate with a loop
    p_values <- list()

    for (i in (1:(ncol(reflectance) - 1))) {
      # Compose a formula linked band name and grouping variable with class names
      formula <- as.formula(paste0(colnames(reflectance[i]), " ~ label"))

      if (method == "wilcoxon") {
        # Perform Wilcoxon (Mann-Whitney) pairwise comparison test
        test.res <- wilcox.test(formula = formula, data = reflectance, subset = label %in% target_classes)
      } else if (method == "t-test") {
        # Perform Student's t-Test for pairwise comparisons per band
        test.res <- t.test(formula = formula, data = reflectance, subset = label %in% target_classes)
      } else {
        warning("Failed to apply selected method. Please choose one of the available method (either 'wilcoxon' or 't-test')")
      }
      # Store band name and obtained p-value for each band separately
      p_values[[i]] <- c(colnames(reflectance[i]), test.res[[3]])
    }

    # Convert band names and p-values into a dataframe
    wc_df <- p_values  %>%
      as.data.frame() %>% t() %>% as_tibble(.name_repair = 'unique') %>%
      rename(band = 1,
             p_value = 2
      ) %>%
      mutate(band = as.factor(band)) %>%
      mutate_if(is.character, as.numeric) %>%
      mutate(sign.level = case_when(
        p_value > 0.05 ~ "non-signficant" ,
        p_value <= 0.05 & p_value > 0.01 ~ "*",
        p_value <= 0.01 & p_value > 0.001 ~ "**",
        p_value <= 0.001 ~ "***"
      )) %>%
      arrange(factor(band,
                     levels = c("B2","B3","B4","B5","B6","B7","B8","B8A","B11","B12")))


    # Retrieve indeces of the bands with significant differences
    signifbands <- which(wc_df$p_value <= 0.05)

    band.plot <- stat.summary.plot(reflectance, target_classes = target_classes) +
      annotate(xmin = signifbands - 0.5, # index of
               xmax = signifbands + 0.5,
               ymin = 0, ymax = Inf,
               geom = 'rect', alpha = 0.2) +
      annotate(x = greatest.dif.means$band,
               y = max(reflectance[, ncol(reflectance)-1] - 0.01),
               geom = "text", label = "~") +
      annotate(x = greatest.ci.means$band,
               y = max(reflectance[, ncol(reflectance)-1] - 0.03),
               geom = "text", label = "*") +
      labs(caption = "shadowed - significant differences
       ~ highest differences in means
       * highest confidence intervals for a difference in means"
      ) +
      theme_bw()

    # Boxplot
    # Boxplot all bands w/ significant differences
    box.plot <- reflectance %>%
      filter(label %in% target_classes) %>%
      pivot_longer(cols = 1:(ncol(reflectance)-1),
                   names_to = "band",
                   values_to = "value") %>%
      mutate(band = factor(band,
                           levels = c("B2","B3","B4","B5","B6","B7","B8","B8A","B11","B12"))) %>%
      ggplot(aes(x = band, y = value, color = label)) +
      geom_boxplot(
        outlier.alpha = 0.3
      ) +
      annotate(xmin = signifbands - 0.5, # index of
               xmax = signifbands + 0.5,
               ymin = 0, ymax = Inf,
               geom = 'rect', alpha = 0.2) +
      annotate(x = greatest.dif.means$band,
               y = max(reflectance[, ncol(reflectance)-1] - 0.01),
               geom = "text", label = "~") +
      annotate(x = greatest.ci.means$band,
               y = max(reflectance[, ncol(reflectance)-1] - 0.03),
               geom = "text", label = "*") +
      labs(x = "Satellite band",
           y = "Reflectance value",
           colour = "Surface\nclasses",
           caption = "shadowed - significant differences
       ~ highest differences in means
       * highest confidence intervals for a difference in means"
      ) +
      mytheme

    outputs <- list(
      box.plot,
      band.plot,
      ci_dif_df,
      wc_df
    )

    names(outputs) <- c(
      "boxplot",
      "bandplot",
      "confidenceIntervals",
      "significantDifferences"
    )

    return(outputs)
  }
  else {
    print("Error: Wrong mumber of target classes provided. You shall explicitly specify a list of two classes")
  }
}
