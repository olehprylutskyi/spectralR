#' Statistical tests for two surface classes
#'
#' @description Exploratory analysis of the pair of user-specified classes.
#' Includes two statistical tests - Shapiro-Wilk normality test and Bartlett
#' test of homogeneity of variances, - as well as class imbalance assessment
#' and histograms of distribution of reflectance values per band.
#'
#' @param data reflectance data as dataframe with pixel values for Sentinel optical bands
#' B2, B3, B4, B5, B6, B7, B8, B8A, B11, B12
#'
#' @param target_classes list of two classes of surface to analyse.
#'
#' @return list of four lists: .$normality, .$homogeneity, .$imbalance, .$plot
#' @export
#' @import tibble reshape2 dplyr ggplot2
#' @importFrom stats as.formula
#' @importFrom stats bartlett.test
#' @importFrom stats shapiro.test
#' @importFrom rlang .data
#'
#' @examples
#' # Load example data
#' load(system.file("testdata/reflectance_test_data.RData", package = "spectralR"))
#'
#' # Check available class names
#' levels(reflectance$label)
#'
#' # Calculate normality, homogeneity, class imbalance, and plot the distribution
#' stattests <- stat.tests(data = reflectance,
#'                         target_classes = list("deciduous_forest", "coniferous_forest"))
#'
#' stattests$normality # to look at Shapiro-Wilk test for normality
#'
#' stattests$homogeneity # to look at Bartlett Test of Homogeneity of Variances
#'
#' stattests$imbalance # to explore possible class imbalance
#'
#' stattests$plot # to look at reflectance values distribution per band
#'
stat.tests <- function(data, target_classes) {
  mytheme <- theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  if(is.null(target_classes)) {
    print("Error: No classes provided. You shall explicitly specify a list of two classes")
  } else if (length(target_classes != 2)) {
    print("Error: Wrong mumber of target classes provided. You shall explicitly specify a list of two classes")
  } else {
    ## Shapiro-Wilk Normality Test
    # Normality calculated for each surface class separately
    # class #1
    # Create an empty list
    normality.results <- list()
    # Populate the list
    for (i in (1:(ncol(data) - 1))) {
      test.res <- shapiro.test(subset(data, label %in% target_classes[1])[[i]])
      # Store band name, statistic, and obtained p-value for each band separately
      normality.results[[i]] <- c(colnames(data[i]), test.res[[1]], test.res[[2]])
    }
    # Convert band names, statistics, and p-values into a dataframe
    norm_df_class1 <- normality.results  %>%
      as.data.frame() %>% t() %>%  as_tibble(.name_repair = 'unique') %>%
      rename(band = 1,
             W = 2,
             p = 3
      ) %>%
      mutate(band = as.factor(band)) %>%
      mutate_if(is.character, as.numeric) %>%
      mutate(class = target_classes[[1]]) %>%
      relocate(class, .after = band) %>%
      mutate(sign.level = case_when(
        p > 0.05 ~ "non-signficant" ,
        p <= 0.05 & p > 0.01 ~ "*",
        p <= 0.01 & p > 0.001 ~ "**",
        p <= 0.001 ~ "***"
      )
      ) %>%
      arrange(factor(band,
                     levels = c("B2","B3","B4","B5","B6","B7","B8","B8A","B11","B12")))

    # class #2
    normality.results <- list()

    for (i in (1:(ncol(data) - 1))) {
      test.res <- shapiro.test(subset(data, label %in% target_classes[2])[[i]])
      # Store band name, statistic, and obtained p-value for each band separately
      normality.results[[i]] <- c(colnames(data[i]), test.res[[1]], test.res[[2]])
    }

    # Convert band names, statistics, and p-values into a dataframe
    norm_df_class2 <- normality.results  %>%
      as.data.frame() %>% t() %>%  as_tibble(.name_repair = 'unique') %>%
      # as.data.frame() %>%
      rename(band = 1,
             W = 2,
             p = 3
      ) %>%
      mutate(band = as.factor(band)) %>%
      mutate_if(is.character, as.numeric) %>%
      mutate(class = target_classes[[2]]) %>%
      relocate(class, .after = band) %>%
      mutate(sign.level = case_when(
        p > 0.05 ~  "non-signficant",
        p <= 0.05 & p > 0.01 ~ "*",
        p <= 0.01 & p > 0.001 ~ "**",
        p <= 0.001 ~ "***"
      )
      ) %>%
      arrange(factor(band,
                     levels = c("B2","B3","B4","B5","B6","B7","B8","B8A","B11","B12")))

    # Merge data frames for both classes of surface
    norm_df <- norm_df_class1 %>%
      left_join(norm_df_class2, join_by(band))


    # Extract names of bands which variances are not equal
    band.names.norm <- norm_df %>%
      filter(p.x <= 0.05 | p.y <= 0.05) %>%
      select(band) %>%
      mutate(band = factor(band, ordered = TRUE,
                           levels = c("B2","B3","B4","B5","B6","B7","B8","B8A","B11","B12"))) %>%
      as.matrix()

    # Test interpretation
    norm_interp <- if (length(band.names.norm) >= 1 ) {
      print(
        paste0("Values distribution in bands ", toString(band.names.norm), " differs from normal. Non-parametrcal tests (like Wilcoxon pairwise test) advised.")
      )
    } else {
      print("Values distribution is normal in all bands, parametric tests (like t-test) would be appropriate")

    }

    normality <- list("Shapiro-Wilk normality test for compared surface classes", norm_df, norm_interp)
    names(normality) <- c("test.name", "test.results", "test.interpretation")


    ## Bartlett Test of Homogeneity of Variances
    # If p > 0.05, accept Null Hypothesis (variance are homogeneous)

    bartlett.results <- list()

    for (i in (1:(ncol(data) - 1))) {
      # Compose a formula linked band name and grouping variable with class names
      formula <- as.formula(paste0(colnames(data[i]), " ~ label"))
      # Perform Bartlett Test of Homogeneity of Variances
      test.res <- bartlett.test(formula = formula, data = data, subset = label %in% target_classes)
      # Store band name, statistic, and obtained p-value for each band separately
      bartlett.results[[i]] <- c(colnames(data[i]), test.res[[1]], test.res[[3]])
    }

    # Convert band names, statistics, and p-values into a dataframe
    bartl_df <- bartlett.results  %>%
      as.data.frame() %>% t() %>%  as_tibble(.name_repair = 'unique') %>%
      # as.data.frame() %>%
      rename(band = 1,
             `Bartlett's K-squared` = 2,
             p = 3
      ) %>%
      mutate(p = as.numeric(p)) %>%
      mutate(band = as.factor(band)) %>%
      mutate(sign.level = case_when(
        p > 0.05 ~ "non-signficant" ,
        p <= 0.05 & p > 0.01 ~ "*",
        p <= 0.01 & p > 0.001 ~ "**",
        p <= 0.001 ~ "***"
      )
      ) %>%
      arrange(factor(band,
                     levels = c("B2","B3","B4","B5","B6","B7","B8","B8A","B11","B12")))

    # Extract names of bands which variances are not equal
    band.names.bartl <- bartl_df %>%
      filter(p <= 0.05) %>%
      select(band) %>%
      mutate(band = factor(band, ordered = TRUE,
                           levels = c("B2","B3","B4","B5","B6","B7","B8","B8A","B11","B12"))) %>%
      as.matrix()


    # Print test interpretation
    bartl_interp <- if (length(band.names.bartl) >= 1 ) {
      print(
        paste0("Variances in band(s) ", toString(band.names.bartl), " are non-homogenouos. Non-parametrcal tests (like Wilcoxon pairwise test) advised.")
      )
    } else {
      print("Variances are homogenouos in all bands, parametric tests (like t-test) would be appropriate")

    }

    # Combine all outputs of Bartlett test
    homogeneity <- list("Bartlett Test of Homogeneity of Variances", bartl_df, bartl_interp)
    names(homogeneity) <- c("test.name", "test.results", "test.interpretation")


    ## Class imbalance
    # Size of classes
    n1 <- nrow(data[data$label == target_classes[1],]) # sample 1 size
    n2 <- nrow(data[data$label == target_classes[2],]) # sample 2 size

    # Normalized difference in classes' size
    size.norm.dif <- abs((n1-n2) / (n1+n2))

    # Print message
    imbalance.sizes <- matrix(
      c(1, target_classes[[1]], n1,
        2, target_classes[[2]], n2
      ),
      nrow = 2, ncol = 3, byrow = TRUE,
      dimnames = list(c("1", "2"),
                      c("Class index", "Class Name", "Number of pixels received")
      )
    )

    print(imbalance.sizes)

    imbalance.output <- if (size.norm.dif > 0.3) {
      warning("Strong class imbalance: one of the classes covers much larger area than the other one. Further analyses might bring misleading results.")
    } else if (size.norm.dif <= 0.3 & size.norm.dif >= 0.21) {
      warning("Moderate class imbalance: one of the classes covers larger area than the other one. It might affect further analyses.")
    } else {
      print("Classes are balanced in size")
    }


    imbalance <- list("Imbalance in classes", imbalance.sizes, imbalance.output)
    names(imbalance) <- c("test.name", "test.results", "test.interpretation")

    # Plot distribution of pixel values
    dist.plot <- data %>%
      filter(label %in% target_classes) %>%
      pivot_longer(cols = 1:(ncol(data)-1),
                   names_to = "band",
                   values_to = "value") %>%
      mutate(band = factor(band,
                           levels = c("B2","B3","B4","B5","B6","B7","B8","B8A","B11","B12"))) %>%
      ggplot(aes(x = value, fill = label)) + # Plot (histogram)
      geom_histogram() +
      facet_wrap(vars(band), ncol = 2, scales = "free") +
      labs(x = "data value",
           y = "number of pixels",
           fill = "Surface\nclasses"
      ) +
      mytheme

    outputs <- list(
      normality,    # Shapiro-Wilk Normality Test
      homogeneity,  # Bartlett Test of Homogeneity of Variances
      imbalance,    # Class imbalance
      dist.plot     # comparable plots of values distribution
    )

    names(outputs) <- c("normality", "homogeneity", "imbalance", "plot")

    return(outputs)
  }
}
