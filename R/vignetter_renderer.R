library("knitr")
knit2html("./spectralR_vignette.Rmd")

# It seems you should call rmarkdown::render() instead of knitr::knit2html()
# because ./spectralR_vignette.Rmd appears to be an R Markdown v2 document.

rmarkdown::render("./spectralR_vignette.Rmd")
