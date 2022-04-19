---
title: "spectralR: Spectral reflectance visualisations for user-defined areas"
author: "Oleh Prylutskyi"
date: "2022-04-19"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{spectralR: Spectral reflectance visualisations for user-defined areas}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## Introduction

The `spectralR` package ([homepage and source code](https://github.com/olehprylutskyi/spectralR)) is aimed to obtain, process, and visualize spectral reflectance data for the user-defined earth surface classes for visual exploring in which wavelengths the classes differ. User-defined classes might represent different habitat or vegetation types, crops, land uses, landscapes, or other types of territories or water areas. Input should be a shapefile with polygons of surface classes (it might be different habitat types, crops, or other things). So far, the single source of spectral data is [Sentinel 2 Level 2A](https://sentinels.copernicus.eu/web/sentinel/missions/sentinel-2) satellite mission optical bands pixel data so far, obtained through the [Google Earth Engine](https://earthengine.google.com/) service.

The initial purpose of `spectralR` development was to quickly and visually explore the differences in spectral reflectance patterns for supervised vegetation (and, widely, habitat) mapping. While machine learning classification methods, such as [RandomForest](https://en.wikipedia.org/wiki/Random_forest), typically utilize the full set of available spectral data (satellite bands), we observed that highly similar habitat types (e.g., different types of grasslands, floodplain habitats) may have similar reflectance values during one season and different for another, so time-series reflectance data are also needed. Without the preliminary selection of the most discriminating satellite bands for given habitat types in given conditions, the models became overfitted and required extensive machine resources for calculation.

The [first version](https://github.com/olehprylutskyi/habitat-spectral-reflectance) of our product was based on three code snippets, two written for R (shapefile preparation and visualization of the results) and one for Google Earth Engine for satellite image preparation and spectral data sampling itself. That approach, though, required a bunch of manual operations, as well as downloading big files on a local machine. So we moved to the recent [rgee](https://r-spatial.github.io/rgee/) R package, which provides a bridge between R and Python API for Google Earth Engine. All the operations with satellite images now run in a cloud, and the obtained pixel data is visualized locally afterward. Therefore, the most resource-hungry operations do not overload your local machine even despite the large extent of input data. However, a stable Internet connection is required for using the API.

For using `rgee` you should have a Google Earth Engine account. If you don't, first [register](https://earthengine.google.com/new_signup/) using your Google account.

Depending on the operating system you use and your current Python configuration, it may require the installation of additional R and Python packages for running `rgee`. See the following links for instructions. See also [Quick Start User's Guide](https://www.rdocumentation.org/packages/rgee/versions/1.0.7), [Official documentation](https://r-spatial.github.io/rgee/index.html), and the [source code](https://github.com/r-spatial/rgee) of rgee for solving issues that arise during installation and setting up.

We strongly encourage you to follow the official `rgee` installation guide and pay attention to the messages that arrive during installation.

The overall workflow is following:

1. Load user's ESRI shapefile containing polygons for user-defined surface classes,
as well as the text or numerical field with classes names (labels).
2. Apply `rgee` functionality to retrieve multi-band pixel data for classes polygons from the Google Earth Engine service.
3. Visualize retrieved pixel data locally, using `ggplot2` approach.

Essential requirements:

* Stable Internet connection (for using API)

* Installed and correctly pre-configured Python environment (v. 3.5 or above)

* Active Google Earth Engine account


## Essential preparations

### Install and set up `rgee`

```r
remotes::install_github("r-spatial/rgee")
```

Load the library:

```r
library(rgee)
```

It is necessary just once to complete the installation of the dependencies:

```r
ee_install()
```

If something went wrong at this step, see `rgee`'s [installation guide](https://r-spatial.github.io/rgee/index.html#installation)

Check non-R dependencies

```r
ee_check() 
```

`rgee` developers recommend installing the version of the Earth Engine Python API which `rgee` was tested with, using the following command. Despite it calls "upgrade", it might actually downgrade your version of `earthengine_api`.

```r
ee_install_upgrade()
```

Initialize the Google Earth Engine API for current session:

```r
ee_Initialize()
```

At this step, you would be prompted to link your Google Earth Engine account with `rgee`. Follow instructions in the R console, and you will be redirected to a web browser for logging into your Google Earth Engine account to confirm access rights. An authorization code generated during this step should be pasted into R console to finalize authentification.

If everything is OK at the last step and you see a message of successful initiation of API with your GEE username in the console, - congratulations, you managed to install and configure rgee`rgee`!

Pay attention that `rgee` uses `earthengine_api` package, which depends on Python. That's why we need to set up a local Python environment for using `rgee`.

Unfortunately, you have to repeat environment setting and re-authorization each time Python gets updates. For actively updated operating systems, like regular versions of Ubuntu, that's quite annoying. On the other hand, this built-in repairing system protects you from accidentally breaking the `earthengine` Python environment during installation or using any other Python-related tools, which is convenient at least if you are such a weak Python user as me. If you get an error message 

> "The current Python PATH: /home/user/.virtualenvs/rgee/bin/python does not have the Python package "earthengine-api" installed. Are you restarted/terminated your R session after install miniconda or run ee_install()?"

then proceed instruction appeared in R console, which will help you to delete your previous configuration and set up it again.

### Installation of the other dependencies


```r
install.packages("geojsonio")
```

### Installation of spectralR

`spectralR` can be installed from **GitHub** sources so far, although we are planning to land it on CRAN soon.

```r
library(remotes)
install_github("olehprylutskyi/spectralR")
```

--------------------------------------------------------

We offer two use-cases for getting users familiar with the functionality of spectralR. The first one is a small-size area in the Kharkiv region, Ukraine, where National Park "Homilsha Forests" is situated, with 8 polygons for 5 land use classes -- let's call it "small data". The second - a "large data" - is the 380 hand-drawing polygons of 26 different habitat types (according to the EUNIS classification system) of Buzky Gard National Park, Mykolaiv region, Ukraine. End users don't expect to notice large differences in workflow, but under the hood we implemented two different algorithms for either "small" or "large" spatial data, which will be explained later.

## Use case 1. Basic habitat types of 'Homilsha Forests' National Park and neighborhoods.

### Environment preparation




















































