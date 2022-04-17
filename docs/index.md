---
title: "spectralR: Spectral reflectance visualisations for user-defined areas"
author: "Oleh Prylutskyi"
date: "2022-04-17"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{spectralR: Spectral reflectance visualisations for user-defined areas}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## Introduction

`spectralR` [homepage and source code](https://github.com/olehprylutskyi/spectralR) is aimed to obtain, process, and visualize spectral reflectance data for the user-defined earth surface classes (it might be different habitat or vegetation types, crops, land uses, landscapes, of any other types of territories or water areas), for visual exploring in which wavelengths the classes differ. Input should be a shapefile with polygons of surface classes (it might be different habitat types, crops, any other things). The single source of spectral data are [Sentinel 2 Level 2A](https://sentinels.copernicus.eu/web/sentinel/missions/sentinel-2) satellite mission optical bands pixel data so far, obtained through [Google Earth Engine](https://earthengine.google.com/) service.

Initial purpose of `spectralR` development was to quickly explore visually the differences in spectral reflectance patterns for supervised vegetation (and, widely, habitat) mapping. While machine learning classification methods, such as [RandomForests](https://en.wikipedia.org/wiki/Random_forest), typically utilize the full set of available spectral data (satellite bands), we observed that highly similar habitat types (e.g. different types of grasslands, floodplain habitats) may have similar reflectance values during one season and different for another, so time-series reflectance data are also needed. Without the preliminary selection of the most discriminating satellite bands for given habitat types in given conditions, the models became overfitted and required extensive machine resources for calculation.

The [first version](https://github.com/olehprylutskyi/habitat-spectral-reflectance) of our product was a three different codes, two written for R (shapefile preparation and visualization of the results) and one for Google Earth Engine, for satellite image preparation and spectral data sampling itself. That approach, though, required a bunch of manual operations, as well as downloading big files on a local machine. So we moved to the recent [rgee](https://r-spatial.github.io/rgee/) R package, which provides a bridge between R and Python API for Google Earth Engine. All the operations with satellite images now run in a cloud, and afterwards obtained pixel data visualize locally. Therefore, despite of extent of input data, the most resource hungry operations do not overload your local machine. But that means that you need a stable Internet connection for using API.

For using `rgee` you should have a Google Earth Engine account. If you don't, first [register](https://earthengine.google.com/new_signup/) using your Google account.

Depends on operating system you use and your current Python configuration, it may require some additional R and Python packages for running `rgee`. See the following links for instructions. See also [Quick Start User's Guide](https://www.rdocumentation.org/packages/rgee/versions/1.0.7), [Official documentation](https://r-spatial.github.io/rgee/index.html), and the [source code](https://github.com/r-spatial/rgee) of rgee for solving issues arises during installation and setting up.

We strongly encourage you to follow official `rgee` installation guide and messages arrived during installation process.

The overall workflow is following:

1. Load user's ESRI shapefile containing polygons for user-defined surface classes,
as well as the text or numerical field with classes names (labels).
2. Apply `rgee` functionality to retrieve multi-band pixel data for classes polygons from 
Google Earth Engine service.
3. Visualize retrieved pixel data locally, using `ggplot2` approach.

Essential requirements:

* stable Internet connection (for using API)

* Installed and correctly pre-configured Python environment (v. 3.5 or above)

* active Google Earth Engine account


## Essential preparations. Install and set up `rgee`

```r
remotes::install_github("r-spatial/rgee")
```

Load the library

```r
library(rgee)
```

It is necessary just once to complete installation necessary dependencies

```r
ee_install()
```

If something went wrong in this step, see `rgee`'s [installation guide](https://r-spatial.github.io/rgee/index.html#installation)

Check non-R dependencies

```r
ee_check() 
```

`rgee` developers recommend installing the version of the Earth Engine Python API which `rgee` was tested with, using the following command. Despite it calls "upgrade", it might actually downgrade your version of `earthengine_api`.

```r
ee_install_upgrade()
```

Initialize Google Earth Engine API for current session.

```r
ee_Initialize()
```

On this or one of previous step you would be prompted to link your Google earth Engine account with `rgee`. Follow instructions in R console, and you will be re-directered to web browser for logging into your Google Earth Engine account and confirm access rights. An authorization code generated during this step should be pasted into R console to finalize authentification.

If everything is OK on the last step and you see a message of successful initiation of API with your GEE username in console, - congratulations, you managed to install and configure `rgee`!

Pay attention, that `rgee` uses earthengine_api package, which depends on Python. That's why we need to setting up local Python environment for using `rgee`.

Unfortunately, you have to repeat environment setting and re-authorization each time Python gets updates. For actively updated operating systems, like regular versions of Ubuntu, that's quite annoying. On the other hand, this built-in repairing system protects you from accidentally breaking earthengine Python environment during installation or using any other Python-related tools, which is convenient at least if you are such a weak Python user as me. If you get an error message 

> "The current Python PATH: /home/user/.virtualenvs/rgee/bin/python does not have the Python package "earthengine-api" installed. Are you restarted/terminated your R session after install miniconda or run ee_install()?"

then proceed instruction appeared in R console, which will help you to delete your previous configuration and set up it again.

## Installation of spectralR

`spectralR` can be installed from **GitHub** sources so far, although we are planning to land it on CRAN soon.

```r
library(remotes)
install_github("olehprylutskyi/spectralR")
```

--------------------------------------------------------

We offer two use-cases for getting users familiar with the functionality of spectralR. First one is a small-size area in Kharkiv region, Ukraine, where National Park "Homilsha Forests" are situated, with 8 polygons for 5 land use classes -- let's call it "small data". The second - a "large data" - is the 380 hand-drawing polygons of 26 different habitat types (according to EUNIS classification system) of Buzky Gard National Park, Mykolaiv region, Ukraine. End users don't expect to notice a large differences in workflow, but under the hood we implemented two different algorithms for either "small" or "large" spatial data, which will be explained later.

## Use case 1. Basic habitat types of 'Homilsha Forests' National Park and neighborhoods.

### Environment preparation

```r
# Reset R's brain before new analysis session started. It will erase all the objects stored in 
# R memory, while keep loaded libraries.
rm(list = ls())

# Load required packages
library(tidyverse)
library(rgee)
library(sf)
library(geojsonio)
library(reshape2)
library(spectralR)
```

### Upload and process vector data

Function `prepare.vector.data`  takes shapefile with polygons of different classes of surface (habitats, crops, vegetation, etc.), and retrieves ready-for-sampling sf object. One should specify shapefile name (should be within working directory, using absolute paths were not tested), as well as the name of the field which contains class labels. The function extract geometries of all polygons, and marked them by custom labels ('label') as well as automatically assign integer class ID ('class') for each entry. The last variable is required because Google Earth Engine sampler respects only numerical class ID - don't delete any field!  don't panic: resulting dataframe will be marked according to your custom labels, not hard-to-memorizable numbers.

While preparing a shapefile with custom polygons (e.d., in [QGIS](https://qgis.org/en/site/)), try to follow recommendation:
* if possible, draw polygons in homogeneous landscapes, avoid class mixture;
* keep geometries simple, if possible. Avoid multipolygons, holes inside polygons, etc.
* tiny polygons (same size as satellite imagery resolution or lesser) resulted in stronger "edge effect";
* few big polygons easier to process than a lot of small ones;
* GEE has its own memory limitation, which may result in extended processing time


```r
# Extract polygons from shapefile and prepare sf object with proper structure
sf_df <- prepare.vector.data(system.file("shapes/test_shapefile.shp", package = "spectralR"), "veget_type")
```

















































