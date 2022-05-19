#' spectralR: A package for obtaining and visualizing spectral reflectance data for earth surface polygons
#'
#' This package aims to obtain, process, and visualize spectral reflectance data for the user-defined 
#' land or water surface classes for visual exploring in which wavelength the classes differ. 
#' Input should be a shapefile with polygons of surface classes (it might be different habitat types, 
#' crops, vegetation, etc.). The Sentinel-2 L2A satellite mission optical bands pixel data 
#' are obtained through the Google Earth Engine service and used as a source of spectral data.
#' 
#' @section Currently spectralR package provides several main functions:
#' \code{\link{get.pixel.data}}
#' \code{\link{prepare.vector.data}}
#' \code{\link{spectral.curves.plot}}
#' \code{\link{stat.summary.plot}}
#' \code{\link{violin.plot}}
#'
#' @docType package
#' @name spectralR
NULL
#> NULL
