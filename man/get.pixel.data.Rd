% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.pixel.data.R
\name{get.pixel.data}
\alias{get.pixel.data}
\title{Obtain Sentinel-2 spectral reflectance data for user-defined vector polygons}
\usage{
get.pixel.data(sf_data, startday, endday, cloud_threshold, scale_value)
}
\arguments{
\item{sf_data}{polygons of surface classes as a sf object, created through prepare.vector.data}

\item{startday}{starting day for Sentinel image collection, as "YYYY-MM-DD". See Note 1 below}

\item{endday}{final day for Sentinel image collection, as "YYYY-MM-DD"}

\item{cloud_threshold}{maximum percent of cloud-covered pixels per image by which individual}

\item{scale_value}{the scale of resulting satellite images in meters (pixel size). See Note 2 below}
}
\value{
A dataframe (non-spatial) with unscaled reflectance data for each pixel of median satellite
image, for each optical band of Sentinel-2 sensor, marked according to the label of surface class from
the polygons.

Note 1.
Particular satellite imagery is typically not ready for instant analysis - it contains clouds,
cloud shadows, athmospheric aerosols, and may cover not all the territory of your interest.
Another issue is that each particular pixel slightly differs in reflectance
between images taken on different days due to differences in atmospheric conditions and angle
of sunlight at the moments images were taken. Google Earth Engine has its own build-in
algorithms for image pre-processing, atmospheric corrections and mosaicing, which allows to
obtain a ready-to-use, rectified image. The approach used in this script is to find a median
value for each pixel between several images within each of 10 optical bands and thereby make a
composite image. To define a set of imageries between which we will calculate the median,
we should set a timespan defining starting and final days. Sentinel-2 apparatus takes a picture
once a 5 days, so if you set up a month-long timesnap, you can expect each pixel value to be
calculated based on 5 to 6 values.

Note 2.
You may set up any image resolution (pixel size) for satellite imagery with GEE, but this is
hardly reasonable to set the finer resolution than the finest for satellite source.
The finest resolution for Sentinel data is 10 m, while using higher scale_value requires less
computational resources and returns a smaller resulting dataframe. Although sampling
satellite data performs in a cloud, there are some memory limitations placed
by GEE itself. If you are about to sample really large areas, consider setting a higher 'scale'
value (100, 1000).
More about GEE best practices: https://developers.google.com/earth-engine/guides/best_practices
}
\description{
The function takes sf polygon object, created through prepare.vector.data function, and
retrieves data frame with brightness values for each pixel intersected with polygons,
for each optical band of Sentinel-2 sensor, marked according to the label of surface
class from the polygons.
}
\examples{
\donttest{
\dontrun{
# Downlad spectral reflectance data
reflectance <-  get.pixel.data(
  sf_data = sf_df,
  startday = "2019-05-15",
  endday = "2019-06-30",
  cloud_threshold = 10,
  scale_value = 100)

head(reflectance)
}}
}
