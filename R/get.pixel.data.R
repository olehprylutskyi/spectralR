#' Obtain Sentinel-2 spectral reflectance data for user-defined vector polygons
#'
#' Function takes sf polygon object, created through prepare.vector.data function, and
#' retrieves data frame with brightness values for each pixel intersected with polygons,
#' for each optical band of Sentinel-2 sensor, marked according to the label of surface
#' class from the polygons.
#'
#' @param sf_data polygons of surface classes as a sf object, created through prepare.vector.data
#' @param startday starting day for Sentinel image collection, as "YYYY-MM-DD". See Note 1 below
#' @param endday final day for Sentinel image collection, as "YYYY-MM-DD"
#' @param cloud_threshold maximum per cent of cloud-covered pixels per image by which individual
# satellite imageries will be filtered
#' @param scale_value scale of resulting satellite images in meters (pixel size). See Note 2 below
#'
#' @return A dataframe (non-spatial) with unscaled reflectance data for each pixel of median satellite
#' image, for each optical band of Sentinel-2 sensor, marked according to the label of surface class from
#' the polygons.
#'
#' Note 1.
#' Particular satellite imagery is typically not ready for instant analysis - it contains clouds,
#' cloud shadows, aerosols, as well as may cover not all the territory you of
#' your interest. Another issue is that each particular pixel slightly differs in reflectance
#' between images taken in different days due to difference in atmospheric conditions and angle
#' of sunlight at the moments images were taken. Google Earth Engine has its own build-in
#' algorithms for image pre-processing, atmospheric corrections and mosaicing, which allows to
#' obtain a ready-to-use, rectified image. Approach used in this script is that to find a median
#' value for each pixel between several images within each of 10 optical band, and thereby make a
#' composite image. To define a set of imageries between which we are going to calculate median,
#' we should set a timespan defining starting and final days. Sentinel-2 apparatus takes picture
#' once a 5 days, so if you set up month-long timesnap, you can expect that each pixel value will
#' be calculated based on 5 to 6 values.
#'
#' Note 2.
#' You may set up any image resolution (pixel size) for satellite imagery with GEE, but this is
#' hardly to be reasonable setting the finer resolution than the finest for satellite source.
#' Finest resolution for Sentinel data - 10 m, while using larger scale values requires less
#' computational resources and returns smaller resulting dataframe. Although sampling
#' satellite data performs in a cloud, there are some memory limitations placed
#' by GEE itself. If you are about to sample really large areas, consider setting higher 'scale'
#' value (100, 1000).
#' More about GEE best practices: https://developers.google.com/earth-engine/guides/best_practices
#'
#' @export
#'
#' @examples
#' reflectance <-  get.pixel.data(
#'   sf_data = sf_df,
#'   startday = "2019-05-15",
#'   endday = "2019-06-30",
#'   cloud_threshold = 10,
#'   scale_value = 100)
#'
#' head(reflectance)
get.pixel.data <- function(sf_data, startday, endday, cloud_threshold, scale_value){
  ee_Initialize()
  ee_df <-  sf_as_ee(sf_data) # convert sf to ee featureCollection object

  # create an envelope region of interest to filter image collection
  region <- ee_df$geometry()$bounds()

  # cloud mask function for Sentinel-2
  # from https://github.com/ricds/DL_RS_GEE/blob/main/rgee_data_acquisition.R
  maskS2clouds <-  function(image) {
    qa = image$select('QA60');

    # Bits 10 and 11 are clouds and cirrus, respectively.
    cloudBitMask = bitwShiftL(1,10)
    cirrusBitMask = bitwShiftL(1, 11)

    # Both flags should be set to zero, indicating clear conditions.
    mask_data = qa$bitwiseAnd(cloudBitMask)$eq(0)$And(qa$bitwiseAnd(cirrusBitMask)$eq(0));

    return(image$updateMask(mask_data)$divide(10000))
  }

  # Make median multi-band image from Sentinel L2A image collection for given
  # date range and region

  sentinel2A <-  ee$ImageCollection("COPERNICUS/S2_SR")$
    filterDate(startday, endday)$
    filterBounds(region)$
    filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', cloud_threshold))$
    map(maskS2clouds)$
    select(c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12"))$
    median()

  # This property of the table stores the land cover labels.
  label <- "class"

  # Overlay the polygons (or any other vector features) on the imagery to get training.
  training <- sentinel2A$sampleRegions(
    collection = ee_df,
    properties = list(label),
    scale = scale_value
  )

  # rgee uses three different approach to upload and download data from and to the server. For small
  # dataset () default value "getInfo" is recommended, while for large vector objects / outputs
  # using intermediate container (Google Drive or Google Cloud Storage) is required. We tested
  # Google Drive approach and it showed good performance with downloading of ca. 90K pixel values.
  # function "get.pixel.data" estimates the size of input data / GEE object to be downloaded, and then
  # pick more appropriate method on their own, based on both total area of vector polygons and user defined scale
  # value (pixel size).

  if(as.numeric(sum(st_area(sf_data) / scale_value^2)) < 15000){
    # Convert training to the sf object directly
    values <-  ee_as_sf(training,
                        maxFeatures = 10000000000)
  } else {
    # Initialize Google Earth Engine API for using Google Drive as a container
    ee_Initialize(user = 'ndef', drive = TRUE)
    # Convert training to the sf object (with saving via google drive)
    values <- ee_as_sf(training,
                       overwrite = TRUE,
                       via = "drive",
                       container = "rgee_backup",
                       crs = NULL,
                       maxFeatures = 10000000000,
                       selectors = NULL,
                       lazy = FALSE,
                       public = TRUE,
                       add_metadata = TRUE,
                       timePrefix = TRUE,
                       quiet = FALSE
    )
  }

  # make a list of label values (types of surface) and its numerical IDs
  classes_cheatsheet <- as.data.frame(levels(factor(sf_df$label)))
  classes_cheatsheet$class <- rownames(as.data.frame(levels(factor(sf_df$label))))
  colnames(classes_cheatsheet) <- c("label", "class")
  classes_cheatsheet <-  classes_cheatsheet %>%
    mutate(across(label, as.factor)) %>%
    mutate(across(class, as.numeric))

  # Get final dataframe with class labels and pixel values
  reflectance <-  values %>%
    left_join(classes_cheatsheet, by="class") %>%
    st_drop_geometry() %>%
    select(-class)
}
