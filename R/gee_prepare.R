
## ----------------------------------------------------------------------------------- ##
## SENTINEL-2 ----
## ----------------------------------------------------------------------------------- ##

#' Masks clouds and cirrus from Sentinel-2 imagery
#'
#' This function applies a cloud and cirrus mask to Sentinel-2 imagery based on the quality 
#' assessment (QA) layer. It identifies cloud and cirrus pixels using specific bit masks and 
#' creates a mask to remove those pixels from the image.
#'
#' @param img An Earth Engine image object representing Sentinel-2 imagery.
#' 
#' @return An Earth Engine image object with clouds and cirrus masked.
#' 
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Selects the quality assessment (QA) layer from the input image.
#'   \item Defines bit masks for cloud and cirrus (bits 10 and 11).
#'   \item Creates a mask by checking that both cloud and cirrus flags are set to zero, 
#'   indicating clear conditions.
#'   \item Applies the mask to the input image using the \code{updateMask} function.
#' }
#' 
#' @importFrom ee Image select bitwiseAnd eq And updateMask
#' @export
#' 

# Works with L1 and L2!!
spt_mask_clouds_s2 <- function(img){
  
  # Select quality layer
  qa <- img$select("QA60")
  
  # Bits 10 and 11 are clouds and cirrus, respectively.
  cloudBitMask <- ee$Number(1 %<<% 10)  # 1 << 10
  cirrusBitMask <- ee$Number(1 %<<% 11) # 1 << 11
  
  # Both flags should be set to zero, indicating clear conditions.
  mask <- ((qa$bitwiseAnd(cloudBitMask))$eq(0))$And(qa$bitwiseAnd(cirrusBitMask)$eq(0))
  
  return(ee$Image(img$updateMask(mask)))
}


#' Scales Sentinel-2 imagery data
#'
#' This function scales the pixel values of Sentinel-2 imagery by multiplying them 
#' with a scaling factor of 0.0001. It is commonly used to convert the data from integer 
#' format to floating-point format.
#'
#' @param img An Earth Engine image object representing Sentinel-2 imagery.
#' 
#' @return An Earth Engine image object with scaled pixel values.
#' 
#' @details
#' The function performs the following step:
#' \enumerate{
#'   \item Multiplies the pixel values of the input image by a scaling factor of 0.0001 using 
#'   the \code{multiply} function.
#' }
#' 
#' @importFrom ee Image multiply
#' @export
#'

scaleData_S2 <- function(img){
  return(ee$Image(img$multiply(0.0001)))
}


## ----------------------------------------------------------------------------------- ##
## LANDSAT MISSIONS ----
## ----------------------------------------------------------------------------------- ##


#' Masks clouds in Landsat 5 imagery
#'
#' This function applies a cloud mask to Landsat 5 imagery based on the quality 
#' assessment (QA) layer. Clouds and cloud shadows are identified using specific bit 
#' masks and are then masked out in the image.
#'
#' @param img An Earth Engine image object representing Landsat 5 imagery.
#' 
#' @return An Earth Engine image object with clouds and cloud shadows masked out.
#' 
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Selects the quality assessment (QA) layer from the input image using 
#'   the \code{select} function.
#'   \item Defines the cloud and cloud shadow bit masks using the \code{ee$Number} 
#'   function and bitwise left shift operator (\code{\%<<\%}).
#'   \item Applies the cloud and cloud shadow masks by performing bitwise operations 
#'   (\code{bitwiseAnd}) on the QA layer and checking if the corresponding bits are 
#'   set to zero.
#'   \item Updates the image mask using the \code{updateMask} function, where pixels 
#'   identified as clouds or cloud shadows are masked out.
#' }
#' 
#' @importFrom ee Image select Number bitwiseAnd updateMask
#' @export
#' 

# Works with L1 and L2!!
spt_mask_clouds_lt5 <- function(img){
  
  # Select quality layer
  qa <- img$select("QA_PIXEL")
  
  cloudBitMask       <- ee$Number(1 %<<% 3)  # 1 << 3
  cloudShadowBitMask <- ee$Number(1 %<<% 4) # 1 << 4
  
  #mask_cir <- qa$bitwiseAnd(cirrusBitMask)$eq(0)
  mask_clo <- qa$bitwiseAnd(cloudBitMask)$eq(0)
  mask_sha <- qa$bitwiseAnd(cloudShadowBitMask)$eq(0)
  
  return(img$updateMask(mask_clo$And(mask_sha)))
}


#' Masks clouds in Landsat-7 imagery
#'
#' This function applies a cloud mask to Landsat 7 imagery based on the quality 
#' assessment (QA) layer. Clouds and cloud shadows are identified using specific 
#' bit masks and are then masked out in the image.
#'
#' @param img An Earth Engine image object representing Landsat 7 imagery.
#' 
#' @return An Earth Engine image object with clouds and cloud shadows masked out.
#' 
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Selects the quality assessment (QA) layer from the input image using 
#'   the \code{select} function.
#'   \item Defines the cloud and cloud shadow bit masks using the \code{ee$Number} 
#'   function and bitwise left shift operator (\code{\%<<\%}).
#'   \item Applies the cloud and cloud shadow masks by performing bitwise operations 
#'   (\code{bitwiseAnd}) on the QA layer and checking if the corresponding bits are 
#'   set to zero.
#'   \item Updates the image mask using the \code{updateMask} function, where pixels 
#'   identified as clouds or cloud shadows are masked out.
#' }
#' 
#' @importFrom ee Image select Number bitwiseAnd updateMask
#' @export
#' 

spt_mask_clouds_lt7 <- function(img){
  
  # Select quality layer
  qa <- img$select("QA_PIXEL")

  cloudBitMask       <- ee$Number(1 %<<% 3)  # 1 << 3
  cloudShadowBitMask <- ee$Number(1 %<<% 4) # 1 << 4
  
  #mask_cir <- qa$bitwiseAnd(cirrusBitMask)$eq(0)
  mask_clo <- qa$bitwiseAnd(cloudBitMask)$eq(0)
  mask_sha <- qa$bitwiseAnd(cloudShadowBitMask)$eq(0)
  
  return(img$updateMask(mask_clo$And(mask_sha)))
}


#' Masks clouds in Landsat 8 imagery
#'
#' This function applies a cloud mask to Landsat 8 imagery based on the quality 
#' assessment (QA) layer. Clouds, cloud shadows, and cirrus clouds are identified 
#' using specific bit masks and are then masked out in the image.
#'
#' @param img An Earth Engine image object representing Landsat 8 imagery.
#' 
#' @return An Earth Engine image object with clouds, cloud shadows, and cirrus 
#' clouds masked out.
#' 
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Selects the quality assessment (QA) layer from the input image using 
#'   the \code{select} function.
#'   \item Defines the bit masks for cirrus clouds, clouds, and cloud shadows 
#'   using the \code{ee$Number} function and bitwise left shift operator (\code{\%<<\%}).
#'   \item Applies the masks for cirrus clouds, clouds, and cloud shadows by 
#'   performing bitwise operations (\code{bitwiseAnd}) on the QA layer and 
#'   checking if the corresponding bits are set to zero.
#'   \item Updates the image mask using the \code{updateMask} function, where 
#'   pixels identified as cirrus clouds, clouds, or cloud shadows are masked out.
#' }
#' 
#' @importFrom ee Image select Number bitwiseAnd updateMask
#' @export
#' 

# Works with L1 and L2!!
spt_mask_clouds_lt8 <- function(img){
  
  # Select quality layer
  qa <- img$select("QA_PIXEL")
  
  cirrusBitMask      <- ee$Number(1 %<<% 2) # 1 << 2
  cloudBitMask       <- ee$Number(1 %<<% 3)  # 1 << 3
  cloudShadowBitMask <- ee$Number(1 %<<% 4) # 1 << 4
  
  mask_cir <- qa$bitwiseAnd(cirrusBitMask)$eq(0)
  mask_clo <- qa$bitwiseAnd(cloudBitMask)$eq(0)
  mask_sha <- qa$bitwiseAnd(cloudShadowBitMask)$eq(0)
  
  return(img$updateMask(mask_cir$And(mask_clo)$And(mask_sha)))
}


#' Masks clouds in Landsat 9 imagery
#'
#' This function applies a cloud mask to Landsat 9 imagery based on the quality 
#' assessment (QA) layer. Clouds, cloud shadows, and cirrus clouds are identified 
#' using specific bit masks and are then masked out in the image.
#'
#' @param img An Earth Engine image object representing Landsat 9 imagery.
#' 
#' @return An Earth Engine image object with clouds, cloud shadows, and cirrus 
#' clouds masked out.
#' 
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Selects the quality assessment (QA) layer from the input image using 
#'   the \code{select} function.
#'   \item Defines the bit masks for cirrus clouds, clouds, and cloud shadows using 
#'   the \code{ee$Number} function and bitwise left shift operator (\code{\%<<\%}).
#'   \item Applies the masks for cirrus clouds, clouds, and cloud shadows by performing 
#'   bitwise operations (\code{bitwiseAnd}) on the QA layer and checking if the 
#'   corresponding bits are set to zero.
#'   \item Updates the image mask using the \code{updateMask} function, where pixels 
#'   identified as cirrus clouds, clouds, or cloud shadows are masked out.
#' }
#' 
#' @importFrom ee Image select Number bitwiseAnd updateMask
#' @export
#' 

spt_mask_clouds_lt9 <- function(img){
  
  # Select quality layer
  qa <- img$select("QA_PIXEL")
  
  cirrusBitMask      <- ee$Number(1 %<<% 2) # 1 << 2
  cloudBitMask       <- ee$Number(1 %<<% 3)  # 1 << 3
  cloudShadowBitMask <- ee$Number(1 %<<% 4) # 1 << 4
  
  mask_cir <- qa$bitwiseAnd(cirrusBitMask)$eq(0)
  mask_clo <- qa$bitwiseAnd(cloudBitMask)$eq(0)
  mask_sha <- qa$bitwiseAnd(cloudShadowBitMask)$eq(0)
  
  return(img$updateMask(mask_cir$And(mask_clo)$And(mask_sha)))
}


#' Scales Landsat TM surface reflectance imagery in GEE
#'
#' This function scales the surface reflectance bands of Landsat TM imagery to their 
#' corresponding surface reflectance values using pre-defined scaling factors and 
#' offsets. It applies the scaling to the optical bands (Blue, Green, Red, NIR, SWIR1, SWIR2) 
#' and adds the scaled bands as new bands to the input image.
#'
#' @param img An Earth Engine image object representing Landsat TM surface reflectance imagery.
#' 
#' @return An Earth Engine image object with the optical bands scaled to surface reflectance values.
#' 
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Selects the optical bands (Blue, Green, Red, NIR, SWIR1, SWIR2) from the input 
#'   image using the \code{select} function.
#'   \item Scales the selected bands by multiplying them with a scaling factor (0.0000275) 
#'   and subtracting an offset (-0.2) using the \code{multiply} and \code{add} functions.
#'   \item Adds the scaled bands as new bands to the input image using the \code{addBands} 
#'   function.
#' }
#' 
#' @importFrom ee Image select multiply addBands
#' @export
#' 

spt_scale_lt_tm_sr <- function(img){
  
  opticalBands = img$select(c("Blue", "Green", 
                              "Red", "NIR", "SWIR1", "SWIR2"))$multiply(0.0000275)$add(-0.2)
  
  return(img$addBands(opticalBands, NULL, TRUE));
}


#' Scales Landsat OLI surface reflectance imagery
#'
#' This function scales the surface reflectance bands of Landsat OLI imagery to 
#' their corresponding surface reflectance values using pre-defined scaling factors 
#' and offsets. It applies the scaling to the optical bands (Coastal, Blue, Green, Red, 
#' NIR, SWIR1, SWIR2) and adds the scaled bands as new bands to the input image.
#'
#' @param img An Earth Engine image object representing Landsat OLI surface reflectance 
#' imagery.
#' 
#' @return An Earth Engine image object with the optical bands scaled to surface 
#' reflectance values.
#' 
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Selects the optical bands (Coastal, Blue, Green, Red, NIR, SWIR1, SWIR2) from 
#'   the input image using the \code{select} function.
#'   \item Scales the selected bands by multiplying them with a scaling factor (0.0000275) 
#'   and subtracting an offset (-0.2) using the \code{multiply} and \code{add} functions.
#'   \item Adds the scaled bands as new bands to the input image using the \code{addBands} 
#'   function.
#' }
#' 
#' @importFrom ee Image select multiply addBands
#' @export
#' 

spt_scale_lt_oli_sr <- function(img){
  
  opticalBands = img$select(c("Coastal","Blue", "Green", 
                              "Red", "NIR", "SWIR1", "SWIR2"))$multiply(0.0000275)$add(-0.2)
  
  return(img$addBands(opticalBands, NULL, TRUE));
}


#' Scales Landsat top-of-atmosphere (TOA) imagery (placeholder function)
#'
#' This function maintains the top-of-atmosphere (TOA) reflectance (placeholder function because values 
#' area already scaled in this case)
#'
#' @param img An Earth Engine image object representing Landsat top-of-atmosphere (TOA) imagery.
#' 
#' @return An Earth Engine image object with the TOA reflectance values
#' 
#' @details
#' The function simply multiplies the input image by a scaling factor of 1 using the \code{multiply} function. 
#' This operation retains the original TOA values of the image.
#' 
#' @importFrom ee Image multiply
#' @export
#' 

spt_scale_lt_toa <- function(img){
  return(ee$Image(img$multiply(1)))
}


#' Interpolates missing values in Landsat 7 imagery
#'
#' This function applies a focal mean filter to the Landsat 7 imagery in order to 
#' interpolate missing values due to SLC-off issues. It calculates the average of 
#' neighboring pixels within a square kernel of size 8x8 and replaces the missing 
#' alues with the interpolated values.
#'
#' @param img An Earth Engine image object representing Landsat 7 imagery.
#' 
#' @return An Earth Engine image object with missing values interpolated using a focal mean filter.
#' 
#' @details
#' The function uses the \code{focal_mean} function to calculate the average of neighboring 
#' pixels within a square kernel of size 8x8. This kernel is applied to the input image, and 
#' the resulting values are used to replace the missing values in the image. The interpolation 
#' helps fill in the gaps caused by missing or corrupted data in the Landsat 7 imagery caused by 
#' SLC-off.
#' 
#' @importFrom ee Image focal_mean
#' @export
#' 

spt_lt7_interpolate <- function(img){
  return(ee$Image$focal_mean(img, 1, 'square', 'pixels', 8))
}

## ----------------------------------------------------------------------------------- ##
## MODIS/Terra/Aqua ----
## ----------------------------------------------------------------------------------- ##


#' Extracts bits from a numeric value using bitwise operations
#'
#' This function performs bitwise operations to extract a range of bits from a numeric value. 
#' It can extract a single bit or a range of bits specified by the start and end positions.
#'
#' @param value A numeric value from which bits are to be extracted.
#' @param fromBit The starting bit position from which extraction should begin.
#' @param toBit (optional) The ending bit position at which extraction should stop.
#'              If not provided, extraction will be performed for a single bit at 
#'              the \code{fromBit} position.
#' 
#' @return A numeric value representing the extracted bits.
#' 
#' @details
#' The function performs bitwise right shift and bitwise AND operations to extract the 
#' desired bits from the input value. If only \code{fromBit} is provided, a single bit is 
#' extracted. If both \code{fromBit} and \code{toBit} are provided, a range of bits is extracted, 
#' inclusive of both positions. The extracted bits are returned as a numeric value.
#' 
#' @examples
#' # Extract a single bit
#' value <- 10  # Binary representation: 1010
#' extracted_bit <- spt_bitwise_extract(value, fromBit = 1)
#' extracted_bit  # Output: 0
#' 
#' # Extract a range of bits
#' value <- 10  # Binary representation: 1010
#' extracted_bits <- spt_bitwise_extract(value, fromBit = 2, toBit = 3)
#' extracted_bits  # Output: 1
#' 
#' @export
#' 

spt_bitwise_extract <- function(value, fromBit, toBit = NULL) {
  
  if (is.null(toBit)) {
    toBit <- fromBit
  }
  
  maskSize <- ee$Number(1)$add(toBit)$subtract(fromBit)
  mask <- ee$Number(1)$leftShift(maskSize)$subtract(1)
  
  return(value$rightShift(fromBit)$bitwiseAnd(mask))
}


#' Masks clouds, cloud shadows, and cirrus in MOD09A1 imagery
#'
#' This function applies cloud, cloud shadow, and cirrus masks to MOD09A1 imagery 
#' based on the StateQA band values. It extracts specific bits from the StateQA band 
#' using the \code{spt_bitwise_extract} function and applies masks based on the extracted values.
#'
#' @param image An ee$Image object representing the MOD09A1 imagery.
#' 
#' @return An ee$Image object with clouds, cloud shadows, and cirrus masked out.
#' 
#' @details
#' The function applies masks to the input image using the StateQA band values. It extracts 
#' specific bits from the StateQA band using the \code{spt_bitwise_extract} function and creates 
#' masks based on the extracted values. Clouds, cloud shadows, and cirrus are masked out by setting 
#' the corresponding pixels to \code{NA} (Not Available) in the output image.
#' 
#' 
#' @export
#' 

spt_mask_clouds_mod09a1 <- function(image) {
  
  qa <- image$select("StateQA")
  
  cloudState <- spt_bitwise_extract(qa, 0, 1)
  cloudShadowState <- spt_bitwise_extract(qa, 2)
  cirrusState <- spt_bitwise_extract(qa, 8, 9)
  
  cloud  <- cloudState$eq(0)
  shadow <- cloudShadowState$eq(0)
  cirrus <- cirrusState$eq(0)
  
  maskedImage <- image$updateMask(cloud$And(shadow)$And(cirrus))
  
  return(maskedImage)
}


#' Masks clouds in MOD13Q1 vegetation index imagery
#'
#' This function applies a cloud mask to MOD13Q1 vegetation index imagery based on 
#' the SummaryQA band values.
#'
#' @param image An ee$Image object representing the MOD13Q1 vegetation index imagery.
#' 
#' @return An ee$Image object with clouds masked out.
#' 
#' @details
#' The function applies a mask to the input image using the SummaryQA band values. 
#' It extracts specific bits from the SummaryQA band using the \code{spt_bitwise_extract} 
#' function and creates a mask based on the extracted values. Cloudy pixels are masked out 
#' by setting them to \code{NA} (Not Available) in the output image.
#' 
#' @export
#' 

spt_mask_clouds_mod13q1 <- function(image) {
  
  qa <- image$select("SummaryQA")
  
  goodQualityVI <- spt_bitwise_extract(qa, 0, 1)
  mask  <- goodQualityVI$eq(0)
  
  maskedImage <- image$updateMask(mask)
  
  return(maskedImage)
}


#' Scales the values of MODIS images
#'
#' This function scales the pixel values of an image in MOD format by multiplying 
#' them with a scaling factor of 0.0001.
#'
#' @param img An `ee$Image` object representing the MOD-format image.
#'
#' @return An `ee$Image` object with scaled pixel values.
#'
#' @details
#' The function applies a scaling factor of 0.0001 to the pixel values of the input image. 
#' This scaling is commonly used to convert MOD-format pixel values to their physical 
#' measurement units.
#'
#' @export
#' 

spt_scale_mod <- function(img){
  return(ee$Image(img$multiply(0.0001)))
}


