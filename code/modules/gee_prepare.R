
## ----------------------------------------------------------------------------------- ##
## SENTINEL-2 ----
## ----------------------------------------------------------------------------------- ##


# Works with L1 and L2!!
maskClouds_S2 <- function(img){
  
  # Select quality layer
  qa <- img$select("QA60")
  
  # Bits 10 and 11 are clouds and cirrus, respectively.
  cloudBitMask <- ee$Number(1 %<<% 10)  # 1 << 10
  cirrusBitMask <- ee$Number(1 %<<% 11) # 1 << 11
  
  # Both flags should be set to zero, indicating clear conditions.
  mask <- ((qa$bitwiseAnd(cloudBitMask))$eq(0))$And(qa$bitwiseAnd(cirrusBitMask)$eq(0))
  
  return(ee$Image(img$updateMask(mask)))
}

scaleData_S2 <- function(img){
  return(ee$Image(img$multiply(0.0001)))
}


## ----------------------------------------------------------------------------------- ##
## LANDSAT MISSIONS ----
## ----------------------------------------------------------------------------------- ##


# Works with L1 and L2!!
maskClouds_LT5 <- function(img){
  
  # Select quality layer
  qa <- img$select("QA_PIXEL")
  
  #cirrusBitMask      <- ee$Number(1 %<<% 2) # 1 << 2
  cloudBitMask       <- ee$Number(1 %<<% 3)  # 1 << 3
  cloudShadowBitMask <- ee$Number(1 %<<% 4) # 1 << 4
  
  #mask_cir <- qa$bitwiseAnd(cirrusBitMask)$eq(0)
  mask_clo <- qa$bitwiseAnd(cloudBitMask)$eq(0)
  mask_sha <- qa$bitwiseAnd(cloudShadowBitMask)$eq(0)
  
  return(img$updateMask(mask_clo$And(mask_sha)))
}

maskClouds_LT7 <- function(img){
  
  # Select quality layer
  qa <- img$select("QA_PIXEL")
  
  #cirrusBitMask      <- ee$Number(1 %<<% 2) # 1 << 2
  cloudBitMask       <- ee$Number(1 %<<% 3)  # 1 << 3
  cloudShadowBitMask <- ee$Number(1 %<<% 4) # 1 << 4
  
  #mask_cir <- qa$bitwiseAnd(cirrusBitMask)$eq(0)
  mask_clo <- qa$bitwiseAnd(cloudBitMask)$eq(0)
  mask_sha <- qa$bitwiseAnd(cloudShadowBitMask)$eq(0)
  
  return(img$updateMask(mask_clo$And(mask_sha)))
}

# Works with L1 and L2!!
maskClouds_LT8 <- function(img){
  
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

maskClouds_LT9 <- function(img){
  
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

scaleData_LT_SR <- function(img){
spt_scale_lt_tm_sr <- function(img){
  
  opticalBands = img$select(c("Blue", "Green", 
                              "Red", "NIR", "SWIR1", "SWIR2"))$multiply(0.0000275)$add(-0.2)
  
  return(img$addBands(opticalBands, NULL, TRUE));
}

spt_scale_lt_oli_sr <- function(img){
  
  opticalBands = img$select(c("Coastal","Blue", "Green", 
                              "Red", "NIR", "SWIR1", "SWIR2"))$multiply(0.0000275)$add(-0.2)
  
  return(img$addBands(opticalBands, NULL, TRUE));
}

scaleData_LT_TOA <- function(img){
  return(ee$Image(img$multiply(1)))
}

spt_lt7_interpolate <- function(img){
  return(ee$Image$focal_mean(img, 1, 'square', 'pixels', 8))
}

## ----------------------------------------------------------------------------------- ##
## MODIS/Terra/Aqua ----
## ----------------------------------------------------------------------------------- ##


bitwiseExtract <- function(value, fromBit, toBit = NULL) {
  
  if (is.null(toBit)) {
    toBit <- fromBit
  }
  
  maskSize <- ee$Number(1)$add(toBit)$subtract(fromBit)
  mask <- ee$Number(1)$leftShift(maskSize)$subtract(1)
  
  return(value$rightShift(fromBit)$bitwiseAnd(mask))
}

maskClouds_MOD09A1 <- function(image) {
  
  qa <- image$select("StateQA")
  
  cloudState <- bitwiseExtract(qa, 0, 1)
  cloudShadowState <- bitwiseExtract(qa, 2)
  cirrusState <- bitwiseExtract(qa, 8, 9)
  
  cloud  <- cloudState$eq(0)
  shadow <- cloudShadowState$eq(0)
  cirrus <- cirrusState$eq(0)
  
  maskedImage <- image$updateMask(cloud$And(shadow)$And(cirrus))
  
  return(maskedImage)
}

maskClouds_MOD13Q1 <- function(image) {
  
  qa <- image$select("SummaryQA")
  
  goodQualityVI <- bitwiseExtract(qa, 0, 1)
  mask  <- goodQualityVI$eq(0)
  
  maskedImage <- image$updateMask(mask)
  
  return(maskedImage)
}

scaleData_MODIS <- function(img){
  return(ee$Image(img$multiply(0.0001)))
}


