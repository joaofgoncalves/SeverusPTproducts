
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
  
  return(img$updateMask( mask_clo$And(mask_sha) ))
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
  
  return(img$updateMask( mask_clo$And(mask_sha) ))
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
  
  return(img$updateMask( mask_cir$And(mask_clo)$And(mask_sha) ))
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
  
  return(img$updateMask( mask_cir$And(mask_clo)$And(mask_sha) ))
}

scaleData_LT_SR <- function(img){
  
  opticalBands = img$select('SR_B.')$multiply(0.0000275)$add(-0.2)
  return(img$addBands(opticalBands, null, true));
}

scaleData_LT_TOA <- function(img){
  return(ee$Image(img$multiply(1)))
}

## ----------------------------------------------------------------------------------- ##
## MODIS/Terra/Aqua ----
## ----------------------------------------------------------------------------------- ##






