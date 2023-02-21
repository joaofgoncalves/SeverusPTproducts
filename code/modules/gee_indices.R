


## ----------------------------------------------------------------------------------- ##
## SENTINEL-2 FUNCTIONS ----
## ----------------------------------------------------------------------------------- ##


maskS2clouds <- function(image){
  
  # Select quality layer
  qa <- image$select("QA60")
  
  # Bits 10 and 11 are clouds and cirrus, respectively.
  cloudBitMask <- ee$Number(1 %<<% 10)  # 1 << 10
  cirrusBitMask <- ee$Number(1 %<<% 11) # 1 << 11
  
  # Both flags should be set to zero, indicating clear conditions.
  mask <- ((qa$bitwiseAnd(cloudBitMask))$eq(0))$And(qa$bitwiseAnd(cirrusBitMask)$eq(0))
  
  return(image$updateMask(mask))
}


calcS2_NDVI <- function(im) {
  out <- ee$Image$normalizedDifference(im, c('B8', 'B4'))
  out <- out$set("system:time_start", im$get("system:time_start"))
  out <- out$rename('NDVI')
  out <- out$copyProperties(im)
  return(out)
}


calcS2_NBR <- function(im) {
  out <- ee$Image$normalizedDifference(im, c('B8', 'B12'))
  out <- out$set("system:time_start", im$get("system:time_start"))
  out <- out$rename('NBR')
  out <- out$copyProperties(im)
  return(out)
}


calcS2_EVI <- function(img){
  out <- ee$Image(img$expression(
    '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))', 
    list(
      NIR = img$select('B8')$toFloat()$multiply(0.0001),
      RED = img$select('B4')$toFloat()$multiply(0.0001),
      BLUE = img$select('B2')$toFloat()$multiply(0.0001)
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("EVI")
  out <- out$copyProperties(img)
  return(out)
}

