
# TODO: Add Landsat indices

# TODO: Add MODIS indices

# TODO: Adapt functions to work with general band names: red, green, blue, ... etc

# TODO: Add Sentinel-2 TCT indices


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


calc_NDVI <- function(im) {
  out <- ee$Image$normalizedDifference(im, c('NIR', 'Red'))
  out <- out$set("system:time_start", im$get("system:time_start"))
  out <- out$rename('NDVI')
  out <- out$copyProperties(im)
  return(out)
}


calc_NBR <- function(im) {
  out <- ee$Image$normalizedDifference(im, c('NIR', 'SWIR2'))
  out <- out$set("system:time_start", im$get("system:time_start"))
  out <- out$rename('NBR')
  out <- out$copyProperties(im)
  return(out)
}

## Still specific to Sentinel-2 because of the scale parameter...??!
## TODO: Add a parameter to select the list in the expression which handles the sat code?!
calc_EVI <- function(img){
  out <- ee$Image(img$expression(
    '2.5 * ((NIR - Red) / (NIR + 6 * Red - 7.5 * Blue + 1))', 
    list(
      NIR = img$select('NIR')$toFloat()$multiply(0.0001),
      Red = img$select('Red')$toFloat()$multiply(0.0001),
      Blue = img$select('Blue')$toFloat()$multiply(0.0001)
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("EVI")
  out <- out$copyProperties(img)
  return(out)
}

