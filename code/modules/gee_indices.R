

## ----------------------------------------------------------------------------------- ##
## ALL SATELLITE MISSIONS ----
## ----------------------------------------------------------------------------------- ##


calc_NDVI <- function(img) {
  out <- ee$Image$normalizedDifference(img, c('NIR', 'Red'))
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename('NDVI')
  out <- out$copyProperties(img)
  return(out)
}


calc_NBR <- function(img) {
  out <- ee$Image$normalizedDifference(img, c('NIR', 'SWIR2'))
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename('NBR')
  out <- out$copyProperties(img)
  return(out)
}


calc_EVI <- function(img){
  out <- ee$Image(img$expression(
    '2.5 * ((NIR - Red) / (NIR + 6 * Red - 7.5 * Blue + 1))', 
    list(
      NIR = img$select('NIR'),
      Red = img$select('Red'),
      Blue = img$select('Blue')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("EVI")
  out <- out$copyProperties(img)
  return(out)
}

# Sicong Liu , Yongjie Zheng , Michele Dalponte & Xiaohua Tong (2020) A
# novel fire index-based burned area change detection approach using Landsat-8 OLI data,
# European Journal of Remote Sensing, 53:1, 104-112, DOI: 10.1080/22797254.2020.1738900

calc_NBRSWIR <- function(img){
  
  out <- ee$Image(img$expression(
    '(SWIR2 - SWIR1 - 0.02) / (SWIR2 + SWIR1 + 0.1)', 
    list(
      'SWIR1' = img$select('SWIR1'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("NBRSWIR")
  out <- out$copyProperties(img)
  return(out)
}

# Trigg, S., & Flasse, S. (2001). An evaluation of different
# bi-spectral spaces for discriminating burnedshrub-savannah. International Journal 
# of RemoteSensing, 22(13), 2641-2647. https://doi.org/10.1080/01431160110053185

calc_MIRBI <- function(img){
  
  out <- ee$Image(img$expression(
    '10 * SWIR2 - 9.8 * SWIR1 + 2', 
    list(
      'SWIR1' = img$select('SWIR1'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("MIRBI")
  out <- out$copyProperties(img)
  return(out)
}

# Char Soil Index
# Smith et al. 2007

calc_CSI <- function(img){
  
  out <- ee$Image(img$expression(
    'NIR / SWIR', 
    list(
      'NIR' = img$select('NIR'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("CSI")
  out <- out$copyProperties(img)
  return(out)
}


## ----------------------------------------------------------------------------------- ##
## SENTINEL-2 ----
## ----------------------------------------------------------------------------------- ##


calc_NBRP_S2 <- function(img){
  
  out <- ee$Image(img$expression(
    # (B12 - B8A - B3 - B2) / (B12 + B8A + B3 + B2)
    '(SWIR2 - RE4 - GREEN - BLUE) / (SWIR2 + RE4 + GREEN + BLUE)',
    list(
      'SWIR2' = img$select('SWIR2'),  # B12
      'RE4'   = img$select('RE4'),    # B8A
      'BLUE'  = img$select('Blue'),   # B2
      'GREEN' = img$select('Green')  # B3
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("NBRP")
  out <- out$copyProperties(img)
  return(out)
}


calc_TCTB_S2 <- function(img){
  
  out <- ee$Image(img$expression(
    '(BLUE * 0.351) + (GREEN * 0.3813) + (RED * 0.3437) + (NIR * 0.7196) + (SWIR1 * 0.2396) + (SWIR2 * 0.1949)', 
    list(
      'BLUE'  = img$select('Blue'),
      'GREEN' = img$select('Green'),
      'RED'   = img$select('Red'),
      'NIR'   = img$select('NIR'),
      'SWIR1' = img$select('SWIR1'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("TCTB")
  out <- out$copyProperties(img)
  return(out)
}

calc_TCTG_S2 <- function(img){
  
  out <- ee$Image(img$expression(
    '(BLUE * -0.3599) + (GREEN * -0.3533) + (RED * -0.4734) + (NIR * 0.6633) + (SWIR1 * 0.0087) + (SWIR2 * -0.2856)', 
    list(
      'BLUE'  = img$select('Blue'),
      'GREEN' = img$select('Green'),
      'RED'   = img$select('Red'),
      'NIR'   = img$select('NIR'),
      'SWIR1' = img$select('SWIR1'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("TCTG")
  out <- out$copyProperties(img)
  return(out)
}

calc_TCTW_S2 <- function(img){
  
  out <- ee$Image(img$expression(
    '(BLUE * 0.2578) + (GREEN * 0.2305) + (RED * 0.0883) + (NIR * 0.1071) + (SWIR1 * -0.7611) + (SWIR2 * -0.5308)', 
    list(
      'BLUE'  = img$select('Blue'),
      'GREEN' = img$select('Green'),
      'RED'   = img$select('Red'),
      'NIR'   = img$select('NIR'),
      'SWIR1' = img$select('SWIR1'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("TCTW")
  out <- out$copyProperties(img)
  return(out)
}

## ----------------------------------------------------------------------------------- ##
## LANDSAT MISSIONS ----
## ----------------------------------------------------------------------------------- ##

## Landsat-5 ----

calc_TCTB_L5 <- function(img){
  
  out <- ee$Image(img$expression(
    '(BLUE * 0.2043) + (GREEN * 0.4158) + (RED * 0.5524) + (NIR * 0.5741) + (SWIR1 * 0.3124) + (SWIR2 * 0.2303)',
    list(
      'BLUE'  = img$select('Blue'),
      'GREEN' = img$select('Green'),
      'RED'   = img$select('Red'),
      'NIR'   = img$select('NIR'),
      'SWIR1' = img$select('SWIR1'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("TCTB")
  out <- out$copyProperties(img)
  return(out)
}

calc_TCTG_L5 <- function(img){
  
  out <- ee$Image(img$expression(
    '(BLUE * -0.1603) + (GREEN * -0.2819) + (RED * -0.4934) + (NIR * 0.7940) + (SWIR1 * -0.0002) + (SWIR2 * -0.1446)',
    list(
      'BLUE'  = img$select('Blue'),
      'GREEN' = img$select('Green'),
      'RED'   = img$select('Red'),
      'NIR'   = img$select('NIR'),
      'SWIR1' = img$select('SWIR1'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("TCTG")
  out <- out$copyProperties(img)
  return(out)
}

calc_TCTW_L5 <- function(img){
  
  out <- ee$Image(img$expression(
    '(BLUE * 0.0315) + (GREEN * 0.2021) + (RED * 0.3102) + (NIR * 0.1594) + (SWIR1 * -0.6806) + (SWIR2 * -0.6109)',
    list(
      'BLUE'  = img$select('Blue'),
      'GREEN' = img$select('Green'),
      'RED'   = img$select('Red'),
      'NIR'   = img$select('NIR'),
      'SWIR1' = img$select('SWIR1'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("TCTW")
  out <- out$copyProperties(img)
  return(out)
}


## Landsat-7 ----

calc_TCTB_L7 <- function(img){
  
  out <- ee$Image(img$expression(
    '(BLUE * 0.3561) + (GREEN * 0.3972) + (RED * 0.3904) + (NIR * 0.6966) + (SWIR1 * 0.2286) + (SWIR2 * 0.1596)', 
      list(
      'BLUE'  = img$select('Blue'),
      'GREEN' = img$select('Green'),
      'RED'   = img$select('Red'),
      'NIR'   = img$select('NIR'),
      'SWIR1' = img$select('SWIR1'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("TCTB")
  out <- out$copyProperties(img)
  return(out)
}

calc_TCTG_L7 <- function(img){
  
  out <- ee$Image(img$expression(
    '(BLUE * -0.3344) + (GREEN * -0.3544) + (RED * -0.4556) + (NIR * 0.6966) + (SWIR1 * -0.0242) + (SWIR2 * -0.2630)',
      list(
      'BLUE'  = img$select('Blue'),
      'GREEN' = img$select('Green'),
      'RED'   = img$select('Red'),
      'NIR'   = img$select('NIR'),
      'SWIR1' = img$select('SWIR1'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("TCTG")
  out <- out$copyProperties(img)
  return(out)
}

calc_TCTW_L7 <- function(img){
  
  out <- ee$Image(img$expression(
    '(BLUE * 0.2626) + (GREEN * 0.2141) + (RED * 0.0926) + (NIR * 0.0656) + (SWIR1 * -0.7629) + (SWIR2 * -0.5388)',
      list(
      'BLUE'  = img$select('Blue'),
      'GREEN' = img$select('Green'),
      'RED'   = img$select('Red'),
      'NIR'   = img$select('NIR'),
      'SWIR1' = img$select('SWIR1'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("TCTW")
  out <- out$copyProperties(img)
  return(out)
}


## Landsat-8/9 ----


calc_TCTB_L8 <- function(img){
  
  out <- ee$Image(img$expression(
    '(BLUE * 0.3029) + (GREEN * 0.2786) + (RED * 0.4733) + (NIR * 0.5599) + (SWIR1 * 0.508) + (SWIR2 * 0.1872)',
    
    list(
      'BLUE'  = img$select('Blue'),
      'GREEN' = img$select('Green'),
      'RED'   = img$select('Red'),
      'NIR'   = img$select('NIR'),
      'SWIR1' = img$select('SWIR1'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("TCTB")
  out <- out$copyProperties(img)
  return(out)
}

calc_TCTG_L8 <- function(img){
  
  out <- ee$Image(img$expression(
    '(BLUE * -0.2941) + (GREEN * -0.243) + (RED * -0.5424) + (NIR * 0.7276) + (SWIR1 * 0.0713) + (SWIR2 * -0.1608)',
    
    list(
      'BLUE'  = img$select('Blue'),
      'GREEN' = img$select('Green'),
      'RED'   = img$select('Red'),
      'NIR'   = img$select('NIR'),
      'SWIR1' = img$select('SWIR1'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("TCTG")
  out <- out$copyProperties(img)
  return(out)
}

calc_TCTW_L8 <- function(img){
  
  out <- ee$Image(img$expression(
    '(BLUE * 0.1511) + (GREEN * 0.1973) + (RED * 0.3283) + (NIR * 0.3407) + (SWIR1 * -0.7117) + (SWIR2 * -0.4559)',
    
    list(
      'BLUE'  = img$select('Blue'),
      'GREEN' = img$select('Green'),
      'RED'   = img$select('Red'),
      'NIR'   = img$select('NIR'),
      'SWIR1' = img$select('SWIR1'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("TCTW")
  out <- out$copyProperties(img)
  return(out)
}


## ----------------------------------------------------------------------------------- ##
## MODIS/Terra/Aqua ----
## ----------------------------------------------------------------------------------- ##

## MOD09A1 ----

calc_TCTB_MOD09A1 <- function(img){
  
  out <- ee$Image(img$expression(
    '(0.4395 * RED) + (0.5945 * NIR) + (0.2460 * BLUE) + (0.3918 * GREEN) + (0.3506 * NIR2) + (0.2136 * SWIR1) + (0.2678 * SWIR2)',
    list(
      'BLUE'  = img$select('Blue'),
      'GREEN' = img$select('Green'),
      'RED'   = img$select('Red'),
      'NIR'   = img$select('NIR'),
      'NIR2'  = img$select('NIR2'),
      'SWIR1' = img$select('SWIR1'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("TCTB")
  out <- out$copyProperties(img)
  return(out)
}

calc_TCTG_MOD09A1 <- function(img){
  
  out <- ee$Image(img$expression(
    '(-0.4064 * RED) + (0.5129 * NIR) + (-0.2744 * BLUE) + (-0.2893 * GREEN) + (0.4882 * NIR2) + (-0.0036 * SWIR1) + (-0.4169 * SWIR2)',
    list(
      'BLUE'  = img$select('Blue'),
      'GREEN' = img$select('Green'),
      'RED'   = img$select('Red'),
      'NIR'   = img$select('NIR'),
      'NIR2'  = img$select('NIR2'),
      'SWIR1' = img$select('SWIR1'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("TCTG")
  out <- out$copyProperties(img)
  return(out)
}

calc_TCTW_MOD09A1 <- function(img){
  
  out <- ee$Image(img$expression(
    '(0.1147 * RED) + (0.2489 * NIR) + (0.2408 * BLUE) + (0.3132 * GREEN) + (-0.3122 * NIR2) + (-0.6416 * SWIR1) + (-0.5087 * SWIR2) ',
    list(
      'BLUE'  = img$select('Blue'),
      'GREEN' = img$select('Green'),
      'RED'   = img$select('Red'),
      'NIR'   = img$select('NIR'),
      'NIR2'  = img$select('NIR2'),
      'SWIR1' = img$select('SWIR1'),
      'SWIR2' = img$select('SWIR2')
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("TCTW")
  out <- out$copyProperties(img)
  return(out)
}


