

## ----------------------------------------------------------------------------------- ##
## ALL SATELLITE MISSIONS / GENERIC INDICES ----
## ----------------------------------------------------------------------------------- ##

#' Calculate Normalized Difference Vegetation Index (NDVI)
#'
#' Calculates the NDVI from an input image using the NIR (Near Infrared) and Red bands.
#'
#' @param img An input image containing NIR and Red bands.
#'
#' @return An image representing the NDVI.
#'
#' @details
#' This function calculates the NDVI (Normalized Difference Vegetation Index) by taking the difference
#' between the NIR and Red bands and normalizing it. The NDVI is a common index used to assess vegetation
#' health and density.
#'
#' The input image should contain the NIR and Red bands as specified in the function.
#' The output image will have a band named 'NDVI' and will inherit the properties of the input image.
#'
#' @examples
#' # Load an image with NIR and Red bands
#' # img <- ee$Image('path/to/image')
#'
#' # Calculate NDVI
#' # ndvi <- spt_calc_ndvi(img)
#'
#' @export
#' 

spt_calc_ndvi <- function(img) {
  out <- ee$Image$normalizedDifference(img, c('NIR', 'Red'))
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename('NDVI')
  out <- out$copyProperties(img)
  return(out)
}


#' Calculate Normalized Burn Ratio (NBR)
#'
#' Calculates the NBR from an input image using the NIR (Near Infrared) and SWIR2 (Shortwave Infrared 2) bands.
#'
#' @param img An input image containing NIR and SWIR2 bands.
#'
#' @return An image representing the NBR.
#'
#' @details
#' This function calculates the NBR (Normalized Burn Ratio) by taking the difference
#' between the NIR and SWIR2 bands and normalizing it. The NBR is commonly used to assess
#' burned area severity and vegetation recovery after wildfires.
#'
#' The input image should contain the NIR and SWIR2 bands as specified in the function.
#' The output image will have a band named 'NBR' and will inherit the properties of the input image.
#'
#' @examples
#' # Load an image with NIR and SWIR2 bands
#' # img <- ee$Image('path/to/image')
#'
#' # Calculate NBR
#' # nbr <- spt_calc_nbr(img)
#'
#' @export
#' 

spt_calc_nbr <- function(img) {
  out <- ee$Image$normalizedDifference(img, c('NIR', 'SWIR2'))
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename('NBR')
  out <- out$copyProperties(img)
  return(out)
}


#' Calculate Enhanced Vegetation Index (EVI)
#'
#' Calculates the EVI from an input image using the NIR (Near Infrared), Red, and Blue bands.
#'
#' @param img An input image containing NIR, Red, and Blue bands.
#'
#' @return An image representing the EVI.
#'
#' @details
#' This function calculates the Enhanced Vegetation Index (EVI) using the NIR, Red, and Blue bands.
#' The formula for EVI is: 2.5 ((NIR - Red) / (NIR + 6 Red - 7.5 Blue + 1)).
#'
#' The input image should contain the NIR, Red, and Blue bands as specified in the function.
#' The output image will have a band named 'EVI' and will inherit the properties of the input image.
#'
#' @examples
#' # Load an image with NIR, Red, and Blue bands
#' # img <- ee$Image('path/to/image')
#'
#' # Calculate EVI
#' # evi <- spt_calc_evi(img)
#'
#' @export
#' 

spt_calc_evi <- function(img){
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


#' Calculate a novel Normalized Burn Ratio (NBR_SWIR)
#'
#' Calculates the Normalized Burn Ratio (NBR_SWIR) from an input image using the SWIR1 and SWIR2 bands.
#'
#' @param img An input image containing SWIR1 and SWIR2 bands.
#'
#' @return An image representing the NBR_SWIR index.
#'
#' @details
#' This function calculates the Normalized Burn Ratio (NBR_SWIR) using the SWIR1 and SWIR2 bands.
#' The formula for NBR_SWIR is: - ((SWIR2 - SWIR1 - 0.02) / (SWIR2 + SWIR1 + 0.1)).
#'
#' The input image should contain the SWIR1 and SWIR2 bands as specified in the function.
#' The output image will have a band named 'NBRSWIR' and will inherit the properties of the input image.
#' 
#' Based on:
#' Sicong Liu , Yongjie Zheng , Michele Dalponte & Xiaohua Tong (2020) A
#' novel fire index-based burned area change detection approach using Landsat-8 OLI data,
#' European Journal of Remote Sensing, 53:1, 104-112, DOI: 10.1080/22797254.2020.1738900
#'
#' NOTE: THIS INDEX WAS ADJUSTED TO NEGATIVE FORM TO GIVE POSITIVE DELTAS HIGH SEVERITY
#' @examples
#' # Load an image with SWIR1 and SWIR2 bands
#' # img <- ee$Image('path/to/image')
#'
#' # Calculate NBR_SWIR
#' # nbr_swir <- spt_calc_nbrswir(img)
#'
#' @export
#' 


spt_calc_nbrswir <- function(img){
  
  out <- ee$Image(img$expression(
    '-1*((SWIR2 - SWIR1 - 0.02) / (SWIR2 + SWIR1 + 0.1))', 
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


#' Calculate Mid-Infrared Burn Index (MIRBI)
#'
#' Calculates the Mid-Infrared Burn Index (MIRBI) from an input image using the SWIR1 and SWIR2 bands.
#'
#' @param img An input image containing SWIR1 and SWIR2 bands.
#'
#' @return An image representing the MIRBI.
#'
#' @details
#' This function calculates the Mid-Infrared Burn Index (MIRBI) using the SWIR1 and SWIR2 bands.
#' The formula for MIRBI is: -1 * (10 * SWIR2 - 9.8 * SWIR1 + 2).
#'
#' The input image should contain the SWIR1 and SWIR2 bands as specified in the function.
#' The output image will have a band named 'MIRBI' and will inherit the properties of the input image.
#' 
#' Trigg, S., & Flasse, S. (2001). An evaluation of different
#' bi-spectral spaces for discriminating burned shrub-savannah. International Journal 
#' of RemoteSensing, 22(13), 2641-2647. https://doi.org/10.1080/01431160110053185
#' 
#' NOTE: THIS INDEX WAS ADJUSTED TO NEGATIVE FORM TO GIVE POSITIVE DELTAS HIGH SEVERITY
#'
#' @examples
#' # Load an image with SWIR1 and SWIR2 bands
#' # img <- ee$Image('path/to/image')
#'
#' # Calculate MIRBI
#' # mirbi <- spt_calc_mirbi(img)
#'
#' @export
#' 

spt_calc_mirbi <- function(img){
  
  out <- ee$Image(img$expression(
    '-1*(10 * SWIR2 - 9.8 * SWIR1 + 2)', 
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


#' Calculate Char Soil Index (CSI)
#'
#' Calculates the Char Soil Index (CSI) from an input image using the NIR and SWIR2 
#' bands.
#'
#' @param img An input image containing NIR and SWIR2 bands.
#'
#' @return An image representing the CSI.
#'
#' @details
#' This function calculates the Char Soil Index (CSI) using the NIR and SWIR2 bands.
#' The formula for CSI is: NIR / SWIR2.
#'
#' The input image should contain the NIR and SWIR2 bands as specified in the function.
#' The output image will have a band named 'CSI' and will inherit the properties of the 
#' input image.
#'
#'# Smith et al. 2007
#'
#' @examples
#' # Load an image with NIR and SWIR2 bands
#' # img <- ee$Image('path/to/image')
#'
#' # Calculate CSI
#' # csi <- spt_calc_csi(img)
#'
#' @export
#' 

spt_calc_csi <- function(img){
  
  out <- ee$Image(img$expression(
    'NIR / SWIR2', 
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

#' Calculate NBR+ (Normalized Burn Ratio Plus)
#'
#' Calculates the NBR+ (Normalized Burn Ratio Plus) from an input image using the SWIR2, NIR, Green, 
#' and Blue bands.
#'
#' @param img An input image containing SWIR2, NIR, Green, and Blue bands.
#'
#' @return An image representing the NBR+.
#'
#' @details
#' This function calculates the NBR+ (Normalized Burn Ratio Plus) using the SWIR2, NIR, Green, and 
#' Blue bands.
#' The formula for NBR+ is: -1 * ((SWIR2 - NIR - Green - Blue) / (SWIR2 + NIR + Green + Blue)).
#'
#' The input image should contain the SWIR2, NIR, Green, and Blue bands as specified in the function.
#' The output image will have a band named 'NBRP' and will inherit the properties of the input image.
#'
#' Please note that the NBR+ index was adjusted to a negative form to give positive deltas high severity.
#' The adjustment was made according to the paper by Alcaras (2022).
#'
#' @references
#' Alcaras, M. 2022. "Satellite-Based Burn Severity Mapping: Implementation of the Normalized Burn 
#' Ratio Plus (NBR+) for Sentinel-2 and Landsat-8." 
#' Remote Sensing, 14(7), 1727. DOI: 10.3390/rs14071727.
#'
#' @examples
#' # Load an image with SWIR2, NIR, Green, and Blue bands
#' # img <- ee$Image('path/to/image')
#'
#' # Calculate NBR+
#' # nbrp <- spt_calc_nbrp(img)
#'

spt_calc_nbrp <- function(img){
  
  out <- ee$Image(img$expression(
    # (B12 - B8A - B3 - B2) / (B12 + B8A + B3 + B2)
    '-1*((SWIR2 - NIR - GREEN - BLUE) / (SWIR2 + NIR + GREEN + BLUE))',
    list(
      'SWIR2' = img$select('SWIR2'),  
      'NIR'   = img$select('NIR'),    
      'BLUE'  = img$select('Blue'), 
      'GREEN' = img$select('Green') 
    ))
  )
  
  out <- out$set("system:time_start", img$get("system:time_start"))
  out <- out$rename("NBRP")
  out <- out$copyProperties(img)
  return(out)
}


## ----------------------------------------------------------------------------------- ##
## SENTINEL-2 ----
## ----------------------------------------------------------------------------------- ##


#' Calculate NBR+ (Normalized Burn Ratio Plus) for Sentinel-2
#'
#' Calculates the NBR+ (Normalized Burn Ratio Plus) for Sentinel-2 images using the SWIR2, 
#' RE4, Green, and Blue bands.
#'
#' @param img An input image containing SWIR2, RE4, Green, and Blue bands.
#'
#' @return An image representing the NBR+ for Sentinel-2.
#'
#' @details
#' This function calculates the NBR+ (Normalized Burn Ratio Plus) for Sentinel-2 images using 
#' the SWIR2, RE4, Green, and Blue bands.
#' The formula for NBR+ is: -1 * ((SWIR2 - RE4 - Green - Blue) / (SWIR2 + RE4 + Green + Blue)).
#'
#' The input image should contain the SWIR2, RE4, Green, and Blue bands as specified in the function.
#' The output image will have a band named 'NBRP' and will inherit the properties of the input image.
#'
#' Note: Please note that the NBR+ index was adjusted to a negative form to give positive deltas high severity.
#' The adjustment was made according to the specified note.
#'
#' @references
#' Alcaras, M. 2022. "Satellite-Based Burn Severity Mapping: Implementation of the Normalized Burn 
#' Ratio Plus (NBR+) for Sentinel-2 and Landsat-8." 
#' Remote Sensing, 14(7), 1727. DOI: 10.3390/rs14071727.
#'
#' @examples
#' # Load an image with SWIR2, RE4, Green, and Blue bands
#' # img <- ee$Image('path/to/image')
#'
#' # Calculate NBR+ for Sentinel-2
#' # nbrp <- spt_calc_nbrp_s2(img)
#'

spt_calc_nbrp_s2 <- function(img){
  
  out <- ee$Image(img$expression(
    # (B12 - B8A - B3 - B2) / (B12 + B8A + B3 + B2)
    '-1*((SWIR2 - RE4 - GREEN - BLUE) / (SWIR2 + RE4 + GREEN + BLUE))',
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


#' Calculate TCTB (Tasseled Cap Transformation Brightness) for Sentinel-2
#'
#' Calculates the TCTB (Tasseled Cap Transformation Brightness) for Sentinel-2 images 
#' using the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @param img An input image containing the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @return An image representing the TCTB for Sentinel-2.
#'
#' @details
#' This function calculates the TCTB (Tasseled Cap Transformation Brightness) for Sentinel-2 
#' images using the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#' The formula for TCTB is: (BLUE x 0.351) + (GREEN x 0.3813) + (RED x 0.3437) + (NIR x 0.7196) + 
#' (SWIR1 x 0.2396) + (SWIR2 x 0.1949).
#'
#' The input image should contain the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands as specified in the function.
#' The output image will have a band named 'TCTB' and will inherit the properties of the input image.
#'
#' @examples
#' # Load an image with Blue, Green, Red, NIR, SWIR1, and SWIR2 bands
#' # img <- ee$Image('path/to/image')
#'
#' # Calculate TCTB for Sentinel-2
#' # tctb <- spt_calc_tctb_s2(img)
#'

spt_calc_tctb_s2 <- function(img){
  
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


#' Calculate TCTG (Tasseled Cap Transformation Greenness) for Sentinel-2
#'
#' Calculates the TCTG (Tasseled Cap Transformation Greenness) for Sentinel-2 images using the 
#' Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @param img An input image containing the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @return An image containing the TCTG values.
#'
#' @details The TCTG is calculated using the following formula:
#' \deqn{TCTG = (BLUE \times -0.3599) + (GREEN \times -0.3533) + (RED \times -0.4734) + 
#' (NIR \times 0.6633) + (SWIR1 \times 0.0087) + (SWIR2 \times -0.2856)}
#'
#' 
#' @examples
#' # Load a Sentinel-2 image
#' image <- ee$Image('COPERNICUS/S2_SR/20210901T184201_20210901T184205_T18TVJ')
#' 
#' # Calculate TCTG
#' tctg <- spt_calc_tctg_s2(image)
#' 

spt_calc_tctg_s2 <- function(img){
  
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


#' Calculate TCTW (Tasseled Cap Transformation Wetness) for Sentinel-2
#'
#' Calculates the TCTW (Tasseled Cap Transformation Wetness) for Sentinel-2 images using the Blue, 
#' Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @param img An input image containing the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @return An image containing the TCTW values.
#'
#' @details The TCTW is calculated using the following formula:
#' \deqn{TCTW = (BLUE \times 0.2578) + (GREEN \times 0.2305) + (RED \times 0.0883) + (NIR \times 0.1071) + 
#' (SWIR1 \times -0.7611) + (SWIR2 \times -0.5308)}
#'
#' 
#' @export
#' 

spt_calc_tctw_s2 <- function(img){
  
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

#' Calculate TCTB (Tasseled Cap Transformation Brightness) for Landsat-5
#'
#' Calculates the TCTB (Tasseled Cap Transformation Brightness) for Landsat-5 images using 
#' the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @param img An input image containing the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @return An image containing the TCTB values.
#'
#' @details The TCTB is calculated using the formula described in the references.
#' 
#' @export
#' 

spt_calc_tctb_l5 <- function(img){
  
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

#' Calculate TCTG (Tasseled Cap Transformation Greenness) for Landsat-5
#'
#' Calculates the TCTG (Tasseled Cap Transformation Greenness) for Landsat-5 images using 
#' the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @param img An input image containing the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @return An image containing the TCTG values.
#'
#' @details The TCTG is calculated using a specific formula.
#'
#' @export
#'

spt_calc_tctg_l5 <- function(img){
  
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


#' Calculate TCTW (Tasseled Cap Transformation Wetness) for Landsat-5
#'
#' Calculates the TCTW (Tasseled Cap Transformation Wetness) for Landsat-5 images using the 
#' Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @param img An input image containing the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @return An image containing the TCTW values.
#'
#' @details The TCTW is calculated using a specific formula.
#'
#' @export
#'

spt_calc_tctw_l5 <- function(img){
  
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


#' Calculate TCTB (Tasseled Cap Transformation Brightness) for Landsat-7
#'
#' Calculates the TCTB (Tasseled Cap Transformation Brightness) for Landsat-7 images using the 
#' Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @param img An input image containing the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @return An image containing the TCTB values.
#'
#' @details The TCTB is calculated using a specific formula.
#'
#' @export
#'

spt_calc_tctb_l7 <- function(img){
  
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


#' Calculate TCTG (Tasseled Cap Transformation Greenness) for Landsat-7
#'
#' Calculates the TCTG (Tasseled Cap Transformation Greenness) for Landsat-7 images using the 
#' Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @param img An input image containing the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @return An image containing the TCTG values.
#'
#' @details The TCTG is calculated using a specific formula.
#'
#' @export
#'

spt_calc_tctg_l7 <- function(img){
  
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


#' Calculate TCTW (Tasseled Cap Transformation Wetness) for Landsat-7
#'
#' Calculates the TCTW (Tasseled Cap Transformation Wetness) for Landsat-7 images using the 
#' Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @param img An input image containing the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @return An image containing the TCTW values.
#'
#' @details The TCTW is calculated using a specific formula.
#'
#' @export
#'

spt_calc_tctw_l7 <- function(img){
  
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

#' Calculate TCTB (Tasseled Cap Transformation Brightness) for Landsat-8 and 9
#'
#' Calculates the TCTB (Tasseled Cap Transformation Brightness) for Landsat-8/9 images using 
#' the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @param img An input image containing the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @return An image containing the TCTB values.
#'
#' @details The TCTB is calculated using a specific formula.
#'
#' @export
#'

spt_calc_tctb_l8 <- function(img){
  
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


#' Calculate TCTG (Tasseled Cap Transformation Greenness) for Landsat-8 and 9
#'
#' Calculates the TCTG (Tasseled Cap Transformation Greenness) for Landsat-8/9 images using the 
#' Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @param img An input image containing the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @return An image containing the TCTG values.
#'
#' @details The TCTG is calculated using a specific formula.
#'
#' @export
#'

spt_calc_tctg_l8 <- function(img){
  
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


#' Calculate TCTW (Tasseled Cap Transformation Wetness) for Landsat-8/9
#'
#' Calculates the TCTW (Tasseled Cap Transformation Wetness) for Landsat-8 images 
#' using the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @param img An input image containing the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
#'
#' @return An image containing the TCTW values.
#'
#' @details The TCTW is calculated using a specific formula.
#'
#' @export
#'

spt_calc_tctw_l8 <- function(img){
  
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

#' Calculate TCTB (Tasseled Cap Transformation Brightness) for MOD09A1
#'
#' Calculates the TCTB (Tasseled Cap Transformation Brightness) for MOD09A1 
#' images using the Blue, Green, Red, NIR, NIR2, SWIR1, and SWIR2 bands.
#'
#' @param img An input image containing the Blue, Green, Red, NIR, NIR2, SWIR1, 
#' and SWIR2 bands.
#'
#' @return An image containing the TCTB values.
#'
#' @details The TCTB is calculated using a specific formula.
#'
#' @export
#'

spt_calc_tctb_mod09a1 <- function(img){
  
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


#' Calculate TCTG (Tasseled Cap Transformation Greenness) for MOD09A1
#'
#' Calculates the TCTG (Tasseled Cap Transformation Greenness) for MOD09A1 images using 
#' the Blue, Green, Red, NIR, NIR2, SWIR1, and SWIR2 bands.
#'
#' @param img An input image containing the Blue, Green, Red, NIR, NIR2, SWIR1, and 
#' SWIR2 bands.
#'
#' @return An image containing the TCTG values.
#'
#' @details The TCTG is calculated using a specific formula.
#'
#' @export
#'

spt_calc_tctg_mod09a1 <- function(img){
  
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


#' Calculate TCTW (Tasseled Cap Transformation Wetness) for MOD09A1
#'
#' Calculates the TCTW (Tasseled Cap Transformation Wetness) for MOD09A1 images 
#' using the Blue, Green, Red, NIR, NIR2, SWIR1, and SWIR2 bands.
#'
#' @param img An input image containing the Blue, Green, Red, NIR, NIR2, SWIR1, and 
#' SWIR2 bands.
#'
#' @return An image containing the TCTW values.
#'
#' @details The TCTW is calculated using a specific formula.
#'
#' @export
#'

spt_calc_tctw_mod09a1 <- function(img){
  
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


