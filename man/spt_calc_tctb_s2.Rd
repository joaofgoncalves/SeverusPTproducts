% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gee_indices.R
\name{spt_calc_tctb_s2}
\alias{spt_calc_tctb_s2}
\title{Calculate TCTB (Tasseled Cap Transformation Brightness) for Sentinel-2}
\usage{
spt_calc_tctb_s2(img)
}
\arguments{
\item{img}{An input image containing the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.}
}
\value{
An image representing the TCTB for Sentinel-2.
}
\description{
Calculates the TCTB (Tasseled Cap Transformation Brightness) for Sentinel-2 images
using the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
}
\details{
This function calculates the TCTB (Tasseled Cap Transformation Brightness) for Sentinel-2
images using the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands.
The formula for TCTB is: (BLUE x 0.351) + (GREEN x 0.3813) + (RED x 0.3437) + (NIR x 0.7196) +
(SWIR1 x 0.2396) + (SWIR2 x 0.1949).

The input image should contain the Blue, Green, Red, NIR, SWIR1, and SWIR2 bands as specified in the function.
The output image will have a band named 'TCTB' and will inherit the properties of the input image.
}
\examples{
# Load an image with Blue, Green, Red, NIR, SWIR1, and SWIR2 bands
# img <- ee$Image('path/to/image')

# Calculate TCTB for Sentinel-2
# tctb <- spt_calc_tctb_s2(img)

}
