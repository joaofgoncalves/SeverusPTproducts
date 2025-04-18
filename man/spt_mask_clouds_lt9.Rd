% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gee_prepare.R
\name{spt_mask_clouds_lt9}
\alias{spt_mask_clouds_lt9}
\title{Masks clouds in Landsat 9 imagery}
\usage{
spt_mask_clouds_lt9(img)
}
\arguments{
\item{img}{An Earth Engine image object representing Landsat 9 imagery.}
}
\value{
An Earth Engine image object with clouds, cloud shadows, and cirrus
clouds masked out.
}
\description{
This function applies a cloud mask to Landsat 9 imagery based on the quality
assessment (QA) layer. Clouds, cloud shadows, and cirrus clouds are identified
using specific bit masks and are then masked out in the image.
}
\details{
The function performs the following steps:
\enumerate{
  \item Selects the quality assessment (QA) layer from the input image using
  the \code{select} function.
  \item Defines the bit masks for cirrus clouds, clouds, and cloud shadows using
  the \code{ee$Number} function and bitwise left shift operator (\code{\%<<\%}).
  \item Applies the masks for cirrus clouds, clouds, and cloud shadows by performing
  bitwise operations (\code{bitwiseAnd}) on the QA layer and checking if the
  corresponding bits are set to zero.
  \item Updates the image mask using the \code{updateMask} function, where pixels
  identified as cirrus clouds, clouds, or cloud shadows are masked out.
}
}
