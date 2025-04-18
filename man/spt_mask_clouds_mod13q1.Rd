% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gee_prepare.R
\name{spt_mask_clouds_mod13q1}
\alias{spt_mask_clouds_mod13q1}
\title{Masks clouds in MOD13Q1 vegetation index imagery}
\usage{
spt_mask_clouds_mod13q1(image)
}
\arguments{
\item{image}{An ee$Image object representing the MOD13Q1 vegetation index imagery.}
}
\value{
An ee$Image object with clouds masked out.
}
\description{
This function applies a cloud mask to MOD13Q1 vegetation index imagery based on
the SummaryQA band values.
}
\details{
The function applies a mask to the input image using the SummaryQA band values.
It extracts specific bits from the SummaryQA band using the \code{spt_bitwise_extract}
function and creates a mask based on the extracted values. Cloudy pixels are masked out
by setting them to \code{NA} (Not Available) in the output image.
}
