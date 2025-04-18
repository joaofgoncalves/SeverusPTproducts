% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gee_calc.R
\name{spt_spectral_index_fun}
\alias{spt_spectral_index_fun}
\title{Gets a Spectral Index Function based on the index name and satellite}
\usage{
spt_spectral_index_fun(baseIndex, satCode = NULL, modisProduct = NULL)
}
\arguments{
\item{baseIndex}{The spectral index to be calculated (character).}

\item{satCode}{The satellite code (character). Default is NULL.}

\item{modisProduct}{The MODIS product (character). Default is NULL.}
}
\value{
The spectral index calculation function.
}
\description{
This function selects and returns the corresponding spectral index calculation
function based on the specified parameters.
}
