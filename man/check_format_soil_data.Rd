% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_format_soil_data.R
\name{check_format_soil_data}
\alias{check_format_soil_data}
\title{Format soil data for Soil Water Balance Modelling}
\usage{
check_format_soil_data(
  soil_data,
  layer_col = "layer_depth",
  FC_col = "ThetaFC",
  PWP_col = "ThetaPWP"
)
}
\arguments{
\item{soil_data}{data.frame, containing one-line-per-layer PWP and FC}

\item{layer_col}{character, the name of the column containing TDR measurement depths}

\item{FC_col}{character, the name of the column containing FC}

\item{PWP_col}{character, the name of the column containing PWP}
}
\value{

}
\description{
This function takes soil FC and PWP (measured or estimated for each layer) as an input, checks the values and returns the corresponding values at a 1cm layer-depth resolution (assuming valeus are homogenous within a measured layer)
}
