% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pet.R
\name{hargreavesPET}
\alias{hargreavesPET}
\title{Potential evapotranspiration (PET) by hargreaves method}
\usage{
hargreavesPET(date, tavg, tdif, lat)
}
\arguments{
\item{date}{a time-series date object}

\item{tavg}{a vector of average temperature values (°C)}

\item{tdif}{a vector of differences computed from maximum and minimum
temperatures}

\item{lat}{latitude information (negative values for southern hemisphere)}
}
\value{
the output is a vector of PET values
}
\description{
\url{http://www.civil.uwaterloo.ca/watflood/manual/02_03_2.htm}
}
