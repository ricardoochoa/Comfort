\name{stations_mx}
\alias{stations_mx}
\docType{data}
\title{
Database stations in Mexico 
}
\description{
Indicates the geoposition for each location.  
}
\usage{data("stations_mx")}
\format{
  A data frame with 130 observations on the following 9 variables.
  \describe{
    \item{\code{USAF}}{Station ID}
    \item{\code{STATION_NA}}{Station name}
    \item{\code{LAT}}{Latitude in thousandths of decimal degrees}
    \item{\code{LON}}{Longitude in thousandths of decimal degrees}
    \item{\code{ELEV}}{Elevation in meters}
    \item{\code{BEGIN}}{Beginning Period Of Record}
    \item{\code{END}}{Ending Period Of Record}
    \item{\code{NOM_CITY}}{City}
    \item{\code{NOM_STATE}}{State for MX stations}
  }
}
\source{ Database provided by NOAA.
\url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt}
}
\examples{
data(stations_mx)
}
\keyword{datasets}
