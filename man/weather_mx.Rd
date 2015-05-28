\name{weather_mx}
\alias{weather_mx}
\docType{data}
\title{
Meteorological data
}
\description{
Meteorological data from automated weather stations in Mexico
}
\usage{data("weather_mx")}
\format{
  A data frame with 395403 observations on the following 16 variables.
  \describe{
    \item{\code{USAF}}{Station number}
    \item{\code{Y}}{Year}
    \item{\code{M}}{Month}
    \item{\code{D}}{Day}
    \item{\code{Mean.temp}}{Mean temperature [C]}
    \item{\code{Min.temp}}{Minimum temperature[C]}
    \item{\code{Max.temp}}{Maximum temperature[C]}
    \item{\code{Dewpoint}}{Mean dew point [C]}
    \item{\code{Relative.H}}{Relative humidity [\%]}
    \item{\code{Specific.H}}{Specific humidity}
    \item{\code{Pressure}}{Mean air pressure [hPa]}
    \item{\code{Precipitation}}{Total precipitation}
    \item{\code{Visibility}}{Visibility for the day [m]}
    \item{\code{Wind.speed}}{Mean wind speed [m/s]}
    \item{\code{Max.WS}}{Maximum sustained wind speed (1 min) [m/s]}
    \item{\code{Wind.gust}}{Maximum wind gust}
  }
}
\details{
This dataset contains automated weather station data from the National Oceanic and Atmospheric Administration (NOAA).
}
\source{
Data provided by National Oceanic and Atmospheric Administration (NOAA).
\url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt}
}

\examples{
data(weather_mx)
## maybe str(weather_mx) ; plot(weather_mx) ...
}
\keyword{datasets}
