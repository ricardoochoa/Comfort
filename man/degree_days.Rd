\name{degree_days}
\alias{degree_days}

\title{
Degree days 
}
\description{
calculate the degree days. the value can be daily, monthly and yearly. 
}
\usage{
degree_days(x, season = c("daily", "monthly", "yearly"), low, high)
}

\arguments{
  \item{x}{
Air Temperature in [C]
}
  \item{season}{
There are three options of season: daily, monthly and yearly. If you choose "daily", you have to add in \code{x} 365 values of mean temperatures (1 year). If you choose "monthly", add the monthly mean temperatures (12 values). If you choose "yearly", add the yearly mean temperature.
}
  \item{low}{
The Minimum temperature limit of the comfort zone.
}
  \item{high}{
The Maximum temperature limit of the comfort zone.
}
}
\details{
When the site's temperature is outside of the comfort zone, it is measured in heating or cooling "degree days". A degree day is a measure of heating or cooling. A degree day is defined as a departure of the mean daily temperature from a given standard: one degree day for each degree of departure above (or below) the base temperature during one day. The degree day is related to the outside temperature and is not related to time.

Heating and cooling degree days can be used to relate how much more or less you might spend on heating or air conditioning if you move from one part of the country to another.
}
\value{
A table with three columns:

\item{season}{shows (depending of the season) the numbers of days, months or years}
\item{Degree.days}{shows the degree days}
\item{Type}{Type of degree day (heating or cooling)}

}
\references{
Preciado, O. U., & Molina, C. M. (2013). Grados Dia, 1-2.
}
\author{
Ricardo Ochoa, Ilse Avalos
}

\examples{
data(weather_mx)
comfort_zone <- subset(weather_mx, USAF==766120)
comfort_zone <- ddply(.data = comfort_zone, 
                .variables = c('D', "M"), 
                .fun = summarise, 
                tempmax = round(mean(Max.temp, na.rm = T),2), 
                tempmin=round(mean(Min.temp, na.rm=T),2), 
                tempmed=round(mean(Mean.temp, na.rm=T),2))
sp <- degree_days(comfort_zone$tempmed, "daily", 22,26)
sp <- subset(sp, Type != "comfort")

}

\keyword{ degree-days}
