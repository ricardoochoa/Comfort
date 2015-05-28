\name{Comfort-package}
\alias{Comfort-package}
\alias{Comfort}
\docType{package}
\title{
Thermal Comfort methods
}
\description{
There are different methods for thermal comfort. This package is based on ASHRAE standard 55, Andris Auliciems, Humphreys and Steven Szokolay.
}
\details{
\tabular{ll}{
Package: \tab Comfort\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-05-26\cr
License: \tab MIT License\cr
}
data(weather_mx)

comfort_zone <- subset(weather_mx, USAF==766120)

comfort_zone <- 
ddply(.data = comfort_zone, 
      .variables = c('D', "M"),
      .fun = summarise,
      tempmax = round(mean(Max.temp, na.rm = T),2),
      tempmin=round(mean(Min.temp, na.rm=T),2),
      tempmed=round(mean(Mean.temp, na.rm=T),2))

comfort_zone$Osc <- comfort_zone$tempmax-comfort_zone$tempmin

geom_comfort(polygon = "Szokolay" , season = "winter", Tm = mean(comfort_zone$tempmed), Osc = mean(comfort_zone$Osc))
                      
}
\author{
Ricardo Ochoa, Ilse Avalos

Maintainer: Ilse Avalos <i.avalosvargas@gmail.com>
}
\references{
Auliciems, A., & Szokolay, S. V. (2007). Thermal comfort. Passive and Low Energy Architecture International. DESIGN TOOLS AND TECHNIQUES. Note 3, 66. http://doi.org/10.1007/s00484-010-0393-2.

Ansi/Ashrae. (2004). ANSI/ASHRAE 55:2004 Thermal Environmental Conditions for Human Occupancy. Ashrae, 2004, 30. http://doi.org/10.1007/s11926-011-0203-9.
}

\keyword{ thermal}
\keyword{Comfort}