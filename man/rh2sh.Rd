\name{rh2sh}
\alias{rh2sh}

\title{
Relative humidity to specific humidity
}
\description{
Computes specific humidity from given relative humidity, temperature and pressure.
}
\usage{
rh2sh(Temp, Pz, RH)
}

\arguments{
  \item{Temp}{
Air Temperature[C]
}
  \item{Pz}{
Pressure in [hPa]
}
  \item{RH}{
Relative humidity [\%]
}
}

\value{
Specific humidity (kg/kg).
}
\references{
Bolton (1980): The computation of Equivalent Potential Temperature.
\url{http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html}
}
\author{
Ricardo Ochoa, Ilse Avalos
}
\seealso{
\code{\link{RH}, \link{sh2rh}}
}
\examples{
plot(x = 10:30,y = rh2sh(10:30, 1013, 50), xlab="Temperature [C]", 
ylab="Specific humidity", main="Specific humidity for RH=0.5 and varying temperature")
}

\keyword{ SH }
