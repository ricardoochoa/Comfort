\name{sh2rh}
\alias{sh2rh}

\title{
Convert specific humidity to relative humidity
}
\description{
converting specific humidity into relative humidity
}
\usage{
sh2rh(Temp, Pz, SH)
}

\arguments{
  \item{Temp}{
Air temperature [C]
}
  \item{Pz}{
Pressure in mb
}
  \item{SH}{
Specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass 
}
}

\value{
Relative humidity [\%]
}
\references{
From Bolton (1980) "The computation of Equivalent Potential Temperature"
\url{http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html}
}
\author{
Ricardo Ochoa, Ilse Avalos
}

\seealso{
\code{\link{rh2sh}}
}
\examples{
sh2rh(25,1013.25,9)
}

\keyword{RH}

