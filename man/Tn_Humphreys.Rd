\name{Tn_Humphreys}
\alias{Tn_Humphreys}

\title{
Neutral temperature, Humphreys equation.
}
\description{
Thermal comfort method proposed by Humphreys and Nicol for indoor buildings.
}
\usage{
Tn_Humphreys(To, TB = c("NVB", "ACB", "CB"))
}

\arguments{
  \item{To}{
Is the monthly mean of the outdoor air temperature [C]
}
  \item{TB}{
Is the type of bulding. There are 3 types of buildings: \code{NVB} is for naturally ventilated buildings, \code{ACB} is air-conditioned buildings and \code{CB} is for combined building.
}
}
\details{
Humphreys equation is a fit to considerable data for climate-controlled and non-climate controlled buildings.
}
\value{
The neutral temperature in celsius degree.
}
\references{
Fuentes, Victor.:2014, Mapas bioclimaticos de la Republica Mexicana, Mexico DF. 2014 edition.
}
\author{
Ricardo Ochoa, Ilse Avalos
}


\seealso{
\code{\link{Tn_auliciems}}
}
\examples{
data(weather_mx)
Tn_Humphreys(weather_mx$Mean.temp, "NVB")
}

\keyword{Humphreys}
