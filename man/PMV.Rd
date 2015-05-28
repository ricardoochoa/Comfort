\name{PMV}
\alias{PMV}

\title{
Predicted Mean Vote (PMV) 
}
\description{
Computes Fanger's predicted mean vote used in ASHRAE Standard 55.
}
\usage{
PMV(clo, met, air.temp, RH, ws)
}

\arguments{
  \item{clo}{
Thermal insulation of clothing in [clo] (underwear, blouse/shirt, slacks/trousers, jacket, socks and shoes are approximately 1 clo)
}
  \item{met}{
Physical activity in [met] (one person seated at rest is approximately 1 met)
}
  \item{air.temp}{
Indoor air temperature (assumed equal to mean radiant temperature) in [C]
}
  \item{RH}{
Relative humidity in [\%] (approximately the same thing as saturation)
}
  \item{ws}{
Relative air velocity [m/s]
}
}
\details{
Compute the predicted mean vote for one or more combinations of clo, met, air temperature, relative humidity and wind speed. The inputs arguments can be scalars or vectors.
}
\value{
The predicted mean vote, a value between -3 (cold) to +3 (hot)
}
\references{
ASHRAE (American Society of Heating Refrigerating and Air-Conditioning Engineers). (2013). 2013 Ashrae Handbook: Fundamentals, I-P Edition (Vol. 30329). http://doi.org/10.1039/c1cs15219j.
}
\author{
Ricardo Ochoa, Ilse Avalos
}

\seealso{
\code{\link{PPD}}
}
\examples{
PMV(clo = 0.5,
    met = 1.2, 
    air.temp = 22, 
    RH = 60, 
    ws=0.1)
}

\keyword{ PMV }
\keyword{ ASHRAE }
