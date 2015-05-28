\name{cz_limits}
\alias{cz_limits}
\title{Comfort Zone limits

}
\description{
Calculate the range of the comfort zone 
}
\usage{
cz_limits(Osc)
}

\arguments{
  \item{Osc}{
The difference between the minimum and maximum temperature.
}
}
\details{
The comfort zone depends of the mean temperature and the width of depends of the range between maximum and minimum temperature.  
}
\value{
The range of comfort zone.
}
\references{
Morillon, D. (2004). Atlas del bioclima de Mexico (1st ed.). Mexico, DF: Instituto de Ingenieria UNAM.
}
\author{
Ricardo Ochoa, Ilse Avalos
}

\examples{
20 + (cz_limits (17))
20 - (cz_limits(17)) 
}

