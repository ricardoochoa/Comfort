\name{PPD}
\alias{PPD}

\title{
Predicted Percent of Dissatisfied
}
\description{
A percent of dissatisfied persons. 
}
\usage{
PPD(PMV)
}

\arguments{
  \item{PMV}{
Predicted Mean Vote. A value between -3 to +3
}
}
\details{
After estimating the PMV, the predicted percent dissatisfied (PPD) with a condition can be estimated.A PPD of 10\% corresponds to the PMV range of 0.5, and even with PMV = 0, about 5\% of the people are dissatisfied.
}
\value{
The number of thermally dissatisfied persons among a large group pf people. [\%]
}

\author{
Ricardo Ochoa, Ilse Avalos
}
\note{
Ansi/Ashrae. (2004). ANSI/ASHRAE 55:2004 Thermal Environmental Conditions for Human Occupancy. Ashrae, 2004, 30. http://doi.org/10.1007/s11926-011-0203-9.
}

\seealso{
\code{\link{PMV}}
}
\examples{
PMV <- seq(-3, +3, 0.5)
plot(x= PMV, y= PPD(PMV))
}

\keyword{ PPD }
\keyword{ ASHRAE }
