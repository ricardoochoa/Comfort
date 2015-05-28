\name{RH}
\alias{RH}

\title{
Get relative humidity
}
\description{
Calculate RH from temperature and dewpoint
}
\usage{
RH(Temp, Td)
}

\arguments{
  \item{Temp}{
Air temperature [C]
}
  \item{Td}{
Dew point [C]
}
}

\value{
Percent of Relative humidity [\%]
}

\author{
Ricardo Ochoa, Ilse Avalos
}

\seealso{
\code{\link{rh2sh}}
}
\examples{
RH(25,13)
}
\keyword{ RH }
