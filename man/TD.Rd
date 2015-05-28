\name{TD}
\alias{TD}
\title{
Get dewpoint 
}
\description{
Calculate dewpoint from temperature and relative humidity 
}
\usage{
TD(Temp, RH)
}
\arguments{
  \item{Temp}{
Air temperature [C]
}
  \item{RH}{
Relative humidity [\%]
}
}

\value{
Dewpoint temperature [C]
}

\author{
Ricardo Ochoa, Ilse Avalos
}

\seealso{
\code{\link{RH}}}

\examples{
TD (25,50)
}
\keyword{dewpoint}
