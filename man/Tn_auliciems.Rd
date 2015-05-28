\name{Tn_auliciems}
\alias{Tn_auliciems}

\title{
Neutral temperature, Auliciems equation
}
\description{
Comfort temperature proposed by Auliciems. 
}
\usage{
Tn_auliciems(x, y)
}

\arguments{
  \item{x}{
Is the monthly mean minimum temperature [C].
}
  \item{y}{
Is the monthly mean maximum temperature [C].
}
}
\value{
A list with two values.

\item{low }{comfort temperature for the month with the minimum temperature (e.g. December)}
\item{high}{comfort temperature for the month with the maximum temperature (e.g. July).}

}
\references{
Auliciems, A (1981): Towards a psycho-physiological model of thermal perception, Int J of Biometeorology, 25:109-122

Auliciems, A., & Szokolay, S. V. (2007). Thermal comfort. Passive and Low Energy Architecture International. DESIGN TOOLS AND TECHNIQUES. Note 3, 66. http://doi.org/10.1007/s00484-010-0393-2.
}
\author{
Ricardo Ochoa, Ilse Avalos
}
\note{
Only for temperatures between 18 C to 28 C.
}

\seealso{
\code{\link{Tn_Humphreys}
}}
\examples{
Tn_auliciems(20,25)
}

\keyword{ Auliciems }
