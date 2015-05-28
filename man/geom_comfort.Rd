\name{geom_comfort}
\alias{geom_comfort}

\title{
Comfort psychometric chart 
}
\description{
Shows the comfort thermal zone on a psychometric chart.
}
\usage{
geom_comfort(polygon = c("ASHRAE", "Szokolay"), season = c("summer", "winter"), Tm, Osc)
}

\arguments{
  \item{polygon}{
A character specifying the comfort zone method. There are two options: "ASHRAE" or "Szokolay". 
}
  \item{season}{
Name of season polygon, it can be "summer" or "winter". 
}
  \item{Tm}{
Mean temperature [C].
}
  \item{Osc}{
A number specifying the range between the maximum and minimum temperatures (e.g. Osc = 22 C - 17 C). 
}
}
\details{
Comfort zones are when the people feel comfortable within a small range of temperatures and humidities. The ranges vary based on where you live and on the time of year. 
}
\value{
Shows the comfort zone on a psychometric chart. There are two differents comfort zones' methods: ASHRAE Standard 55 and Steven V. Szokolay. 
}
\references{
Honjo, T. (2009). Thermal Comfort in Outdoor Environment, 43-47.
International Organization for Standardization. (2006). ISO 7730:2006 Ergonomics of the thermal environment.

Auliciems, A., & Szokolay, S. V. (2007). Thermal comfort. Passive and Low Energy Architecture International. DESIGN TOOLS AND TECHNIQUES. Note 3, 66. http://doi.org/10.1007/s00484-010-0393-2.
}
\author{
Ricardo Ochoa, Ilse Avalos
}

\examples{
geom_comfort(polygon = "Szokolay", season = "summer", Tm = 24, Osc = 16)
}

\keyword{plot_comfort}
