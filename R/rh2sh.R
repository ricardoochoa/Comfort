rh2sh <-
function(Temp, Pz, RH) {
  Temperature <- Temp + 273.15 # K
  Pressure <- Pz * 100 # Pa
  # Define constants
  Rd <- 287.058 # dry air constant J/(K kg)
  Rv <- 461.5 # J/(K kg)
  T0 <- 273.15 # (K)
  es0 <- 611
  es <- Temperature
  # ____________________
  # Saturation pressure (Pa) over ice and water respectively (Bohren & Albrecht 2000, pp 197-200)
  iceMask <- which(Temperature < T0)
  es[iceMask] <- es0 * exp((6293 / T0) - (6293 / Temperature[iceMask]) - 0.555 * log(abs(Temperature[iceMask] / T0)))
  waterMask <- which(Temperature >= T0)
  es[waterMask] <- es0 * exp((6808 / T0) - (6808 / Temperature[waterMask]) - 5.09 * log(abs(Temperature[waterMask] / T0)))
  Temperature <- NULL
  ws <- (Rd / Rv) * (es / (Pressure - es))
  Pressure <- NULL
  es <- NULL
  w <- ws * RH * 0.01
  ws <- NULL
  RH <- NULL
  huss <- w / (1 + w)
  w <- NULL
  return(round(huss*1000,2))
}
