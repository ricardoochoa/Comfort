sh2rh <-
function(Temp, Pz, SH){
SH <- SH/1000
es <-  6.112 * exp((17.67 * Temp)/(Temp + 243.5))
e <- (SH * Pz) / (0.378 * SH + 0.622)
rh <- e / es
# rh[rh > 1] <- 1
# rh[rh < 0] <- 0
return(rh*100)
}
