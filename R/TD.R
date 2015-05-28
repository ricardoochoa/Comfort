TD <-
function (Temp, RH){
  lv <- 17.625
  Rv <- 243.04
  Rv*((log(RH/100)+((lv*Temp)/(Rv+Temp)))/(lv-log(RH/100)-((lv*Temp)/(Rv+Temp))))
}
