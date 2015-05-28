RH <-
function(Temp, Td) {
  lv <- 17.625
  Rv <- 243.04
  hurs <- 100*(exp((lv*Td)/(Rv+Td))/exp((lv*Temp)/(Rv+Temp))) 
  return(hurs)
}
