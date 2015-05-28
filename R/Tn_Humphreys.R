Tn_Humphreys <-
function(To, TB=c("NVB", "ACB", "CB")){
  if(TB=="NVB") {11.9+0.534+To}
  else{if (TB=="ACB") {23.9+0.295*(To-22)*exp(-((To-22)/(24*sqrt(2)))^2)}
       else{if(TB=="CB"){24.2+0.430*(To-22)*exp(-((To-22)/(20*sqrt(2)))^2)}
            else{NULL}}}}
