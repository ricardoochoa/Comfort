PPD <-
function(PMV){
  100 - 95*exp(-(0.03353 *(PMV^4) + 0.2179 *(PMV^2)))
}
