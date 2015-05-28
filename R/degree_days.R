degree_days <-
function(x, season=c("daily", "monthly", "yearly"), low, high){
  dd <-ifelse(x > high,x - high, ifelse (x < low,low - x, 0))
  
  hoc <- ifelse(x > high,"cooling",
                ifelse(x < low,"heating","comfort"))
  
  hoc<- as.factor(hoc)  
  sea <-c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug"," Sep","Oct", "Nov"," Dec")
  
  
  d<-data.frame("season"= if(season=="daily") {1:365} else{ 
    if(season=="monthly") {sea}
    else{if(season=="yearly") {1} else {NULL}}}, 
    "Degree-days"= dd, "Type"= hoc)
  return(d)
}
