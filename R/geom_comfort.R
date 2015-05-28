geom_comfort <-
function(polygon = c("ASHRAE", "Szokolay"), season = c("summer", "winter"), Tm, Osc){
  comfort <- function(wetbulbtemp=F , relativehumidity=F, specificvolume=F, enthalpy=F, colour=F){  
    
    patm = 101.325 # standard atmosphere (kPa)
    rair = 0.287 # gas constant of air (kJ/kg.K)
    
    #Tables
    
    t = 0:50 # temperature (C)
    pg = # saturation vapor pressure (kPa)
      c(0.61165,
        0.65709,0.70599,0.75808,0.81355,0.87258,
        0.93536,1.00210,1.07300,1.14830,1.22820,
        1.31300,1.40280,1.49810,1.59900,1.70580,
        1.81880,1.93840,2.06470,2.19830,2.33930,
        2.48820,2.64530,2.81110,2.98580,3.16990,
        3.36390,3.56810,3.78310,4.00920,4.24700,
        4.49690,4.75960,5.03540,5.32510,5.62900,
        5.94790,6.28230,6.63280,7.00020,7.38490,
        7.78780,8.20960,8.65080,9.11240,9.59500,
        10.0990,10.6270,11.1770,11.7520,12.3520)
    
    wg = 622*pg/(patm-pg) # saturation specific humidity
    
    # specific volume and enthalpy/wet-bulb-temp
    t1 = seq(5,35,5) # saturation temperature (C)
    pg1 = c(0.87258,1.22820,1.70580,2.33930,3.16990,4.24700,5.62900) # saturation pressure (kPa)
    wg1 = 622*pg1/(patm-pg1) # saturation specific humidity
    
    # specific volume of dry air (cubic m/kg dry air) (green)
    vol = rair*(t1+273)/(patm-pg1) # specific vol at saturation
    tv0 = patm*vol/rair-273 # air temperature at zero humidity
    
    h = t1 + 2.5*wg1 # enthalpy (kJ/kg-dry-air) (displayed)
    t0 = h # temperature at zero humidity for enthalpy h  
    
    c1=c2=c3=c4="grey"  
    
    if ( colour==T ) {
      c1="red"
      c2="blue"
      c3="green"
      c4="grey"
    }
    
    #chart
    p=  qplot(x=c(0,0), y=c(0,0), geom="line")+
      coord_cartesian(xlim = c(0,50), ylim = c(0,30))+
      xlab("Dry Bulb Temperature (C)")+
      ylab("Specific Humidity  (grams moisture/kg dry air)")+
      theme_bw()
    
    #Enthalpy
    if ( enthalpy==T ) {
      p=p+  
        geom_line(colour=c4, aes(x=c(10, (10 - 12.5)/3.5), y=c(0, 5+(10 - 12.5)/3.5)))+
        geom_line(colour=c4, aes(x=c(20, (20 - 12.5)/3.5), y=c(0, 5+(20 - 12.5)/3.5)))+
        geom_line(colour=c4, aes(x=c(30, (30 - 12.5)/3.5), y=c(0, 5+(30 - 12.5)/3.5)))+
        geom_line(colour=c4, aes(x=c(40, (40 - 12.5)/3.5), y=c(0, 5+(40 - 12.5)/3.5)))+
        geom_line(colour=c4, aes(x=c(50, (50 - 12.5)/3.5), y=c(0, 5+(50 - 12.5)/3.5)))+
        geom_line(colour=c4, aes(x=c(60, (60 - 12.5)/3.5), y=c(0, 5+(60 - 12.5)/3.5)))+
        geom_line(colour=c4, aes(x=c(70, (70 - 12.5)/3.5), y=c(0, 5+(70 - 12.5)/3.5)))+
        geom_line(colour=c4, aes(x=c(80, (80 - 12.5)/3.5), y=c(0, 5+(80 - 12.5)/3.5)))+
        geom_line(colour=c4, aes(x=c(90, (90 - 12.5)/3.5), y=c(0, 5+(90 - 12.5)/3.5)))+
        geom_line(colour=c4, aes(x=c(100, (100 - 12.5)/3.5), y=c(0, 5+(100 - 12.5)/3.5)))+
        geom_line(colour=c4, aes(x=c(110, (110 - 12.5)/3.5), y=c(0, 5+(110 - 12.5)/3.5)))+
        geom_line(colour=c4, aes(x=c(0,25), y=c(5,30)))+ # the oblique enthalpy axis
        geom_text(size=3, colour=c4, aes(label=seq(20,90,10), 
                                         x=c( (20 - 16)/3.5, (30 - 16)/3.5, 
                                              (40 - 16)/3.5, (50 - 16)/3.5, 
                                              (60 - 16)/3.5, (70 - 16)/3.5, 
                                              (80 - 16)/3.5, (90 - 16)/3.5), 
                                         y=c( 5+(20 - 11.5)/3.5, 5+(30 - 11.5)/3.5, 
                                              5+(40 - 11.5)/3.5, 5+(50 - 11.5)/3.5, 
                                              5+(60 - 11.5)/3.5, 5+(70 - 11.5)/3.5, 
                                              5+(80 - 11.5)/3.5, 5+(90 - 11.5)/3.5)))+
        geom_text(size=3, colour=c4, aes(label=c("Enthalpy\ndry air kJ/kg"), 
                                         x=14, y=25 ))
    } 
    
    #wet bulb temperature 
    if ( wetbulbtemp==T ) {
      p=p+    
        geom_line(colour=c1, aes(x=t,y=wg))+    
        geom_line(colour=c1, aes(x=c(t1[1], t0[1]),y=c(wg1[1],0)))+
        geom_line(colour=c1, aes(x=c(t1[2], t0[2]),y=c(wg1[2],0)))+
        geom_line(colour=c1, aes(x=c(t1[3], t0[3]),y=c(wg1[3],0)))+
        geom_line(colour=c1, aes(x=c(t1[4], t0[4]),y=c(wg1[4],0)))+
        geom_line(colour=c1, aes(x=c(t1[5], t0[5]),y=c(wg1[5],0)))+
        geom_line(colour=c1, aes(x=c(t1[6], t0[6]),y=c(wg1[6],0)))+
        geom_line(colour=c1, aes(x=c(t1[6], t0[6]),y=c(wg1[6],0)))+
        geom_text(size=3, colour=c1, aes(label=c("5C", "10C", "15C", "20C", "25C", "30C"), 
                                         x=c(4.5,8.7,13.7,18.7,23.7,28.7), 
                                         y=c(6,8,11, 15, 20.5, 27.5) ))+
        geom_text(size=3, colour=c1, aes(label=c("Wet bulb\ntemperature"), 
                                         x=17, y=17.5 ))
    } 
    
    #Relative humidity
    if ( relativehumidity==T ) {
      p=p+  
        geom_line(colour=c2, aes(x=t,y=622*0.1*pg/(patm-0.1*pg)))+
        geom_line(colour=c2, aes(x=t,y=622*0.2*pg/(patm-0.2*pg)))+
        geom_line(colour=c2, aes(x=t,y=622*0.3*pg/(patm-0.3*pg)))+
        geom_line(colour=c2, aes(x=t,y=622*0.4*pg/(patm-0.4*pg)))+
        geom_line(colour=c2, aes(x=t,y=622*0.6*pg/(patm-0.6*pg)))+
        geom_line(colour=c2, aes(x=t,y=622*0.8*pg/(patm-0.8*pg)))+
        geom_text(size=3, colour=c2, aes(label=c("10%", "20%", "30%", "80%", "60%", "40%"), 
                                         x=c(47.5,47.5,47.5,33.2,38.5,46.5), 
                                         y=c(8, 15.2, 23, 29, 29, 29) ))+
        geom_text(size=3, colour=c2, aes(label=c("Relative\nhumidity"), 
                                         x=46, y=9.6 ))
      
    } 
    
    #VSpecific volume 
    if ( specificvolume==T ) {
      p=p+  
        geom_line(colour=c3, aes(x=c(t1[1], tv0[1]),y=c(wg1[1],0)))+
        geom_line(colour=c3, aes(x=c(t1[2], tv0[2]),y=c(wg1[2],0)))+
        geom_line(colour=c3, aes(x=c(t1[3], tv0[3]),y=c(wg1[3],0)))+
        geom_line(colour=c3, aes(x=c(t1[4], tv0[4]),y=c(wg1[4],0)))+
        geom_line(colour=c3, aes(x=c(t1[5], tv0[5]),y=c(wg1[5],0)))+
        geom_line(colour=c3, aes(x=c(t1[6], tv0[6]),y=c(wg1[6],0)))+
        geom_line(colour=c3, aes(x=c(t1[7], tv0[7]),y=c(wg1[7],0)))+
        geom_text(size=3, colour=c3, aes(label=c("0.79", "0.81", "0.83", "0.85", "0.87", "0.90", "0.92"), 
                                         x=c(8.2,14.3,21.1,28.5,36,44.5, 48), 
                                         y=c(1, 1, 1, 1, 1, 1, 12) ))+
        geom_text(size=3, colour=c3, aes(label=c("Specific\nvolume m3/kg"), 
                                         x=46, y=2.6 ))
      
    } 
    
    #print chart
    return(p)
  }
  
  cz_limits<- function(x){
    ifelse(x <= 13,2.5,
           ifelse(x > 13 & x <= 16,3,
                  ifelse(x > 16 & x <= 19,3.5,
                         ifelse(x > 19 & x <= 24,4,
                                ifelse(x > 24 & x <= 28,4.5,
                                       ifelse(x > 28 & x <= 33,5,
                                              ifelse(x > 33 & x <= 38,5.5,
                                                     ifelse(x > 38 & x <= 45,6,
                                                            ifelse(x > 45 & x <= 52,6.5,
                                                                   ifelse(52< x,7,0))))))))))}
  
  summer <- data.frame ("Temp"= c(23.6, 26.8, 28.3,25.1), "SH"= c(12, 12, 0, 0))
  
  winter <- data.frame ("Temp"= c(19.6, 23.9, 26.3,21.7), "sH"= c(12, 12, 0, 0))
  
  Os <- data.frame("Temp"= c((17.6 + (0.31*Tm) - cz_limits(Osc))-2.5, 17.6 + (0.31*Tm) + cz_limits(Osc)-2.5,
                             17.6 + (0.31*Tm) + cz_limits(Osc), 17.6 + (0.31*Tm) - cz_limits(Osc)),
                   "SH" = c(12,12,4,4))
  
  pol <- function(polygon = c("ASHRAE", "Szokolay"), season = c("summer", "winter")){
    
    if (polygon =="ASHRAE"){
      p =  if(season =="summer") return (geom_polygon(data=summer, aes(x=Temp, y=SH,alpha=0.2)))
      else{if (season == "winter") return (geom_polygon(data=winter, aes(x=Temp, y=SH,alpha=0.2)))
           else{return(NULL)}}          
      return (p)
    }
    if (polygon =="Szokolay"){geom_polygon(data=Os, aes(x=Temp, y=SH,alpha=0.2))}
  }
  
  
  comfortPlot <- plot({
    comfort(wetbulbtemp = T, relativehumidity = T, specificvolume = T,enthalpy = T, colour = T) +
      pol (polygon, season) +
      ylab("Specific humidity [g/kg]")+
      xlab('Air temperature [C]')})
  
}
