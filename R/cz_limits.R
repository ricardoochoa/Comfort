cz_limits <-
function(Osc){
  ifelse(Osc <= 13,2.5,
         ifelse(Osc > 13 & Osc <= 16,3,
                ifelse(Osc > 16 & Osc <= 19,3.5,
                       ifelse(Osc > 19 & Osc <= 24,4,
                              ifelse(Osc > 24 & Osc <= 28,4.5,
                                     ifelse(Osc > 28 & Osc <= 33,5,
                                            ifelse(Osc > 33 & Osc <= 38,5.5,
                                                   ifelse(Osc > 38 & Osc <= 45,6,
                                                          ifelse(Osc > 45 & Osc <= 52,6.5,
                                                                 ifelse(52<= Osc,7,0))))))))))}
