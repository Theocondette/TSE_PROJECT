
########################
####  Utils script  ####
########################


# Packages ----

library(tidyverse)
library(patchwork)
library(KernSmooth)
library(ICV)
library(plotly)
library(np)
library(crs)

# Functions ----

  # Normal scale rule bandwidth:

h.ns<-function(dta){
  n<-length(dta)
  1.059*sqrt((n-1)*var(dta)/n)*(length(dta))^{-1/5}
}

  # Plot histogram with estimated density :

hist_year<-function(ye,df,List_of_ICV,List_of_LSCV,List_of_NSR){
  
  df_current<-df%>%
    filter(Year==ye) 
  
  
  hist(df_current$AvgTemperature,probability=T,xlab = paste("Average Temperature in",ye),main="Kernel Density Estimate",ylim=c(0,0.06))
  lines(List_of_NSR[[ye]], col='green', lwd=4, lty=2)
  lines(x=List_of_ICV[[ye]][[1]],y=List_of_ICV[[ye]][[2]], col='red', lwd=4, lty=2)
  lines(bkde(df_current$AvgTemperature,bandwidth=List_of_LSCV[[ye]]$bw), col='orange', lwd=4, lty=2)
  legend("topright", legend = c("NSR", "ICV","LSCV"),
         col = c("green", "red","orange"), lwd = c(1, 4, 4), lty = c(1, 2, 2))
  rug(df_current$AvgTemperature, col = 'blue')
}






