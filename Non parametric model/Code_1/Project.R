
#########################################
##### Non parametric models project #####
#########################################
#########################################

  # DOCUMENT ORGANISATION # 

# 0_Preparation_environment
# 1_Descriptive analysis
# 3

# 0_Preparation_environment ----
# Packages and function:

source("Utils.R")


# Import of the data base:
df<-read.csv("Data_0/city_temperature.csv")
df$Year<-as.numeric(df$Year)
df<-df%>%
  filter(AvgTemperature>0,Year !=2020)


# Descriptive analysis ----


df%>%
  group_by(City)%>%
  summary(AvgTemperature)


combined_plots=list()
for (j in unique(df$Region)){
  
df_ggplot<-df%>%
  filter(Region==j)%>%
  group_by(Year)%>%
  summarise(mean_value = mean(AvgTemperature),
            max=max(AvgTemperature),
            min=min(AvgTemperature),
            var=var(AvgTemperature))
plot_list=list()
for(i in c("mean_value","max","min","var")){
  i<-as.name(i)
  plot_list[[i]]<-df_ggplot%>%
  ggplot()+
  geom_line(aes(x=Year,y=!! i),size=1)+
    ggtitle(paste(i,paste("in ",j)))
  
  
  
}
combined_plots[[j]] <- wrap_plots(plotlist = plot_list)
}
plot=wrap_plots(plotlist = combined_plots)
plot

df$Year<-as.factor(df$Year)

df%>%
  filter(Region=="South/Central America & Carribean")%>%
  ggplot()+
  geom_boxplot(aes(x=Year,y=AvgTemperature,fill=Year))+
  ggtitle("Evolution of the mean temperature in South/Central America & Carribean")
  
  
df<-df%>%
  filter(Region=="Africa")

# Our data is not really approximately symmetric  :
df%>%
  ggplot()+
  geom_histogram(aes(x=AvgTemperature))

# Let's look at the distribution by country:

df%>%
  ggplot()+
  geom_boxplot(aes(x =Country,y=AvgTemperature,fill=Year))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# We are better.

fig <- plot_ly(df%>%
                 filter(Country=="Algeria"),alpha = 0.6) %>%
  add_histogram(x = ~AvgTemperature, color = ~as.factor(Year)) %>%
  layout(barmode ="overlay",title="AvgTemperature distribution in May from 1995 to 2019")
fig

# df <- df%>%
#   filter(Country=="Finland")
df <- df%>%
    filter(Country=="Algeria")

df%>%
  ggplot()+
  geom_boxplot(aes(x=Year,y=AvgTemperature,fill=Year))+
  ggtitle("Evolution of the temperature distribution in Algeria")




df%>%
  ggplot()+
    geom_histogram(aes(x = AvgTemperature))+
    ggtitle("Distribution of the temperature in Algeria from 1995 to 2019")

df%>%
  ggplot()+
  geom_boxplot(aes(x=as.factor(Month),y=AvgTemperature,fill=as.factor(Year)))+
  ggtitle("Evolution of the temperature distribution by month in Algeria")+
  xlab("Month")



# Kernel density estimation ----

# Let's estimate the density every 4 years interval


df_1995_2000<-df%>%
  filter(Year%in%c("1995", "1996" ,"1997" ,"1998" ,"1999","2000"))

df_2000_2006<-df%>%
  filter(Year%in%c("2006", "2001" ,"2002", "2003" ,"2004" ,"2005"))

df_2007_2012<-df%>%
  filter(Year%in%c("2012", "2007" ,"2008", "2009" ,"2010" ,"2011"))

df_2013_2018<-df%>%
  filter(Year%in%c("2018", "2013" ,"2014", "2015" ,"2016" ,"2017"))



# Normal scale rule :

d.ns_1995_2000<-bkde(df_1995_2000$AvgTemperature, bandwidth = h.ns(df_1995_2000$AvgTemperature))
plot(d.ns_1995_2000,type="l", ylab='Density', xlab='Population', col='red',
     main="Estimator of the density of population with the Normal Scale Rule")
rug(df_1995_2000$AvgTemperature, col = 'blue')

d.ns_2000_2006<-bkde(df_2000_2006$AvgTemperature, bandwidth = h.ns(df_2000_2006$AvgTemperature))
plot(d.ns_2000_2006,type="l", ylab='Density', xlab='Population', col='red',
     main="Estimator of the density of population with the Normal Scale Rule")
rug(df_2000_2006$AvgTemperature, col = 'blue')

d.ns_2007_2012<-bkde(df_2007_2012$AvgTemperature, bandwidth = h.ns(df_2007_2012$AvgTemperature))
plot(d.ns_2007_2012,type="l", ylab='Density', xlab='Population', col='red',
     main="Estimator of the density of population with the Normal Scale Rule")
rug(df_2007_2012$AvgTemperature, col = 'blue')

d.ns_2013_2018<-bkde(df_2013_2018$AvgTemperature, bandwidth = h.ns(df_2013_2018$AvgTemperature))
plot(d.ns_2013_2018,type="l", ylab='Density', xlab='Population', col='red',
     main="Estimator of the density of population with the Normal Scale Rule")
rug(df_2013_2018$AvgTemperature, col = 'blue')


# LSCV :

hLSCV_1995_2000<-npudensbw(formula= ~df_1995_2000$AvgTemperature, bwmethod = "cv.ls")
hLSCV_2000_2006<-npudensbw(formula= ~df_2000_2006$AvgTemperature, bwmethod = "cv.ls")
hLSCV_2007_2012<-npudensbw(formula= ~df_2007_2012$AvgTemperature, bwmethod = "cv.ls")
hLSCV_2013_2018<-npudensbw(formula= ~df_2013_2018$AvgTemperature, bwmethod = "cv.ls")



# KDE ICV :
den_1995_2000<-KDE_ICV(df_1995_2000$AvgTemperature)
den_2000_2006<-KDE_ICV(df_2000_2006$AvgTemperature)
den_2007_2012<-KDE_ICV(df_2007_2012$AvgTemperature)
den_2013_2018<-KDE_ICV(df_2013_2018$AvgTemperature)




# Comparison ICV and NSR
par(mfrow=c(2,2))

hist(df_1995_2000$AvgTemperature,probability=T,xlab = "Average Temperature 1995-2000",main="Histogram and Kernel Density Estimate",ylim=c(0,0.04))
lines(d.ns_1995_2000, col='green', lwd=4, lty=2)
lines(x=den_1995_2000[[1]],y=den_1995_2000[[2]], col='red', lwd=4, lty=2)
lines(bkde(df_1995_2000$AvgTemperature,bandwidth=hLSCV_1995_2000$bw), col='orange', lwd=4, lty=2)

legend("topright", legend = c("NSR", "ICV","LSCV"),
       col = c( "green", "red","orange"), lwd = c(1, 4, 4), lty = c(1, 2, 2))

hist(x=df_2000_2006$AvgTemperature,probability=T,xlab = "Average Temperature 2000-2006",main="Histogram and Kernel Density Estimate",ylim=c(0,0.04))
lines(d.ns_2000_2006, col='green', lwd=4, lty=2)
lines(bkde(df_2000_2006$AvgTemperature,bandwidth=hLSCV_2000_2006$bw), col='orange', lwd=4, lty=2)
lines(x=den_2000_2006[[1]],y=den_2000_2006[[2]], col='red', lwd=4, lty=2)
legend("topright", legend = c("NSR", "ICV","LSCV"),
       col = c("green", "red","orange"), lwd = c(1, 4, 4), lty = c(1, 2, 2))

hist(df_2007_2012$AvgTemperature,probability=T,xlab = "Average Temperature 2007-2012",main="Histogram and Kernel Density Estimate",ylim=c(0,0.04))
lines(d.ns_2007_2012, col='green', lwd=4, lty=2)
lines(x=den_2007_2012[[1]],y=den_2007_2012[[2]], col='red', lwd=4, lty=2)
lines(bkde(df_2007_2012$AvgTemperature,bandwidth=hLSCV_2007_2012$bw), col='orange', lwd=4, lty=2)
legend("topright", legend = c("NSR", "ICV","LSCV"),
       col = c( "green", "red","orange"), lwd = c(1, 4, 4), lty = c(1, 2, 2))


hist(df_2013_2018$AvgTemperature,probability=T,xlab = "Average Temperature 2013-2018",main="Histogram and Kernel Density Estimate",ylim=c(0,0.04))
lines(d.ns_2013_2018, col='green', lwd=4, lty=2)
lines(x=den_2013_2018[[1]],y=den_2013_2018[[2]], col='red', lwd=4, lty=2)
lines(bkde(df_2013_2018$AvgTemperature,bandwidth=hLSCV_2013_2018$bw), col='orange', lwd=4, lty=2)
legend("topright", legend = c("NSR", "ICV","LSCV"),
       col = c("green", "red","orange"), lwd = c(1, 4, 4), lty = c(1, 2, 2))






# Evolution of the extreme events ----

# Evolution of the probability of having an average of more than 86?F (which is extreme since it correspond to an average of 30?C in a day) :

list_year <- as.character(seq(1995,2019,1))
Evolution <- data.frame(year=list_year,Prob=rep(NA,25))
List_of_ICV <-list()
List_of_LSCV <- list()
List_of_NSR <- list()
for(ye in list_year){
  print(ye)
  
  # Prob estimation
  df_current<-df%>%
    filter(Year==ye)  
  
  List_of_LSCV[[ye]]<-npudensbw(formula= ~df_current$AvgTemperature, bwmethod = "cv.ls")
  List_of_ICV[[ye]] <-KDE_ICV(df_current$AvgTemperature)
  List_of_NSR[[ye]] <-bkde(df_current$AvgTemperature, bandwidth = h.ns(df_current$AvgTemperature))
  
  P_current<-sum(List_of_ICV[[ye]][[2]][which(List_of_ICV[[ye]][[1]]>=86)])
  Evolution[which(Evolution$year==ye),"Prob"]<-P_current
  print(P_current)
}

par(mfrow=c(1,3))

  
hist_year(ye="1996",df,List_of_ICV,List_of_LSCV,List_of_NSR)
hist_year(ye="2008",df,List_of_ICV,List_of_LSCV,List_of_NSR)
hist_year(ye="2016",df,List_of_ICV,List_of_LSCV,List_of_NSR)

#2003
# 2005





plot(y=Evolution$Prob, x= Evolution$year,type='l')
ggplot(data = Evolution)+
  geom_smooth(aes(x =as.numeric(year),y=Prob))

#Non parametric fit : Local polynomial

plot(as.numeric(Evolution$year),Evolution$Prob,type="l",col="red",xlab="Year",ylab="Probability",main="Evolution of extreme event probability in Algeria")

lines(locpoly(as.numeric(Evolution$year),Evolution$Prob,bandwidth=3))

Evolution$x_smooth<-locpoly(as.numeric(Evolution$year),Evolution$Prob,bandwidth=3)$x

ggplot(data=Evolution)


