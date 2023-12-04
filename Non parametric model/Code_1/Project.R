
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
  filter(AvgTemperature>0)





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
  
  












