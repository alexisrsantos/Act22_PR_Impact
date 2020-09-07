install.packages("CausalImpact") #Install the library
library(ggplot2) #Get the visualization library
library(CausalImpact) #Activate Causal Impact library

set.seed(9841981) #Seed for Monte-Carlo simulations 

#Read the data
library(readxl) #Read excel function
data <- read_excel("~/full_panel_alljobs.xlsx")

dummy_data<-cbind(data$Jobs) #Generate the time series for non-Agricultural Jobs

matplot(dummy_data,type="l") #Simple plot

pre.period <- c(1, 275) #Pre-Intervention 
post.period <- c(276, 348) #Post-Intervention

# Fit the causal model
impact <- CausalImpact(dummy_data[,1], pre.period, post.period) 

# See the plots with the original time series, and cumulative change
plot<-plot(impact, c("original","cumulative"))

#See add a title
impact.plot1 <- plot + theme_classic()+ylab("Total Jobs (in thousands)")

#See the summary of the model
summary(impact)

#See the report of the model
summary(impact, "report")

#Run the model with 500 simulations and 7 seasonal effects
impact1 <- CausalImpact(dummy_data, pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))

# See the plot of the new model
plot(impact1, c("original", "pointwise"))

# Summary of the model with simulations and seasonal effects
summary(impact1)

#See the report for the model
summary(impact1, "report")

##############################
## Sector Specific Analysis ##
##############################

data <- read_excel("~/full_panel.xlsx")

data<-subset(data,Sector_Num==7)
data$Jobs<-as.numeric(data$Jobs)

dummy_data<-cbind(data$Jobs)

matplot(dummy_data,type="l")

#Determine the pre- and post-intervention periods
pre.period <- c(1, 269)
post.period <- c(270, 348)

impact <- CausalImpact(dummy_data[,1], pre.period, post.period)

plot<-plot(impact, c("original","cumulative"))

impact.plot2 <- plot + theme_classic()+ylab("Service Jobs (in thousands)")

plot(impact, c("original", "pointwise"))

summary(impact)

summary(impact, "report")

impact1 <- CausalImpact(dummy_data, pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))

plot(impact1, c("original", "pointwise"))

summary(impact1, "report")


###FIRE####
data2 <- read_excel("~/full_panel.xlsx")

data2<-subset(data2,Sector_Num==5)
data2$Jobs<-as.numeric(data2$Jobs)

dummy_data2<-cbind(data2$Jobs)

matplot(dummy_data2,type="l")

pre.period <- c(1, 269)
post.period <- c(270, 348)

impact2 <- CausalImpact(dummy_data2[,1], pre.period, post.period)

plot2<-plot(impact2, c("original","cumulative"))
impact.plot3 <- plot2 + theme_classic()+ylab("Jobs in FIRE (in thousands)")
impact.plot3 

plot(impact, c("original", "pointwise"))

summary(impact)

summary(impact, "report")

impact2 <- CausalImpact(dummy_data2, pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))

plot(impact2, c("original", "pointwise"))

summary(impact2)

summary(impact2, "report")

#######Transportation and Communication#
data3 <- read_excel("~/full_panel.xlsx")

data3<-subset(data3,Sector_Num==6)
data3$Jobs<-as.numeric(data3$Jobs)

dummy_data3<-cbind(data3$Jobs)

matplot(dummy_data3,type="l")

pre.period <- c(1, 269)
post.period <- c(270, 348)

impact <- CausalImpact(dummy_data3[,1], pre.period, post.period)

plot(impact)

plot(impact, c("original", "pointwise"))

summary(impact)

summary(impact, "report")

impact3 <- CausalImpact(dummy_data3, pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))

impact.plot4<-plot(impact3, c("original","cumulative"))
impact.plot4<-impact.plot4+theme_classic()+ylab("Jobs in Commu. and Transportation (in thousands)")

plot(impact3, c("original", "cumulative"))

summary(impact3, "report")

library(ggpubr) #Join Figures

#This is figure 2
joined<-ggarrange(impact.plot2, impact.plot3,
                  impact.plot4,ncol=2,nrow=2)

joined #See Figure 2

##############
###FIGURE 1###
##############
library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)

data <- read_excel("~/full_panel.xlsx")
data$Jobs<-as.numeric(data$Jobs)

#Produces Figure 1
sb <- ggplot(data, aes(x=Month_cont, weight = Jobs, fill = Sector)) +
  geom_bar(width =1)+theme_bw()+
  labs(x ="Year", y = "Jobs (in thousands)")+
  scale_x_discrete(limits=seq(1,348,12),
                   labels=seq(1990,2018,1))+
  geom_vline(xintercept = 270, linetype="dashed", 
             color = "gray34", size=1)+
  geom_hline(yintercept=1300, linetype="dashed", 
             color = "red", size=1)+
  theme(legend.position="bottom")
  
sb #See Figure 1