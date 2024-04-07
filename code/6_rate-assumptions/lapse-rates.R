library(readr)
data<-read_csv("data/inforce-data-cleaned.csv")
data[c(2,5,7,8,9,10,11,12,14,16)]<-lapply(data[c(2,5,7,8,9,10,11,12,14,16)], factor)

data$End.year<-pmin(2023,data$Year.of.Death,data$Year.of.Lapse, na.rm = TRUE)
data$Policy.exposure<-data$End.year-data$Issue.year+1
data$End.age<-data$End.year-data$Issue.year + data$Issue.age


#Only T20 policies have lapses, so we measure lapse rate within T20
T20_data<-data[data$Policy.type == "T20",]

active_T20<-matrix(NA,nrow = 4, ncol = 20)
#Year of policy
active_T20[1,]<-1:20
#Number of policies that survive up to year x
T20_survival<-function(x){
  sum(T20_data$Policy.exposure>=x)
}
active_T20[2,]<-sapply(active_T20[1,], T20_survival)
#Number of policies that lapse in year x
T20_lapse<-function(x){
  sum(T20_data$Lapse.Indicator==1 & T20_data$Policy.exposure == x)
}
active_T20[3,]<-sapply(active_T20[1,], T20_lapse)
#Calculate rate of lapse in each year
active_T20[4,]<-active_T20[3,]/active_T20[2,]
#Policies end in the 20th year, so we graph lapse rate for years 1 to 19
#Note Lapse rate is fluctuating but appears stationary over time
plot(active_T20[4,1:19], type = "l")
#Total lapses over total years covered gives overall lapse rate, extremely
#close to 1% so we round to 1%
sum(active_T20[3,1:19])/sum(active_T20[2,1:19])
