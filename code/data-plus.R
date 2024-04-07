library(readr)
library(openxlsx)
data<-read_csv("data/inforce-data-cleaned.csv")
mort_data<-read.xlsx("srcsc-2024-lumaria-mortality-table.xlsx", startRow = 14)
data[c(2,5,7,8,9,10,11,12,14,16)]<-lapply(data[c(2,5,7,8,9,10,11,12,14,16)], factor)

data$End.year<-pmin(2023,data$Year.of.Death,data$Year.of.Lapse, na.rm = TRUE)
data$Policy.exposure<-data$End.year-data$Issue.year+1
data$End.age<-data$End.year-data$Issue.year + data$Issue.age


active_policies<-matrix(NA,5,83)
'age of policyholder'
active_policies[1,]<-18:100
'find number of policies inforce at a certain age'
check_active<-function(x){
  sum(data$End.age>=x & data$Issue.age<=x)
}
'find deaths at a certain age'
check_death<-function(x){
  sum(data$End.age==x & data$Death.indicator==1)
}
active_policies[2,]<-sapply(active_policies[1,], check_active)
active_policies[3,]<-sapply(active_policies[1,], check_death)
'death rate for age'
active_policies[4,]<-active_policies[3,]/active_policies[2,]
'lumaria mortality table values'
active_policies[5,]<-mort_data$Mortality.Rate[active_policies[1,]]


'plotting customer mortality experience vs lumarian mortality'

plot(active_policies[1,],active_policies[4,], col = "Green", xlab = "Age", ylab = "Mortality Rate", type = "l")
points(active_policies[1,],active_policies[5,], col = "Red", type = "l")


plot(active_policies[1,9:40],active_policies[4,9:40], col = "Green", xlab = "Age", ylab = "Mortality Rate", type = "l")
points(active_policies[1,9:40],active_policies[5,9:40], col = "Red", type = "l")


age_type_table<-matrix(NA,6,62, dimnames = list(c("SmokeM","SmokeH","NSvL","NSL","NSM","NSH"),26:87))
age_type_death<-matrix(NA,6,62, dimnames = list(c("SmokeM","SmokeH","NSvL","NSL","NSM","NSH"),26:87))
age_type_mortality<-matrix(NA,6,62, dimnames = list(c("SmokeM","SmokeH","NSvL","NSL","NSM","NSH"),26:87))
check_active_type<-function(x,smoker,class){
  sum(data$Issue.age<=x & data$End.age>=x & data$Smoker.Status==smoker & data$Underwriting.Class==class)
}
age_type_table[1,]<-sapply(colnames(age_type_table),check_active_type,smoker = "S",class = "moderate risk")
age_type_table[2,]<-sapply(colnames(age_type_table),check_active_type,smoker = "S",class = "high risk")
age_type_table[3,]<-sapply(colnames(age_type_table),check_active_type,smoker = "NS",class = "very low risk")
age_type_table[4,]<-sapply(colnames(age_type_table),check_active_type,smoker = "NS",class = "low risk")
age_type_table[5,]<-sapply(colnames(age_type_table),check_active_type,smoker = "NS",class = "moderate risk")
age_type_table[6,]<-sapply(colnames(age_type_table),check_active_type,smoker = "NS",class = "high risk")
check_death_type<-function(x,smoker,class){
  sum(data$End.age==x & data$Smoker.Status==smoker & data$Underwriting.Class==class & data$Death.indicator==1)
}
age_type_death[1,]<-sapply(colnames(age_type_death),check_death_type,smoker = "S",class = "moderate risk")
age_type_death[2,]<-sapply(colnames(age_type_death),check_death_type,smoker = "S",class = "high risk")
age_type_death[3,]<-sapply(colnames(age_type_death),check_death_type,smoker = "NS",class = "very low risk")
age_type_death[4,]<-sapply(colnames(age_type_death),check_death_type,smoker = "NS",class = "low risk")
age_type_death[5,]<-sapply(colnames(age_type_death),check_death_type,smoker = "NS",class = "moderate risk")
age_type_death[6,]<-sapply(colnames(age_type_death),check_death_type,smoker = "NS",class = "high risk")
'mortality experience separated by smoking status and underwriting class'
age_type_mortality<-age_type_death/age_type_table
'e.g. plot, limit ages for more difference'
plot(colnames(age_type_mortality), age_type_mortality[1,], type = "l")
