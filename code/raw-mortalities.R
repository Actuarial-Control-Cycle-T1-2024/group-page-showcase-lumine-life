library(readr)
library(openxlsx)
data<-read_csv("data/inforce-data-cleaned.csv")
data[c(2,5,7,8,9,10,11,12,14,16)]<-lapply(data[c(2,5,7,8,9,10,11,12,14,16)], factor)

data$End.year<-pmin(2023,data$Year.of.Death,data$Year.of.Lapse, na.rm = TRUE)
data$Policy.exposure<-data$End.year-data$Issue.year+1
data$End.age<-data$End.year-data$Issue.year + data$Issue.age

#Tables for total lives, death and 1-year mortality in each category at each age
age_type_table<-matrix(NA,8,62, dimnames = list(c("SPWL_Female_NS","SPWL_Female_S","SPWL_Male_NS","SPWL_Male_S","T20_Female_NS","T20_Female_S", "T20_Male_NS", "T20_Male_S"),26:87))
age_type_death<-matrix(NA,8,62, dimnames = list(c("SPWL_Female_NS","SPWL_Female_S","SPWL_Male_NS","SPWL_Male_S","T20_Female_NS","T20_Female_S", "T20_Male_NS", "T20_Male_S"),26:87))
age_type_mortality<-matrix(NA,8,62, dimnames = list(c("SPWL_Female_NS","SPWL_Female_S","SPWL_Male_NS","SPWL_Male_S","T20_Female_NS","T20_Female_S", "T20_Male_NS", "T20_Male_S"),26:87))
#Checking number of active policies at each age and category
check_active_type<-function(x,type,sex,smoker){
  sum(data$Issue.age<=x & data$End.age>=x & data$Smoker.Status==smoker & data$Sex == sex & data$Policy.type == type)
}
age_type_table[1,]<-sapply(colnames(age_type_table),check_active_type,smoker = "NS",sex = "F", type = "SPWL")
age_type_table[2,]<-sapply(colnames(age_type_table),check_active_type,smoker = "S",sex = "F", type = "SPWL")
age_type_table[3,]<-sapply(colnames(age_type_table),check_active_type,smoker = "NS",sex = "M", type = "SPWL")
age_type_table[4,]<-sapply(colnames(age_type_table),check_active_type,smoker = "S",sex = "M", type = "SPWL")
age_type_table[5,]<-sapply(colnames(age_type_table),check_active_type,smoker = "NS",sex = "F", type = "T20")
age_type_table[6,]<-sapply(colnames(age_type_table),check_active_type,smoker = "S",sex = "F", type = "T20")
age_type_table[7,]<-sapply(colnames(age_type_table),check_active_type,smoker = "NS",sex = "M", type = "T20")
age_type_table[8,]<-sapply(colnames(age_type_table),check_active_type,smoker = "S",sex = "M", type = "T20")
#Checking number of deaths for each age and category
check_death_type<-function(x,type,sex,smoker){
  sum(data$End.age==x & data$Smoker.Status==smoker & data$Policy.type==type & data$Sex == sex & data$Death.indicator==1)
}
age_type_death[1,]<-sapply(colnames(age_type_death),check_death_type,smoker = "NS",sex = "F", type = "SPWL")
age_type_death[2,]<-sapply(colnames(age_type_death),check_death_type,smoker = "S",sex = "F", type = "SPWL")
age_type_death[3,]<-sapply(colnames(age_type_death),check_death_type,smoker = "NS",sex = "M", type = "SPWL")
age_type_death[4,]<-sapply(colnames(age_type_death),check_death_type,smoker = "S",sex = "M", type = "SPWL")
age_type_death[5,]<-sapply(colnames(age_type_death),check_death_type,smoker = "NS",sex = "F", type = "T20")
age_type_death[6,]<-sapply(colnames(age_type_death),check_death_type,smoker = "S",sex = "F", type = "T20")
age_type_death[7,]<-sapply(colnames(age_type_death),check_death_type,smoker = "NS",sex = "M", type = "T20")
age_type_death[8,]<-sapply(colnames(age_type_death),check_death_type,smoker = "S",sex = "M", type = "T20")

#Mortality experience separated by smoking status, age and sex 
age_type_mortality<-age_type_death/age_type_table
#Storing raw data in tables to use for life tables
write.csv(as.data.frame(age_type_mortality), row.names = F, file = "raw-mortality-data.csv")
write.csv(as.data.frame(age_type_table), row.names = F, file = "raw-exposures.csv")
write.csv(as.data.frame(age_type_death), row.names = F, file = "raw-deaths.csv")





