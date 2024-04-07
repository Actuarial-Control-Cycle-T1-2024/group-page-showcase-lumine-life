library(openxlsx)
#Find differences in death benefits over previous 20 year period
data<-read.csv("data/inforce-data-cleaned.csv")
historical_interest<-read.xlsx("soa-materials/srcsc-2024-lumaria-economic-data.xlsx", startRow = 12)
#T20 Lapse Rate found from lapse-rates.R
lapse_rate<-0.01
data$End.year<-pmin(2023,data$Year.of.Death,data$Year.of.Lapse, na.rm = TRUE)
data$Policy.exposure<-data$End.year-data$Issue.year+1
data$End.age<-data$End.year-data$Issue.year + data$Issue.age
#Restrict data to last 20 years
data<-data[data$End.year>=2004,]
#Find value at end of 2023 of 1 crown at the start of year "year"
find_current_value<-function(year){
  year_index<-which(historical_interest$Year==year)
  cv<-1
  if(year <= 2023){
    for(x in year_index:62){
      cv<-cv*(1+historical_interest$Inflation[x])
    }
  }
  cv
}
#CV of historical costs for program implemented in past, assumed program costs occur at start of year
data$Program.Cost.Index<-sapply(pmax(2003,data$Issue.year),find_current_value)
#Filter deaths to find total death benefit paid in past 20 years
death_data<-data[data$Death.indicator==1,]
#Death benefit paid at end of year, equivalent to start of the next year
death_data$Crown.value<-sapply(death_data$Year.of.Death+1,find_current_value)
#CV of benefits paid
death_data$CVBenefit<-death_data$Crown.value*death_data$Face.amount
current_payout<-sum(death_data$CVBenefit)


#Inserting pre-intervention mortality models to compare model with actual results
SPWL_F_NS<-read.xlsx("data/base-life-tables.xlsx", sheet = 1)
SPWL_F_S<-read.xlsx("data/base-life-tables.xlsx", sheet = 2)
SPWL_M_NS<-read.xlsx("data/base-life-tables.xlsx", sheet = 3)
SPWL_M_S<-read.xlsx("data/base-life-tables.xlsx", sheet = 4)
T20_F_NS<-read.xlsx("data/base-life-tables.xlsx", sheet = 5)
T20_F_S<-read.xlsx("data/base-life-tables.xlsx", sheet = 6)
T20_M_NS<-read.xlsx("data/base-life-tables.xlsx", sheet = 7)
T20_M_S<-read.xlsx("data/base-life-tables.xlsx", sheet = 8)
#Function to calculate CV of expected death benefits for each policy
x_payout<-function(input){
  #Death Benefit CV
  x<-0
  #Chance of survival
  px<-1
  current_year<-as.numeric(input["Issue.year"])
  current_age<-as.numeric(input["Issue.age"])
  #Life table selection
  current_table<-get(paste(input["Policy.type"], input["Sex"], input["Smoker.Status"], sep = "_"))
  current_index<-which(current_table$age == current_age)
  repeat{
    #Assuming identical experience to real outcome before start of 20 yr period
    #So all lives survive to start of 2004
    if(current_year>=2004){
      #Probability of death in current year
      qx<-current_table[current_index,]$qx*px
      #Add Expected Payout of current year to total
      x<-x+qx*as.numeric(input["Face.amount"])*find_current_value(current_year+1)
      #Proportion of policies surviving to next year
      px<-px*current_table[current_index,]$px
      if(input["Policy.type"] == "T20"){
        px<-px*(1-lapse_rate)
      }
    }
    current_year<-current_year + 1
    current_age<-current_age + 1
    current_index<-current_index + 1
    #Up to current time
    if(current_year>2023){
      break
    }
    #20 Year term insurance ending
    if(current_age - as.numeric(input["Issue.age"]) >=20 & input["Policy.type"] == "T20") {
      break
    }
  }
  return(x)
}
#Calculating expected benefits for each policy and total for all policies
#Fairly close to real data increasing validity of model
data$Expected.loss<-apply(data,1,x_payout)
base_x_loss<-sum(data$Expected.loss)

#Loading post-intervention life tables to compare benefits paid
SPWL_F_NS<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 1)
SPWL_F_S_34<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 2)
SPWL_F_S_44<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 3)
SPWL_F_S_54<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 4)
SPWL_F_S_64<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 5)
SPWL_M_NS<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 6)
SPWL_M_S_34<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 7)
SPWL_M_S_44<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 8)
SPWL_M_S_54<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 9)
SPWL_M_S_64<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 10)
T20_F_NS<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 11)
T20_F_S_34<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 12)
T20_F_S_44<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 13)
T20_F_S_54<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 14)
T20_F_S_64<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 15)
T20_M_NS<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 16)
T20_M_S_34<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 17)
T20_M_S_44<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 18)
T20_M_S_54<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 19)
T20_M_S_64<-read.xlsx("data/adjusted-life-tables.xlsx", sheet = 20)
#Function to calculate death benefits after interventions
x_payout_adjusted<-function(input){
  x<-0
  px<-1
  current_year<-as.numeric(input["Issue.year"])
  current_age<-as.numeric(input["Issue.age"])
  #Age at quitting smoking affects life table choice for smokers
  if(input["Smoker.Status"] == "NS"){
    current_table<-get(paste(input["Policy.type"], input["Sex"], input["Smoker.Status"], sep = "_"))
  } else {
    if(current_age <= 34){
      age_group<-34
    } else if(current_age <= 44) {
      age_group<-44
    } else if (current_age <=54) {
      age_group<-54
    } else {
      age_group<-64
    }
    current_table<-get(paste(input["Policy.type"], input["Sex"], input["Smoker.Status"], age_group, sep = "_"))

  }
  current_index<-which(current_table$age == current_age)
  repeat{
    #Assuming identical experience before start of 20 yr period
    if(current_year>=2004){
      #Probability of death in current year
      qx<-current_table[current_index,]$qx*px
      #Add Expected Payout of current year to total
      x<-x+qx*as.numeric(input["Face.amount"])*find_current_value(current_year+1)
      #Proportion of policies surviving to next year
      px<-px*current_table[current_index,]$px
      if(input["Policy.type"] == "T20"){
        px<-px*(1-lapse_rate)
      }
    }
    current_year<-current_year + 1
    current_age<-current_age + 1
    current_index<-current_index + 1
    #Up to current time
    if(current_year>2023){
      break
    }
    #20 Year term insurance ending
    if(current_age - as.numeric(input["Issue.age"]) >=20 & input["Policy.type"] == "T20") {
      break
    }
  }
  return(x)
}
#Find expected benefits after intervention
data$adjExpected.loss<-apply(data,1,x_payout_adjusted)
adjusted_x_loss<-sum(data$adjExpected.loss)
#Compare benefits paid before and after to find mortality savings
mortality_savings<-base_x_loss - adjusted_x_loss
#Find $ at which the mortality savings exceed costs of programs
breakeven_point<-mortality_savings/sum(data$Program.Cost.Index)

