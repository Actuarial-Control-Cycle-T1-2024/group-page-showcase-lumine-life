library(readr)
library(MortalityTables)
library(lifecontingencies)
library(openxlsx)
#Loading raw data for mortality by age and category
age_type_table<-as.matrix(read_csv("raw-exposures.csv"))
age_type_mortality<-as.matrix(read_csv("raw-mortality-data.csv"))


#Using Whittaker-Henderson smoothing to smooth raw life tables
temp_table<-mortalityTable.observed(deathProbs = as.data.frame(c(age_type_mortality[1,10:62],NA)), years = 0, ages = 35:120)
temp_table2<-getPeriodTable(temp_table)
temp_table2@exposures<-c(age_type_table[1,],NA)
temp_table2<-whittaker.mortalityTable(temp_table2, weights = temp_table2@exposures)
expanded<-mT.extrapolateProbsExp(temp_table2,age = 87, up = TRUE)
expanded@deathProbs[expanded@deathProbs>1]<-1
temp_table3<-lifeTable(expanded)
temp_table3@x<-35:121
SPWL_Female_NS<-data.frame("age" = 35:120, "qx" = qxt(temp_table3,35:120,1), "px" = pxt(temp_table3,35:120,1))
rm(temp_table)
rm(temp_table2)
rm(expanded)
rm(temp_table3)


temp_table<-mortalityTable.observed(deathProbs = as.data.frame(c(age_type_mortality[2,10:62],NA)), years = 0, ages = 35:120)
temp_table2<-getPeriodTable(temp_table)
temp_table2@exposures<-c(age_type_table[2,],NA)
temp_table2<-whittaker.mortalityTable(temp_table2, weights = temp_table2@exposures)
expanded<-mT.extrapolateProbsExp(temp_table2,age = 85, up = TRUE)
expanded<-mT.extrapolateProbsExp(expanded,age = 41, up = FALSE)
expanded@deathProbs[expanded@deathProbs>1]<-1
temp_table3<-lifeTable(expanded)
temp_table3@x<-35:90
SPWL_Female_S<-data.frame("age" = 35:120, "qx" = qxt(temp_table3,35:120,1), "px" = pxt(temp_table3,35:120,1))
rm(temp_table)
rm(temp_table2)
rm(expanded)
rm(temp_table3)

temp_table<-mortalityTable.observed(deathProbs = as.data.frame(c(age_type_mortality[3,10:62],NA)), years = 0, ages = 35:120)
temp_table2<-getPeriodTable(temp_table)
temp_table2@exposures<-c(age_type_table[3,],NA)
temp_table2<-whittaker.mortalityTable(temp_table2, weights = temp_table2@exposures)
expanded<-mT.extrapolateProbsExp(temp_table2,age = 87, up = TRUE)
expanded@deathProbs[expanded@deathProbs>1]<-1
temp_table3<-lifeTable(expanded)
temp_table3@x<-35:121
SPWL_Male_NS<-data.frame("age" = 35:120, "qx" = qxt(temp_table3,35:120,1), "px" = pxt(temp_table3,35:120,1))
rm(temp_table)
rm(temp_table2)
rm(expanded)
rm(temp_table3)

temp_table<-mortalityTable.observed(deathProbs = as.data.frame(c(age_type_mortality[4,10:62],NA)), years = 0, ages = 35:120)
temp_table2<-getPeriodTable(temp_table)
temp_table2@exposures<-c(age_type_table[4,],NA)
temp_table2<-whittaker.mortalityTable(temp_table2, weights = temp_table2@exposures)
expanded<-mT.extrapolateProbsExp(temp_table2,age = 87, up = TRUE)
expanded@deathProbs[expanded@deathProbs>1]<-1
temp_table3<-lifeTable(expanded)
temp_table3@x<-35:90
SPWL_Male_S<-data.frame("age" = 35:120, "qx" = qxt(temp_table3,35:120,1), "px" = pxt(temp_table3,35:120,1))
rm(temp_table)
rm(temp_table2)
rm(expanded)
rm(temp_table3)

temp_table<-mortalityTable.observed(deathProbs = as.data.frame(c(age_type_mortality[5,1:49],NA)), years = 0, ages = 26:74)
temp_table2<-getPeriodTable(temp_table)
temp_table2@exposures<-c(age_type_table[5,1:49])
temp_table2<-whittaker.mortalityTable(temp_table2, weights = temp_table2@exposures)
temp_table3<-lifeTable(temp_table2)
temp_table3@x<-26:75
T20_Female_NS<-data.frame("age" = 26:74, "qx" = qxt(temp_table3,26:74,1), "px" = pxt(temp_table3,26:74,1))
rm(temp_table)
rm(temp_table2)
rm(temp_table3)

temp_table<-mortalityTable.observed(deathProbs = as.data.frame(c(age_type_mortality[6,1:49],NA)), years = 0, ages = 26:74)
temp_table2<-getPeriodTable(temp_table)
temp_table2@exposures<-c(age_type_table[6,1:49])
temp_table2<-whittaker.mortalityTable(temp_table2, weights = temp_table2@exposures)
temp_table3<-lifeTable(temp_table2)
temp_table3@x<-26:74
T20_Female_S<-data.frame("age" = 26:74, "qx" = qxt(temp_table3,26:74,1), "px" = pxt(temp_table3,26:74,1))
rm(temp_table)
rm(temp_table2)
rm(temp_table3)

temp_table<-mortalityTable.observed(deathProbs = as.data.frame(c(age_type_mortality[7,1:49],NA)), years = 0, ages = 26:74)
temp_table2<-getPeriodTable(temp_table)
temp_table2@exposures<-c(age_type_table[7,1:49])
temp_table2<-whittaker.mortalityTable(temp_table2, weights = temp_table2@exposures)
temp_table3<-lifeTable(temp_table2)
temp_table3@x<-26:75
T20_Male_NS<-data.frame("age" = 26:74, "qx" = qxt(temp_table3,26:74,1), "px" = pxt(temp_table3,26:74,1))
rm(temp_table)
rm(temp_table2)
rm(temp_table3)

temp_table<-mortalityTable.observed(deathProbs = as.data.frame(c(age_type_mortality[8,1:49],NA)), years = 0, ages = 26:74)
temp_table2<-getPeriodTable(temp_table)
temp_table2@exposures<-c(age_type_table[8,1:49])
temp_table2<-whittaker.mortalityTable(temp_table2, weights = temp_table2@exposures)
temp_table3<-lifeTable(temp_table2)
temp_table3@x<-26:75
T20_Male_S<-data.frame("age" = 26:74, "qx" = qxt(temp_table3,26:74,1), "px" = pxt(temp_table3,26:74,1))
rm(temp_table)
rm(temp_table2)
rm(temp_table3)

write.xlsx(list("SPWL_Female_NS" = SPWL_Female_NS, "SPWL_Female_S" = SPWL_Female_S, "SPWL_Male_NS" = SPWL_Male_NS,
                "SPWL_Male_S" = SPWL_Male_S, "T20_Female_NS" = T20_Female_NS, "T20_Female_S" = T20_Female_S,
                "T20_Male_NS" = T20_Male_NS, "T20_Male_S" = T20_Male_S), file = "data/base-life-tables.xlsx")


