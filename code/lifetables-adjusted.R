library(openxlsx)
#Load pre-intervention life tables to adjust
SPWL_F_NS<-read.xlsx("data/base-life-tables.xlsx", sheet = 1)
SPWL_F_S<-read.xlsx("data/base-life-tables.xlsx", sheet = 2)
SPWL_M_NS<-read.xlsx("data/base-life-tables.xlsx", sheet = 3)
SPWL_M_S<-read.xlsx("data/base-life-tables.xlsx", sheet = 4)
T20_F_NS<-read.xlsx("data/base-life-tables.xlsx", sheet = 5)
T20_F_S<-read.xlsx("data/base-life-tables.xlsx", sheet = 6)
T20_M_NS<-read.xlsx("data/base-life-tables.xlsx", sheet = 7)
T20_M_S<-read.xlsx("data/base-life-tables.xlsx", sheet = 8)
#Hazard rates for male and female smokers by: never smoking, quit before 35,45,55,65, current smoker
m_rates<-c(1,1.01,1.17,1.44,1.68,2.7)
f_rates<-c(1,1.014,1.28,1.53,1.83,2.9)
#Breast cancer mortality reduction after intervention
b_cancer<-0.007
#Colorectal cancer mortality reduction after intervention
c_cancer<-0.005

#Apply appropriate interventions to each lifetable and form new tables
SPWL_F_NS_a<-SPWL_F_NS
SPWL_F_NS_a$qx<-SPWL_F_NS$qx*(1-b_cancer)*(1-c_cancer)
SPWL_F_NS_a$px<-1-SPWL_F_NS_a$qx
SPWL_F_S$qx<-SPWL_F_S$qx*(1-b_cancer)*(1-c_cancer)
SPWL_F_S34<-SPWL_F_S
SPWL_F_S34$qx<-SPWL_F_NS_a$qx*(f_rates[6]-f_rates[2])/(f_rates[6]-f_rates[1]) +
  SPWL_F_S$qx*(f_rates[2]-f_rates[1])/(f_rates[6]-f_rates[1])
SPWL_F_S34$px<- 1 - SPWL_F_S34$qx
SPWL_F_S44<-SPWL_F_S
SPWL_F_S44$qx<-SPWL_F_NS_a$qx*(f_rates[6]-f_rates[3])/(f_rates[6]-f_rates[1]) +
  SPWL_F_S$qx*(f_rates[3]-f_rates[1])/(f_rates[6]-f_rates[1])
SPWL_F_S44$px<- 1 - SPWL_F_S44$qx
SPWL_F_S54<-SPWL_F_S
SPWL_F_S54$qx<-SPWL_F_NS_a$qx*(f_rates[6]-f_rates[4])/(f_rates[6]-f_rates[1]) +
  SPWL_F_S$qx*(f_rates[4]-f_rates[1])/(f_rates[6]-f_rates[1])
SPWL_F_S54$px<- 1 - SPWL_F_S54$qx
SPWL_F_S64<-SPWL_F_S
SPWL_F_S64$qx<-SPWL_F_NS_a$qx*(f_rates[6]-f_rates[5])/(f_rates[6]-f_rates[1]) +
  SPWL_F_S$qx*(f_rates[5]-f_rates[1])/(f_rates[6]-f_rates[1])
SPWL_F_S64$px<- 1 - SPWL_F_S64$qx

SPWL_M_NS_a<-SPWL_M_NS
SPWL_M_NS_a$qx<-SPWL_M_NS$qx*(1-c_cancer)
SPWL_M_NS_a$px<-1-SPWL_M_NS_a$qx
SPWL_M_S$qx<-SPWL_M_S$qx*(1-c_cancer)
SPWL_M_S34<-SPWL_M_S
SPWL_M_S34$qx<-SPWL_M_NS_a$qx*(m_rates[6]-m_rates[2])/(m_rates[6]-m_rates[1]) +
  SPWL_M_S$qx*(m_rates[2]-m_rates[1])/(m_rates[6]-m_rates[1])
SPWL_M_S34$px<- 1 - SPWL_M_S34$qx
SPWL_M_S44<-SPWL_M_S
SPWL_M_S44$qx<-SPWL_M_NS_a$qx*(m_rates[6]-m_rates[3])/(m_rates[6]-m_rates[1]) +
  SPWL_M_S$qx*(m_rates[3]-m_rates[1])/(m_rates[6]-m_rates[1])
SPWL_M_S44$px<- 1 - SPWL_M_S44$qx
SPWL_M_S54<-SPWL_M_S
SPWL_M_S54$qx<-SPWL_M_NS_a$qx*(m_rates[6]-m_rates[4])/(m_rates[6]-m_rates[1]) +
  SPWL_M_S$qx*(m_rates[4]-m_rates[1])/(m_rates[6]-m_rates[1])
SPWL_M_S54$px<- 1 - SPWL_M_S54$qx
SPWL_M_S64<-SPWL_M_S
SPWL_M_S64$qx<-SPWL_M_NS_a$qx*(m_rates[6]-m_rates[5])/(m_rates[6]-m_rates[1]) +
  SPWL_M_S$qx*(m_rates[5]-m_rates[1])/(m_rates[6]-m_rates[1])
SPWL_M_S64$px<- 1 - SPWL_M_S64$qx

T20_F_NS_a<-T20_F_NS
T20_F_NS_a$qx<-T20_F_NS$qx*(1-b_cancer)*(1-c_cancer)
T20_F_NS_a$px<-1-T20_F_NS_a$qx
T20_F_S$qx<-T20_F_S$qx*(1-b_cancer)*(1-c_cancer)
T20_F_S34<-T20_F_S
T20_F_S34$qx<-T20_F_NS_a$qx*(f_rates[6]-f_rates[2])/(f_rates[6]-f_rates[1]) +
  T20_F_S$qx*(f_rates[2]-f_rates[1])/(f_rates[6]-f_rates[1])
T20_F_S34$px<- 1 - T20_F_S34$qx
T20_F_S44<-T20_F_S
T20_F_S44$qx<-T20_F_NS_a$qx*(f_rates[6]-f_rates[3])/(f_rates[6]-f_rates[1]) +
  T20_F_S$qx*(f_rates[3]-f_rates[1])/(f_rates[6]-f_rates[1])
T20_F_S44$px<- 1 - T20_F_S44$qx
T20_F_S54<-T20_F_S
T20_F_S54$qx<-T20_F_NS_a$qx*(f_rates[6]-f_rates[4])/(f_rates[6]-f_rates[1]) +
  T20_F_S$qx*(f_rates[4]-f_rates[1])/(f_rates[6]-f_rates[1])
T20_F_S54$px<- 1 - T20_F_S54$qx
T20_F_S64<-T20_F_S
T20_F_S64$qx<-T20_F_NS_a$qx*(f_rates[6]-f_rates[5])/(f_rates[6]-f_rates[1]) +
  T20_F_S$qx*(f_rates[5]-f_rates[1])/(f_rates[6]-f_rates[1])
T20_F_S64$px<- 1 - T20_F_S64$qx

T20_M_NS_a<-T20_M_NS
T20_M_NS_a$qx<-T20_M_NS$qx*(1-c_cancer)
T20_M_NS_a$px<-1-T20_M_NS_a$qx
T20_M_S$qx<-T20_M_S$qx*(1-c_cancer)
T20_M_S34<-T20_M_S
T20_M_S34$qx<-T20_M_NS_a$qx*(m_rates[6]-m_rates[2])/(m_rates[6]-m_rates[1]) +
  T20_M_S$qx*(m_rates[2]-m_rates[1])/(m_rates[6]-m_rates[1])
T20_M_S34$px<- 1 - T20_M_S34$qx
T20_M_S44<-T20_M_S
T20_M_S44$qx<-T20_M_NS_a$qx*(m_rates[6]-m_rates[3])/(m_rates[6]-m_rates[1]) +
  T20_M_S$qx*(m_rates[3]-m_rates[1])/(m_rates[6]-m_rates[1])
T20_M_S44$px<- 1 - T20_M_S44$qx
T20_M_S54<-T20_M_S
T20_M_S54$qx<-T20_M_NS_a$qx*(m_rates[6]-m_rates[4])/(m_rates[6]-m_rates[1]) +
  T20_M_S$qx*(m_rates[4]-m_rates[1])/(m_rates[6]-m_rates[1])
T20_M_S54$px<- 1 - T20_M_S54$qx
T20_M_S64<-T20_M_S
T20_M_S64$qx<-T20_M_NS_a$qx*(m_rates[6]-m_rates[5])/(m_rates[6]-m_rates[1]) +
  T20_M_S$qx*(m_rates[5]-m_rates[1])/(m_rates[6]-m_rates[1])
T20_M_S64$px<- 1 - T20_M_S64$qx
#Storing life tables after intervention
write.xlsx(list("SPWL_F_NS" = SPWL_F_NS_a, "SPWL_F_S34" = SPWL_F_S34,
                "SPWL_F_S44" = SPWL_F_S44,"SPWL_F_S54" = SPWL_F_S54,
                "SPWL_F_S64" = SPWL_F_S64, "SPWL_M_NS" = SPWL_M_NS_a,
                "SPWL_M_S34" = SPWL_M_S34,"SPWL_M_S44" = SPWL_M_S44, 
                "SPWL_M_S54" = SPWL_M_S54,"SPWL_M_S64" = SPWL_M_S64,
                "T20_F_NS" = T20_F_NS_a, "T20_F_S34" = T20_F_S34,
                "T20_F_S44" = T20_F_S44, "T20_F_S54" = T20_F_S54,
                "T20_F_S64" = T20_F_S64,"T20_M_NS" = T20_M_NS_a, 
                "T20_M_S34" = T20_M_S34,"T20_M_S44" = T20_M_S44,
                "T20_M_S54" = T20_M_S54,"T20_M_S64" = T20_M_S64), file = "data/adjusted-life-tables.xlsx")

