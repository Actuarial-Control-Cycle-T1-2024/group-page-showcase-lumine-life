library(readr)
library(readxl)
library(dplyr)
library(purrr)
#### +-20%
#### worst: high lapse, high cost, low interest
#### best: low lapse, low cost, high interest
#
#### BEST CASE


# assumptions -------------------------------------------------------------

assumption_lapse <- 0.01 * 0.8
assumption_reserve_T20_S <- 0.35
assumption_reserve_T20_NS <- 0.06

assumption_smoking_cost <- 2290.3 * 0.8 # not in premium, 1yr free, 3yr 50%
assumption_smoking_ratio <- 0.5
assumption_cancer_cost <- 348.6 * 0.8 # in premium

# read in -----------------------------------------------------------------

## policyholder (customer base)
PH_existing <- read_csv("data/end-of-2023-inforce.csv") |> select(Policy.type, Issue.year, Issue.age, Sex, Face.amount, Smoker.Status)
PH_future <- read_csv("data/Combined_Future_20Y.csv", col_types = cols(...1 = col_skip()))

## discount rate
i_discount <- read_csv("data/inflation-rates.csv") |> select(year, rate)
i_discount[nrow(i_discount), 1] <- "2029" # start year of long term
i_discount$year <- as.numeric(i_discount$year)
# 5 short term + 15 long term and so on
# 120 lifetime
i_discount <- i_discount |> 
  bind_rows(i_discount[rep(nrow(i_discount), 14+121), ] |> 
              mutate(year = year + seq(14+121)))

#### BEST CASE
i_discount <- i_discount |> mutate(rate = rate * 1.2)

# functions ---------------------------------------------------------------

WL_techprem_perDB <- function(issue_year, issue_age, ratetable, lifetable) {
  
  # read in
  rate <- ratetable |> filter(year>=issue_year) |> pull(rate)
  qx <- lifetable |> filter(age>=issue_age) |> pull(qx)
  px <- lifetable |> filter(age>=issue_age) |> pull(px)
  
  # consolidate length
  minlen <- min(length(rate), length(qx))
  rate <- rate[1:minlen]
  qx <- qx[1:minlen]
  px <- px[1:minlen]
  
  # ASSUME premium @ start of year
  
  # expected outflow
  # ASSUME death benefit @ end of year
  ex_out <- sum(cumprod((1+rate)^(-1)) * qx * c(1,cumprod(px)[1:(minlen-1)]))
  
  # single premium
  prem <- ex_out
  
  # return
  return(prem)
}

WL_cancerprem_percost <- function(issue_year, issue_age, ratetable, lifetable) {
  
  # read in
  rate <- ratetable |> filter(year>=issue_year) |> pull(rate)
  qx <- lifetable |> filter(age>=issue_age) |> pull(qx)
  px <- lifetable |> filter(age>=issue_age) |> pull(px)
  
  # consolidate length
  minlen <- min(length(rate), length(qx))
  rate <- rate[1:minlen]
  qx <- qx[1:minlen]
  px <- px[1:minlen]
  
  # expected cost @ $1
  # ASSUME cost @ start of year
  ex_cost <- sum(c(1,cumprod((1+rate)^(-1))[1:(minlen-1)]) * c(1,cumprod(px)[1:(minlen-1)]))
  
  # return
  return(ex_cost)
}

T20_techprem_perDB <- function(issue_year, issue_age, ratetable, lifetable) {
  
  # read in
  rate <- ratetable |> filter(year>=issue_year) |> pull(rate)
  qx <- lifetable |> filter(age>=issue_age) |> slice(1:20) |> pull(qx)
  px <- lifetable |> filter(age>=issue_age) |> slice(1:20) |> pull(px)
  
  # consolidate length
  minlen <- min(length(rate), length(qx))
  rate <- rate[1:minlen]
  qx <- qx[1:minlen]
  px <- px[1:minlen]
  
  # expected inflow @ $1
  # ASSUME premium @ start of year
  ex_in <- sum(c(1,cumprod((1+rate)^(-1))[1:(minlen-1)]) * c(1,cumprod(px)[1:(minlen-1)]))
  
  # expected outflow
  # ASSUME death benefit @ end of year
  ex_out <- sum(cumprod((1+rate)^(-1)) * qx * c(1,cumprod(px)[1:(minlen-1)]))
  
  # premium
  prem <- ex_out / ex_in
  
  # return
  return(prem)
}

T20_cancerprem_percost <- function(issue_year, issue_age, ratetable, lifetable) {
  
  # read in
  rate <- ratetable |> filter(year>=issue_year) |> pull(rate)
  qx <- lifetable |> filter(age>=issue_age) |> slice(1:20) |> pull(qx)
  px <- lifetable |> filter(age>=issue_age) |> slice(1:20) |> pull(px)
  
  # consolidate length
  minlen <- min(length(rate), length(qx))
  rate <- rate[1:minlen]
  qx <- qx[1:minlen]
  px <- px[1:minlen]
  
  # expected cost @ $1
  # ASSUME cost @ start of year
  ex_cost <- sum(c(1,cumprod((1+rate)^(-1))[1:(minlen-1)]) * c(1,cumprod(px)[1:(minlen-1)]))
  
  # return
  return(ex_cost)
}

T20_res <- function(prem, db, start_year, year_left, now_age, ratetable, lifetable) {
  
  # read in
  rate <- ratetable |> filter(year>=start_year) |> pull(rate)
  qx <- lifetable |> filter(age>=now_age) |> slice_head(n = year_left) |> pull(qx)
  px <- lifetable |> filter(age>=now_age) |> slice_head(n = year_left) |> pull(px)
  
  # consolidate length
  minlen <- min(length(rate), length(qx))
  rate <- rate[1:minlen]
  qx <- qx[1:minlen]
  px <- px[1:minlen]
  
  # EPV future outflow
  ex_out <- sum(cumprod((1+rate)^(-1)) * qx * c(1,cumprod(px)[1:(minlen-1)]))
  
  # EPV future inflow
  ex_in <- sum(c(1,cumprod((1+rate)^(-1))[1:(minlen-1)]) * c(1,cumprod(px)[1:(minlen-1)]))
  
  # reserve
  reserve <- db * ex_out - prem * ex_in
  
  # return
  return(reserve)
  
}

# with intervention program ----------------------------------

## lifetable
LT_SPWL_F_NS <- read_excel("data/adjusted-life-tables.xlsx", sheet = "SPWL_F_NS")
LT_SPWL_F_S34 <- read_excel("data/adjusted-life-tables.xlsx", sheet = "SPWL_F_S34")
LT_SPWL_F_S44 <- read_excel("data/adjusted-life-tables.xlsx", sheet = "SPWL_F_S44")
LT_SPWL_F_S54 <- read_excel("data/adjusted-life-tables.xlsx", sheet = "SPWL_F_S54")
LT_SPWL_F_S64 <- read_excel("data/adjusted-life-tables.xlsx", sheet = "SPWL_F_S64")
LT_SPWL_M_NS <- read_excel("data/adjusted-life-tables.xlsx", sheet = "SPWL_M_NS")
LT_SPWL_M_S34 <- read_excel("data/adjusted-life-tables.xlsx", sheet = "SPWL_M_S34")
LT_SPWL_M_S44 <- read_excel("data/adjusted-life-tables.xlsx", sheet = "SPWL_M_S44")
LT_SPWL_M_S54 <- read_excel("data/adjusted-life-tables.xlsx", sheet = "SPWL_M_S54")
LT_SPWL_M_S64 <- read_excel("data/adjusted-life-tables.xlsx", sheet = "SPWL_M_S64")
LT_T20_F_NS <- read_excel("data/adjusted-life-tables.xlsx", sheet = "T20_F_NS")
LT_T20_F_S34 <- read_excel("data/adjusted-life-tables.xlsx", sheet = "T20_F_S34")
LT_T20_F_S44 <- read_excel("data/adjusted-life-tables.xlsx", sheet = "T20_F_S44")
LT_T20_F_S54 <- read_excel("data/adjusted-life-tables.xlsx", sheet = "T20_F_S54")
LT_T20_F_S64 <- read_excel("data/adjusted-life-tables.xlsx", sheet = "T20_F_S64")
LT_T20_M_NS <- read_excel("data/adjusted-life-tables.xlsx", sheet = "T20_M_NS")
LT_T20_M_S34 <- read_excel("data/adjusted-life-tables.xlsx", sheet = "T20_M_S34")
LT_T20_M_S44 <- read_excel("data/adjusted-life-tables.xlsx", sheet = "T20_M_S44")
LT_T20_M_S54 <- read_excel("data/adjusted-life-tables.xlsx", sheet = "T20_M_S54")
LT_T20_M_S64 <- read_excel("data/adjusted-life-tables.xlsx", sheet = "T20_M_S64")

## technical premium per $1 death benefit
techprem_perdb <- expand.grid(issue_year = min(PH_existing$Issue.year):max(PH_future$Issue.year),
                              issue_age = min(PH_existing$Issue.age):119,
                              policy_type = c("SPWL", "T20"),
                              sex = c("F", "M"),
                              smoker = c("NS", "S34", "S44", "S54", "S64")) |> 
  filter(!(issue_age>max(PH_future$Issue.age) & policy_type=="T20")) |> 
  mutate(lifetable = paste("LT", policy_type, sex, smoker, sep = "_")) |> 
  rowwise() |>
  mutate(techprem_perdb = case_when(
    policy_type=="SPWL" ~ WL_techprem_perDB(issue_year, issue_age, i_discount, get(lifetable)),
    policy_type=="T20" ~ T20_techprem_perDB(issue_year, issue_age, i_discount, get(lifetable)),
    .default = NA)
  ) |> 
  write_csv("data/techprem_perdb_intervention_best.csv")
### 19 minutes

## cancer awareness premium per $1 cost
cancerprem_percost <- expand.grid(issue_year = min(PH_existing$Issue.year):max(PH_future$Issue.year),
                                  issue_age = min(PH_existing$Issue.age):119,
                                  policy_type = c("SPWL", "T20"),
                                  sex = c("F", "M"),
                                  smoker = c("NS", "S34", "S44", "S54", "S64")) |> 
  filter(!(issue_age>max(PH_future$Issue.age) & policy_type=="T20")) |> 
  mutate(lifetable = paste("LT", policy_type, sex, smoker, sep = "_")) |> 
  rowwise() |>
  mutate(cancerprem_percost = case_when(
    policy_type=="SPWL" ~ WL_cancerprem_percost(issue_year, issue_age, i_discount, get(lifetable)),
    policy_type=="T20" ~ T20_cancerprem_percost(issue_year, issue_age, i_discount, get(lifetable)),
    .default = NA)
  ) |> 
  write_csv("data/cancerprem_percost_intervention_best.csv")
### 10 minutes

## decrement table (T20) with death and lapse
# LT_T20_F_NS
# LT_T20_F_S34
# LT_T20_F_S44
# LT_T20_F_S54
# LT_T20_F_S64
# LT_T20_M_NS
# LT_T20_M_S34
# LT_T20_M_S44
# LT_T20_M_S54
# LT_T20_M_S64
for (i in c("LT_T20_F_NS", "LT_T20_F_S34", "LT_T20_F_S44", "LT_T20_F_S54", "LT_T20_F_S64",
            "LT_T20_M_NS", "LT_T20_M_S34", "LT_T20_M_S44", "LT_T20_M_S54", "LT_T20_M_S64")) {
  name <- i # CHANGE
  print(paste("doing", i))
  death_table <- get(name) |> select(age, qx)
  mdt <- tibble(
    age = 26,
    death_prob = death_table$qx[1],
    lapse_prob = assumption_lapse, # ASSUME 1% from taking average
    lx = 1,
    death = lx * death_prob,
    lapse = lx * lapse_prob
  )
  for (i in 27:74) { # life table (T20) for ages 26 to 74
    mdt <- mdt |> 
      add_row(
        age = i,
        death_prob = death_table |> filter(age==i) |> pull(qx),
        lapse_prob = mdt$lapse_prob[nrow(mdt)],
        lx = mdt$lx[nrow(mdt)] - mdt$death[nrow(mdt)] - mdt$lapse[nrow(mdt)],
        death = death_prob * lx,
        lapse = min(lx - death, lapse_prob * lx)
      )
  }
  mdt |> write_csv(paste0("data/mdt", "_", name, "_intervention_best.csv"))
}


## decrement table (SPWL) with death and NO lapse
# LT_SPWL_F_NS
# LT_SPWL_F_S34
# LT_SPWL_F_S44
# LT_SPWL_F_S54
# LT_SPWL_F_S64
# LT_SPWL_M_NS
# LT_SPWL_M_S34
# LT_SPWL_M_S44
# LT_SPWL_M_S54
# LT_SPWL_M_S64
for (i in c("LT_SPWL_F_NS", "LT_SPWL_F_S34", "LT_SPWL_F_S44", "LT_SPWL_F_S54", "LT_SPWL_F_S64",
            "LT_SPWL_M_NS", "LT_SPWL_M_S34", "LT_SPWL_M_S44", "LT_SPWL_M_S54", "LT_SPWL_M_S64")) {
  name <- i # CHANGE
  print(paste("doing", i))
  death_table <- get(name) |> select(age, qx)
  mdt <- tibble(
    age = 35,
    death_prob = death_table$qx[1],
    lx = 1,
    death = lx * death_prob
  )
  for (i in 36:120) { # life table (SPWL) for ages 35 to 120
    mdt <- mdt |> 
      add_row(
        age = i,
        death_prob = death_table |> filter(age==i) |> pull(qx),
        lx = mdt$lx[nrow(mdt)] - mdt$death[nrow(mdt)],
        death = death_prob * lx
      )
  }
  mdt |> write_csv(paste0("data/mdt", "_", name, "_intervention_best.csv"))
}

# MAIN (with intervention) --------------------------------------------------------------------

techprem_perdb0 <- read_csv("data/techprem_perdb.csv")
techprem_perdb <- read_csv("data/techprem_perdb_intervention.csv")
cancerprem_percost <- read_csv("data/cancerprem_percost_intervention.csv")
mdt_LT_SPWL_F_NS <- read_csv("data/mdt_LT_SPWL_F_NS_intervention.csv")
mdt_LT_SPWL_F_S34 <- read_csv("data/mdt_LT_SPWL_F_S34_intervention.csv")
mdt_LT_SPWL_F_S44 <- read_csv("data/mdt_LT_SPWL_F_S44_intervention.csv")
mdt_LT_SPWL_F_S54 <- read_csv("data/mdt_LT_SPWL_F_S54_intervention.csv")
mdt_LT_SPWL_F_S64 <- read_csv("data/mdt_LT_SPWL_F_S64_intervention.csv")
mdt_LT_SPWL_M_NS <- read_csv("data/mdt_LT_SPWL_M_NS_intervention.csv")
mdt_LT_SPWL_M_S34 <- read_csv("data/mdt_LT_SPWL_M_S34_intervention.csv")
mdt_LT_SPWL_M_S44 <- read_csv("data/mdt_LT_SPWL_M_S44_intervention.csv")
mdt_LT_SPWL_M_S54 <- read_csv("data/mdt_LT_SPWL_M_S54_intervention.csv")
mdt_LT_SPWL_M_S64 <- read_csv("data/mdt_LT_SPWL_M_S64_intervention.csv")
mdt_LT_T20_F_NS <- read_csv("data/mdt_LT_T20_F_NS_intervention.csv")
mdt_LT_T20_F_S34 <- read_csv("data/mdt_LT_T20_F_S34_intervention.csv")
mdt_LT_T20_F_S44 <- read_csv("data/mdt_LT_T20_F_S44_intervention.csv")
mdt_LT_T20_F_S54 <- read_csv("data/mdt_LT_T20_F_S54_intervention.csv")
mdt_LT_T20_F_S64 <- read_csv("data/mdt_LT_T20_F_S64_intervention.csv")
mdt_LT_T20_M_NS <- read_csv("data/mdt_LT_T20_M_NS_intervention.csv")
mdt_LT_T20_M_S34 <- read_csv("data/mdt_LT_T20_M_S34_intervention.csv")
mdt_LT_T20_M_S44 <- read_csv("data/mdt_LT_T20_M_S44_intervention.csv")
mdt_LT_T20_M_S54 <- read_csv("data/mdt_LT_T20_M_S54_intervention.csv")
mdt_LT_T20_M_S64 <- read_csv("data/mdt_LT_T20_M_S64_intervention.csv")

for (i in min(PH_future$Issue.year):max(PH_future$Issue.year)) {
  year = i # <--- change years
  print(paste("doing", i))
  
  ## existing customers !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # old premium used
  scored_existing <- PH_existing |> 
    # keep only active ones
    mutate(active = case_when(
      Policy.type=="SPWL" ~ 1,
      Policy.type=="T20" ~ ifelse(year-Issue.year < 20, 1, 0),
      .default = NA
    )) |> 
    filter(active == 1) |> 
    left_join(techprem_perdb0,
              by = join_by(Policy.type == policy_type,
                           Issue.year == issue_year,
                           Issue.age == issue_age,
                           Sex == sex,
                           Smoker.Status == smoker)) |> 
    mutate(techprem_perdb = ifelse(Policy.type=="SPWL" & Issue.year!=year, 0, techprem_perdb)) |> 
    # new lifetable reference
    mutate(lifetable0 = lifetable) |> 
    mutate(lifetable = case_when(
      lifetable=="LT_SPWL_F_S" & Issue.age<=34 ~ "LT_SPWL_F_S34",
      lifetable=="LT_SPWL_F_S" & Issue.age<=44 ~ "LT_SPWL_F_S44",
      lifetable=="LT_SPWL_F_S" & Issue.age<=54 ~ "LT_SPWL_F_S54",
      lifetable=="LT_SPWL_F_S" ~ "LT_SPWL_F_S64",
      lifetable=="LT_SPWL_M_S" & Issue.age<=34 ~ "LT_SPWL_M_S34",
      lifetable=="LT_SPWL_M_S" & Issue.age<=44 ~ "LT_SPWL_M_S44",
      lifetable=="LT_SPWL_M_S" & Issue.age<=54 ~ "LT_SPWL_M_S54",
      lifetable=="LT_SPWL_M_S" ~ "LT_SPWL_M_S64",
      lifetable=="LT_T20_F_S" & Issue.age<=34 ~ "LT_T20_F_S34",
      lifetable=="LT_T20_F_S" & Issue.age<=44 ~ "LT_T20_F_S44",
      lifetable=="LT_T20_F_S" & Issue.age<=54 ~ "LT_T20_F_S54",
      lifetable=="LT_T20_F_S" ~ "LT_T20_F_S64",
      lifetable=="LT_T20_M_S" & Issue.age<=34 ~ "LT_T20_M_S34",
      lifetable=="LT_T20_M_S" & Issue.age<=44 ~ "LT_T20_M_S44",
      lifetable=="LT_T20_M_S" & Issue.age<=54 ~ "LT_T20_M_S54",
      lifetable=="LT_T20_M_S" ~ "LT_T20_M_S64",
      TRUE ~ lifetable
    )) |> 
    # new smoker reference
    mutate(Smoker.Status0 = Smoker.Status) |> 
    mutate(Smoker.Status = case_when(
      Smoker.Status=="S" & Issue.age<=34 ~ "S34",
      Smoker.Status=="S" & Issue.age<=44 ~ "S44",
      Smoker.Status=="S" & Issue.age<=54 ~ "S54",
      Smoker.Status=="S" ~ "S64",
      Smoker.Status=="S" & Issue.age<=34 ~ "S34",
      Smoker.Status=="S" & Issue.age<=44 ~ "S44",
      Smoker.Status=="S" & Issue.age<=54 ~ "S54",
      Smoker.Status=="S" ~ "S64",
      Smoker.Status=="S" & Issue.age<=34 ~ "S34",
      Smoker.Status=="S" & Issue.age<=44 ~ "S44",
      Smoker.Status=="S" & Issue.age<=54 ~ "S54",
      Smoker.Status=="S" ~ "S64",
      Smoker.Status=="S" & Issue.age<=34 ~ "S34",
      Smoker.Status=="S" & Issue.age<=44 ~ "S44",
      Smoker.Status=="S" & Issue.age<=54 ~ "S54",
      Smoker.Status=="S" ~ "S64",
      TRUE ~ Smoker.Status
    )) |> 
    left_join(cancerprem_percost,
              by = join_by(Policy.type == policy_type,
                           Issue.year == issue_year,
                           Issue.age == issue_age,
                           Sex == sex,
                           Smoker.Status == smoker)) |> 
    rename(lifetable = lifetable.x) |> 
    select(-lifetable.y) |> 
    mutate(now_age = Issue.age + year - Issue.year,
           year_left = case_when(
             Policy.type=="T20" ~ Issue.year + 20 - year,
             .default = NA
           ),
           techprem = Face.amount* techprem_perdb,
           cancerprem = assumption_cancer_cost* cancerprem_percost,
           prem = techprem + cancerprem,
           smokeexpense = case_when(
             Smoker.Status0=="NS" ~ 0,
             Smoker.Status0=="S" ~ case_when(year-Issue.year == 0 ~ 0,
                                            year-Issue.year < 4 ~ assumption_smoking_ratio * assumption_smoking_cost,
                                            .default = 0),
             .default = NA
           ),
           cancerexpense = assumption_cancer_cost,
           expense = smokeexpense + cancerexpense
    ) |> 
    # get inforce ratio
    mutate(inforce_nume = case_when(
      lifetable=="LT_SPWL_F_NS" ~ mdt_LT_SPWL_F_NS[match(now_age, mdt_LT_SPWL_F_NS$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S34" ~ mdt_LT_SPWL_F_S34[match(now_age, mdt_LT_SPWL_F_S34$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S44" ~ mdt_LT_SPWL_F_S44[match(now_age, mdt_LT_SPWL_F_S44$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S54" ~ mdt_LT_SPWL_F_S54[match(now_age, mdt_LT_SPWL_F_S54$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S64" ~ mdt_LT_SPWL_F_S64[match(now_age, mdt_LT_SPWL_F_S64$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_NS" ~ mdt_LT_SPWL_M_NS[match(now_age, mdt_LT_SPWL_M_NS$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S34" ~ mdt_LT_SPWL_M_S34[match(now_age, mdt_LT_SPWL_M_S34$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S44" ~ mdt_LT_SPWL_M_S44[match(now_age, mdt_LT_SPWL_M_S44$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S54" ~ mdt_LT_SPWL_M_S54[match(now_age, mdt_LT_SPWL_M_S54$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S64" ~ mdt_LT_SPWL_M_S64[match(now_age, mdt_LT_SPWL_M_S64$age), ] |> pull(lx),
      lifetable=="LT_T20_F_NS" ~ mdt_LT_T20_F_NS[match(now_age, mdt_LT_T20_F_NS$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S34" ~ mdt_LT_T20_F_S34[match(now_age, mdt_LT_T20_F_S34$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S44" ~ mdt_LT_T20_F_S44[match(now_age, mdt_LT_T20_F_S44$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S54" ~ mdt_LT_T20_F_S54[match(now_age, mdt_LT_T20_F_S54$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S64" ~ mdt_LT_T20_F_S64[match(now_age, mdt_LT_T20_F_S64$age), ] |> pull(lx),
      lifetable=="LT_T20_M_NS" ~ mdt_LT_T20_M_NS[match(now_age, mdt_LT_T20_M_NS$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S34" ~ mdt_LT_T20_M_S34[match(now_age, mdt_LT_T20_M_S34$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S44" ~ mdt_LT_T20_M_S44[match(now_age, mdt_LT_T20_M_S44$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S54" ~ mdt_LT_T20_M_S54[match(now_age, mdt_LT_T20_M_S54$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S64" ~ mdt_LT_T20_M_S64[match(now_age, mdt_LT_T20_M_S64$age), ] |> pull(lx),
      .default = NA
    )) |> 
    mutate(inforce_deno = case_when(
      lifetable=="LT_SPWL_F_NS" ~ mdt_LT_SPWL_F_NS[match(Issue.age, mdt_LT_SPWL_F_NS$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S34" ~ mdt_LT_SPWL_F_S34[match(Issue.age, mdt_LT_SPWL_F_S34$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S44" ~ mdt_LT_SPWL_F_S44[match(Issue.age, mdt_LT_SPWL_F_S44$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S54" ~ mdt_LT_SPWL_F_S54[match(Issue.age, mdt_LT_SPWL_F_S54$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S64" ~ mdt_LT_SPWL_F_S64[match(Issue.age, mdt_LT_SPWL_F_S64$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_NS" ~ mdt_LT_SPWL_M_NS[match(Issue.age, mdt_LT_SPWL_M_NS$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S34" ~ mdt_LT_SPWL_M_S34[match(Issue.age, mdt_LT_SPWL_M_S34$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S44" ~ mdt_LT_SPWL_M_S44[match(Issue.age, mdt_LT_SPWL_M_S44$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S54" ~ mdt_LT_SPWL_M_S54[match(Issue.age, mdt_LT_SPWL_M_S54$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S64" ~ mdt_LT_SPWL_M_S64[match(Issue.age, mdt_LT_SPWL_M_S64$age), ] |> pull(lx),
      lifetable=="LT_T20_F_NS" ~ mdt_LT_T20_F_NS[match(Issue.age, mdt_LT_T20_F_NS$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S34" ~ mdt_LT_T20_F_S34[match(Issue.age, mdt_LT_T20_F_S34$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S44" ~ mdt_LT_T20_F_S44[match(Issue.age, mdt_LT_T20_F_S44$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S54" ~ mdt_LT_T20_F_S54[match(Issue.age, mdt_LT_T20_F_S54$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S64" ~ mdt_LT_T20_F_S64[match(Issue.age, mdt_LT_T20_F_S64$age), ] |> pull(lx),
      lifetable=="LT_T20_M_NS" ~ mdt_LT_T20_M_NS[match(Issue.age, mdt_LT_T20_M_NS$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S34" ~ mdt_LT_T20_M_S34[match(Issue.age, mdt_LT_T20_M_S34$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S44" ~ mdt_LT_T20_M_S44[match(Issue.age, mdt_LT_T20_M_S44$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S54" ~ mdt_LT_T20_M_S54[match(Issue.age, mdt_LT_T20_M_S54$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S64" ~ mdt_LT_T20_M_S64[match(Issue.age, mdt_LT_T20_M_S64$age), ] |> pull(lx),
      .default = NA
    )) |> 
    mutate(inforce = inforce_nume / inforce_deno) |> 
    # get inforce ratio (m1 for reserve_sop)
    mutate(inforce_nume_m1 = case_when(
      lifetable=="LT_SPWL_F_NS" ~ mdt_LT_SPWL_F_NS[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_F_NS$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S34" ~ mdt_LT_SPWL_F_S34[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_F_S34$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S44" ~ mdt_LT_SPWL_F_S44[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_F_S44$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S54" ~ mdt_LT_SPWL_F_S54[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_F_S54$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S64" ~ mdt_LT_SPWL_F_S64[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_F_S64$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_NS" ~ mdt_LT_SPWL_M_NS[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_M_NS$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S34" ~ mdt_LT_SPWL_M_S34[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_M_S34$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S44" ~ mdt_LT_SPWL_M_S44[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_M_S44$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S54" ~ mdt_LT_SPWL_M_S54[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_M_S54$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S64" ~ mdt_LT_SPWL_M_S64[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_M_S64$age), ] |> pull(lx),
      lifetable=="LT_T20_F_NS" ~ mdt_LT_T20_F_NS[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_F_NS$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S34" ~ mdt_LT_T20_F_S34[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_F_S34$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S44" ~ mdt_LT_T20_F_S44[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_F_S44$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S54" ~ mdt_LT_T20_F_S54[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_F_S54$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S64" ~ mdt_LT_T20_F_S64[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_F_S64$age), ] |> pull(lx),
      lifetable=="LT_T20_M_NS" ~ mdt_LT_T20_M_NS[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_M_NS$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S34" ~ mdt_LT_T20_M_S34[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_M_S34$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S44" ~ mdt_LT_T20_M_S44[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_M_S44$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S54" ~ mdt_LT_T20_M_S54[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_M_S54$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S64" ~ mdt_LT_T20_M_S64[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_M_S64$age), ] |> pull(lx),
      .default = NA
    )) |> 
    mutate(inforce_deno_m1 = case_when(
      lifetable=="LT_SPWL_F_NS" ~ mdt_LT_SPWL_F_NS[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_F_NS$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S34" ~ mdt_LT_SPWL_F_S34[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_F_S34$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S44" ~ mdt_LT_SPWL_F_S44[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_F_S44$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S54" ~ mdt_LT_SPWL_F_S54[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_F_S54$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S64" ~ mdt_LT_SPWL_F_S64[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_F_S64$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_NS" ~ mdt_LT_SPWL_M_NS[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_M_NS$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S34" ~ mdt_LT_SPWL_M_S34[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_M_S34$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S44" ~ mdt_LT_SPWL_M_S44[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_M_S44$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S54" ~ mdt_LT_SPWL_M_S54[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_M_S54$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S64" ~ mdt_LT_SPWL_M_S64[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_M_S64$age), ] |> pull(lx),
      lifetable=="LT_T20_F_NS" ~ mdt_LT_T20_F_NS[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_F_NS$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S34" ~ mdt_LT_T20_F_S34[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_F_S34$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S44" ~ mdt_LT_T20_F_S44[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_F_S44$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S54" ~ mdt_LT_T20_F_S54[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_F_S54$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S64" ~ mdt_LT_T20_F_S64[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_F_S64$age), ] |> pull(lx),
      lifetable=="LT_T20_M_NS" ~ mdt_LT_T20_M_NS[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_M_NS$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S34" ~ mdt_LT_T20_M_S34[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_M_S34$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S44" ~ mdt_LT_T20_M_S44[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_M_S44$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S54" ~ mdt_LT_T20_M_S54[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_M_S54$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S64" ~ mdt_LT_T20_M_S64[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_M_S64$age), ] |> pull(lx),
      .default = NA
    )) |> 
    mutate(inforce_m1 = inforce_nume_m1 / inforce_deno_m1) |> 
    # get death decrement
    mutate(death = case_when(
      lifetable=="LT_SPWL_F_NS" ~ mdt_LT_SPWL_F_NS[match(now_age, mdt_LT_SPWL_F_NS$age), ] |> pull(death),
      lifetable=="LT_SPWL_F_S34" ~ mdt_LT_SPWL_F_S34[match(now_age, mdt_LT_SPWL_F_S34$age), ] |> pull(death),
      lifetable=="LT_SPWL_F_S44" ~ mdt_LT_SPWL_F_S44[match(now_age, mdt_LT_SPWL_F_S44$age), ] |> pull(death),
      lifetable=="LT_SPWL_F_S54" ~ mdt_LT_SPWL_F_S54[match(now_age, mdt_LT_SPWL_F_S54$age), ] |> pull(death),
      lifetable=="LT_SPWL_F_S64" ~ mdt_LT_SPWL_F_S64[match(now_age, mdt_LT_SPWL_F_S64$age), ] |> pull(death),
      lifetable=="LT_SPWL_M_NS" ~ mdt_LT_SPWL_M_NS[match(now_age, mdt_LT_SPWL_M_NS$age), ] |> pull(death),
      lifetable=="LT_SPWL_M_S34" ~ mdt_LT_SPWL_M_S34[match(now_age, mdt_LT_SPWL_M_S34$age), ] |> pull(death),
      lifetable=="LT_SPWL_M_S44" ~ mdt_LT_SPWL_M_S44[match(now_age, mdt_LT_SPWL_M_S44$age), ] |> pull(death),
      lifetable=="LT_SPWL_M_S54" ~ mdt_LT_SPWL_M_S54[match(now_age, mdt_LT_SPWL_M_S54$age), ] |> pull(death),
      lifetable=="LT_SPWL_M_S64" ~ mdt_LT_SPWL_M_S64[match(now_age, mdt_LT_SPWL_M_S64$age), ] |> pull(death),
      lifetable=="LT_T20_F_NS" ~ mdt_LT_T20_F_NS[match(now_age, mdt_LT_T20_F_NS$age), ] |> pull(death),
      lifetable=="LT_T20_F_S34" ~ mdt_LT_T20_F_S34[match(now_age, mdt_LT_T20_F_S34$age), ] |> pull(death),
      lifetable=="LT_T20_F_S44" ~ mdt_LT_T20_F_S44[match(now_age, mdt_LT_T20_F_S44$age), ] |> pull(death),
      lifetable=="LT_T20_F_S54" ~ mdt_LT_T20_F_S54[match(now_age, mdt_LT_T20_F_S54$age), ] |> pull(death),
      lifetable=="LT_T20_F_S64" ~ mdt_LT_T20_F_S64[match(now_age, mdt_LT_T20_F_S64$age), ] |> pull(death),
      lifetable=="LT_T20_M_NS" ~ mdt_LT_T20_M_NS[match(now_age, mdt_LT_T20_M_NS$age), ] |> pull(death),
      lifetable=="LT_T20_M_S34" ~ mdt_LT_T20_M_S34[match(now_age, mdt_LT_T20_M_S34$age), ] |> pull(death),
      lifetable=="LT_T20_M_S44" ~ mdt_LT_T20_M_S44[match(now_age, mdt_LT_T20_M_S44$age), ] |> pull(death),
      lifetable=="LT_T20_M_S54" ~ mdt_LT_T20_M_S54[match(now_age, mdt_LT_T20_M_S54$age), ] |> pull(death),
      lifetable=="LT_T20_M_S64" ~ mdt_LT_T20_M_S64[match(now_age, mdt_LT_T20_M_S64$age), ] |> pull(death),
      .default = NA
    )) |> 
    # lapse decrement NOT NEEDED no lapse benefit
    #
    # reserve (end of period)
    # SPWL just next year next age premium since single premium
    left_join(techprem_perdb |> mutate(reserve_eop_tech = techprem_perdb) |> filter(issue_year == year),
              by = join_by(Policy.type == policy_type,
                           now_age == issue_age,
                           Sex == sex,
                           Smoker.Status == smoker)) |> 
    rename(lifetable = lifetable.x, techprem_perdb = techprem_perdb.x) |> 
    select(-c(issue_year, lifetable.y, techprem_perdb.y)) |> 
    mutate(reserve_eop_tech = case_when(
      Policy.type=="SPWL" ~ reserve_eop_tech * Face.amount,
      Policy.type=="T20" & Smoker.Status0=="S" ~ assumption_reserve_T20_S * Face.amount,
      Policy.type=="T20" & Smoker.Status0=="NS" ~ assumption_reserve_T20_NS * Face.amount,
      .default = NA
    )) |> 
    left_join(cancerprem_percost |> mutate(reserve_eop_cancer = cancerprem_percost) |> filter(issue_year == year),
              by = join_by(Policy.type == policy_type,
                           now_age == issue_age,
                           Sex == sex,
                           Smoker.Status == smoker)) |> 
    rename(lifetable = lifetable.x, cancerprem_percost = cancerprem_percost.x) |> 
    select(-c(issue_year, lifetable.y, cancerprem_percost.y)) |> 
    mutate(reserve_eop_cancer = case_when(
      Policy.type=="SPWL" ~ reserve_eop_cancer * Face.amount,
      Policy.type=="T20" & Smoker.Status0=="S" ~ assumption_reserve_T20_S * assumption_cancer_cost,
      Policy.type=="T20" & Smoker.Status0=="NS" ~ assumption_reserve_T20_NS * assumption_cancer_cost,
      .default = NA
    )) |> 
    mutate(reserve_eop = reserve_eop_tech + reserve_eop_cancer) |> 
    # reserve last year (start of period)
    mutate(now_age_m1 = now_age - 1) |> 
    left_join(techprem_perdb |> mutate(reserve_sop_tech = techprem_perdb) |> filter(issue_year == year - 1),
              by = join_by(Policy.type == policy_type,
                           now_age_m1 == issue_age,
                           Sex == sex,
                           Smoker.Status == smoker)) |> 
    rename(lifetable = lifetable.x, techprem_perdb = techprem_perdb.x) |> 
    select(-c(issue_year, lifetable.y, techprem_perdb.y)) |> 
    mutate(reserve_sop_tech = case_when(
      Policy.type=="SPWL" ~ reserve_sop_tech * Face.amount,
      Policy.type=="T20" & Smoker.Status0=="S" ~ assumption_reserve_T20_S * Face.amount,
      Policy.type=="T20" & Smoker.Status0=="NS" ~ assumption_reserve_T20_NS * Face.amount,
      .default = NA
    )) |> 
    left_join(cancerprem_percost |> mutate(reserve_sop_cancer = cancerprem_percost) |> filter(issue_year == year - 1),
              by = join_by(Policy.type == policy_type,
                           now_age_m1 == issue_age,
                           Sex == sex,
                           Smoker.Status == smoker)) |> 
    rename(lifetable = lifetable.x, cancerprem_percost = cancerprem_percost.x) |> 
    select(-c(now_age_m1, issue_year, lifetable.y, cancerprem_percost.y)) |> 
    mutate(reserve_sop_cancer = case_when(
      Policy.type=="SPWL" ~ reserve_sop_cancer * Face.amount,
      Policy.type=="T20" & Smoker.Status0=="S" ~ assumption_reserve_T20_S * assumption_cancer_cost,
      Policy.type=="T20" & Smoker.Status0=="NS" ~ assumption_reserve_T20_NS * assumption_cancer_cost,
      .default = NA
    )) |> 
    mutate(reserve_sop = reserve_sop_tech + reserve_sop_cancer) |> 
    # decremented columns
    mutate(reserve_eop_dct = reserve_eop * inforce,
           reserve_sop_dct = reserve_sop * inforce_m1,
           prem_dct = prem * inforce,
           expense_dct = expense * inforce,
           db_dct = Face.amount * death) |> 
    # computed columns
    mutate(cashflow = prem_dct - expense_dct - db_dct,
           reserve_increase = reserve_eop_dct - reserve_sop_dct,
           interest = (prem_dct + reserve_sop_dct) * i_discount[match(year, i_discount$year), ] |> pull(rate),
           profit = cashflow - reserve_increase + interest)
  
  ## future customers !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  scored_future <- PH_future |> 
    # keep only active ones
    filter(Issue.year <= year) |> 
    mutate(active = case_when(
      Policy.type=="SPWL" ~ 1,
      Policy.type=="T20" ~ ifelse(year-Issue.year < 20, 1, 0),
      .default = NA
    )) |> 
    filter(active == 1) |> 
    # new smoker reference
    mutate(Smoker.Status0 = Smoker.Status) |> 
    mutate(Smoker.Status = case_when(
      Smoker.Status=="S" & Issue.age<=34 ~ "S34",
      Smoker.Status=="S" & Issue.age<=44 ~ "S44",
      Smoker.Status=="S" & Issue.age<=54 ~ "S54",
      Smoker.Status=="S" ~ "S64",
      Smoker.Status=="S" & Issue.age<=34 ~ "S34",
      Smoker.Status=="S" & Issue.age<=44 ~ "S44",
      Smoker.Status=="S" & Issue.age<=54 ~ "S54",
      Smoker.Status=="S" ~ "S64",
      Smoker.Status=="S" & Issue.age<=34 ~ "S34",
      Smoker.Status=="S" & Issue.age<=44 ~ "S44",
      Smoker.Status=="S" & Issue.age<=54 ~ "S54",
      Smoker.Status=="S" ~ "S64",
      Smoker.Status=="S" & Issue.age<=34 ~ "S34",
      Smoker.Status=="S" & Issue.age<=44 ~ "S44",
      Smoker.Status=="S" & Issue.age<=54 ~ "S54",
      Smoker.Status=="S" ~ "S64",
      TRUE ~ Smoker.Status
    )) |> 
    left_join(techprem_perdb,
              by = join_by(Policy.type == policy_type,
                           Issue.year == issue_year,
                           Issue.age == issue_age,
                           Sex == sex,
                           Smoker.Status == smoker)) |> 
    mutate(techprem_perdb = ifelse(Policy.type=="SPWL" & Issue.year!=year, 0, techprem_perdb)) |> 
    left_join(cancerprem_percost,
              by = join_by(Policy.type == policy_type,
                           Issue.year == issue_year,
                           Issue.age == issue_age,
                           Sex == sex,
                           Smoker.Status == smoker)) |> 
    rename(lifetable = lifetable.x) |> 
    select(-lifetable.y) |> 
    mutate(now_age = Issue.age + year - Issue.year,
           year_left = case_when(
             Policy.type=="T20" ~ Issue.year + 20 - year,
             .default = NA
           ),
           techprem = Face.amount* techprem_perdb,
           cancerprem = assumption_cancer_cost* cancerprem_percost,
           prem = techprem + cancerprem,
           smokeexpense = case_when(
             Smoker.Status0=="NS" ~ 0,
             Smoker.Status0=="S" ~ case_when(year-Issue.year == 0 ~ 0,
                                             year-Issue.year < 4 ~ assumption_smoking_ratio * assumption_smoking_cost,
                                             .default = 0),
             .default = NA
           ),
           cancerexpense = assumption_cancer_cost,
           expense = smokeexpense + cancerexpense
    ) |> 
    # get inforce ratio
    mutate(inforce_nume = case_when(
      lifetable=="LT_SPWL_F_NS" ~ mdt_LT_SPWL_F_NS[match(now_age, mdt_LT_SPWL_F_NS$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S34" ~ mdt_LT_SPWL_F_S34[match(now_age, mdt_LT_SPWL_F_S34$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S44" ~ mdt_LT_SPWL_F_S44[match(now_age, mdt_LT_SPWL_F_S44$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S54" ~ mdt_LT_SPWL_F_S54[match(now_age, mdt_LT_SPWL_F_S54$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S64" ~ mdt_LT_SPWL_F_S64[match(now_age, mdt_LT_SPWL_F_S64$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_NS" ~ mdt_LT_SPWL_M_NS[match(now_age, mdt_LT_SPWL_M_NS$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S34" ~ mdt_LT_SPWL_M_S34[match(now_age, mdt_LT_SPWL_M_S34$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S44" ~ mdt_LT_SPWL_M_S44[match(now_age, mdt_LT_SPWL_M_S44$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S54" ~ mdt_LT_SPWL_M_S54[match(now_age, mdt_LT_SPWL_M_S54$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S64" ~ mdt_LT_SPWL_M_S64[match(now_age, mdt_LT_SPWL_M_S64$age), ] |> pull(lx),
      lifetable=="LT_T20_F_NS" ~ mdt_LT_T20_F_NS[match(now_age, mdt_LT_T20_F_NS$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S34" ~ mdt_LT_T20_F_S34[match(now_age, mdt_LT_T20_F_S34$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S44" ~ mdt_LT_T20_F_S44[match(now_age, mdt_LT_T20_F_S44$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S54" ~ mdt_LT_T20_F_S54[match(now_age, mdt_LT_T20_F_S54$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S64" ~ mdt_LT_T20_F_S64[match(now_age, mdt_LT_T20_F_S64$age), ] |> pull(lx),
      lifetable=="LT_T20_M_NS" ~ mdt_LT_T20_M_NS[match(now_age, mdt_LT_T20_M_NS$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S34" ~ mdt_LT_T20_M_S34[match(now_age, mdt_LT_T20_M_S34$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S44" ~ mdt_LT_T20_M_S44[match(now_age, mdt_LT_T20_M_S44$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S54" ~ mdt_LT_T20_M_S54[match(now_age, mdt_LT_T20_M_S54$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S64" ~ mdt_LT_T20_M_S64[match(now_age, mdt_LT_T20_M_S64$age), ] |> pull(lx),
      .default = NA
    )) |> 
    mutate(inforce_deno = case_when(
      lifetable=="LT_SPWL_F_NS" ~ mdt_LT_SPWL_F_NS[match(Issue.age, mdt_LT_SPWL_F_NS$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S34" ~ mdt_LT_SPWL_F_S34[match(Issue.age, mdt_LT_SPWL_F_S34$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S44" ~ mdt_LT_SPWL_F_S44[match(Issue.age, mdt_LT_SPWL_F_S44$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S54" ~ mdt_LT_SPWL_F_S54[match(Issue.age, mdt_LT_SPWL_F_S54$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S64" ~ mdt_LT_SPWL_F_S64[match(Issue.age, mdt_LT_SPWL_F_S64$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_NS" ~ mdt_LT_SPWL_M_NS[match(Issue.age, mdt_LT_SPWL_M_NS$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S34" ~ mdt_LT_SPWL_M_S34[match(Issue.age, mdt_LT_SPWL_M_S34$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S44" ~ mdt_LT_SPWL_M_S44[match(Issue.age, mdt_LT_SPWL_M_S44$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S54" ~ mdt_LT_SPWL_M_S54[match(Issue.age, mdt_LT_SPWL_M_S54$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S64" ~ mdt_LT_SPWL_M_S64[match(Issue.age, mdt_LT_SPWL_M_S64$age), ] |> pull(lx),
      lifetable=="LT_T20_F_NS" ~ mdt_LT_T20_F_NS[match(Issue.age, mdt_LT_T20_F_NS$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S34" ~ mdt_LT_T20_F_S34[match(Issue.age, mdt_LT_T20_F_S34$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S44" ~ mdt_LT_T20_F_S44[match(Issue.age, mdt_LT_T20_F_S44$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S54" ~ mdt_LT_T20_F_S54[match(Issue.age, mdt_LT_T20_F_S54$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S64" ~ mdt_LT_T20_F_S64[match(Issue.age, mdt_LT_T20_F_S64$age), ] |> pull(lx),
      lifetable=="LT_T20_M_NS" ~ mdt_LT_T20_M_NS[match(Issue.age, mdt_LT_T20_M_NS$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S34" ~ mdt_LT_T20_M_S34[match(Issue.age, mdt_LT_T20_M_S34$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S44" ~ mdt_LT_T20_M_S44[match(Issue.age, mdt_LT_T20_M_S44$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S54" ~ mdt_LT_T20_M_S54[match(Issue.age, mdt_LT_T20_M_S54$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S64" ~ mdt_LT_T20_M_S64[match(Issue.age, mdt_LT_T20_M_S64$age), ] |> pull(lx),
      .default = NA
    )) |> 
    mutate(inforce = inforce_nume / inforce_deno) |> 
    # get inforce ratio (m1 for reserve_sop)
    mutate(inforce_nume_m1 = case_when(
      lifetable=="LT_SPWL_F_NS" ~ mdt_LT_SPWL_F_NS[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_F_NS$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S34" ~ mdt_LT_SPWL_F_S34[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_F_S34$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S44" ~ mdt_LT_SPWL_F_S44[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_F_S44$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S54" ~ mdt_LT_SPWL_F_S54[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_F_S54$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S64" ~ mdt_LT_SPWL_F_S64[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_F_S64$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_NS" ~ mdt_LT_SPWL_M_NS[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_M_NS$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S34" ~ mdt_LT_SPWL_M_S34[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_M_S34$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S44" ~ mdt_LT_SPWL_M_S44[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_M_S44$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S54" ~ mdt_LT_SPWL_M_S54[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_M_S54$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S64" ~ mdt_LT_SPWL_M_S64[match(ifelse(now_age-1 < 35, 35, now_age-1), mdt_LT_SPWL_M_S64$age), ] |> pull(lx),
      lifetable=="LT_T20_F_NS" ~ mdt_LT_T20_F_NS[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_F_NS$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S34" ~ mdt_LT_T20_F_S34[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_F_S34$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S44" ~ mdt_LT_T20_F_S44[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_F_S44$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S54" ~ mdt_LT_T20_F_S54[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_F_S54$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S64" ~ mdt_LT_T20_F_S64[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_F_S64$age), ] |> pull(lx),
      lifetable=="LT_T20_M_NS" ~ mdt_LT_T20_M_NS[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_M_NS$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S34" ~ mdt_LT_T20_M_S34[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_M_S34$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S44" ~ mdt_LT_T20_M_S44[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_M_S44$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S54" ~ mdt_LT_T20_M_S54[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_M_S54$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S64" ~ mdt_LT_T20_M_S64[match(ifelse(now_age-1 < 26, 26, now_age-1), mdt_LT_T20_M_S64$age), ] |> pull(lx),
      .default = NA
    )) |> 
    mutate(inforce_deno_m1 = case_when(
      lifetable=="LT_SPWL_F_NS" ~ mdt_LT_SPWL_F_NS[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_F_NS$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S34" ~ mdt_LT_SPWL_F_S34[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_F_S34$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S44" ~ mdt_LT_SPWL_F_S44[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_F_S44$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S54" ~ mdt_LT_SPWL_F_S54[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_F_S54$age), ] |> pull(lx),
      lifetable=="LT_SPWL_F_S64" ~ mdt_LT_SPWL_F_S64[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_F_S64$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_NS" ~ mdt_LT_SPWL_M_NS[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_M_NS$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S34" ~ mdt_LT_SPWL_M_S34[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_M_S34$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S44" ~ mdt_LT_SPWL_M_S44[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_M_S44$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S54" ~ mdt_LT_SPWL_M_S54[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_M_S54$age), ] |> pull(lx),
      lifetable=="LT_SPWL_M_S64" ~ mdt_LT_SPWL_M_S64[match(ifelse(Issue.age-1 < 35, 35, Issue.age-1), mdt_LT_SPWL_M_S64$age), ] |> pull(lx),
      lifetable=="LT_T20_F_NS" ~ mdt_LT_T20_F_NS[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_F_NS$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S34" ~ mdt_LT_T20_F_S34[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_F_S34$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S44" ~ mdt_LT_T20_F_S44[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_F_S44$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S54" ~ mdt_LT_T20_F_S54[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_F_S54$age), ] |> pull(lx),
      lifetable=="LT_T20_F_S64" ~ mdt_LT_T20_F_S64[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_F_S64$age), ] |> pull(lx),
      lifetable=="LT_T20_M_NS" ~ mdt_LT_T20_M_NS[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_M_NS$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S34" ~ mdt_LT_T20_M_S34[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_M_S34$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S44" ~ mdt_LT_T20_M_S44[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_M_S44$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S54" ~ mdt_LT_T20_M_S54[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_M_S54$age), ] |> pull(lx),
      lifetable=="LT_T20_M_S64" ~ mdt_LT_T20_M_S64[match(ifelse(Issue.age-1 < 26, 26, Issue.age-1), mdt_LT_T20_M_S64$age), ] |> pull(lx),
      .default = NA
    )) |> 
    mutate(inforce_m1 = inforce_nume_m1 / inforce_deno_m1) |> 
    # get death decrement
    mutate(death = case_when(
      lifetable=="LT_SPWL_F_NS" ~ mdt_LT_SPWL_F_NS[match(now_age, mdt_LT_SPWL_F_NS$age), ] |> pull(death),
      lifetable=="LT_SPWL_F_S34" ~ mdt_LT_SPWL_F_S34[match(now_age, mdt_LT_SPWL_F_S34$age), ] |> pull(death),
      lifetable=="LT_SPWL_F_S44" ~ mdt_LT_SPWL_F_S44[match(now_age, mdt_LT_SPWL_F_S44$age), ] |> pull(death),
      lifetable=="LT_SPWL_F_S54" ~ mdt_LT_SPWL_F_S54[match(now_age, mdt_LT_SPWL_F_S54$age), ] |> pull(death),
      lifetable=="LT_SPWL_F_S64" ~ mdt_LT_SPWL_F_S64[match(now_age, mdt_LT_SPWL_F_S64$age), ] |> pull(death),
      lifetable=="LT_SPWL_M_NS" ~ mdt_LT_SPWL_M_NS[match(now_age, mdt_LT_SPWL_M_NS$age), ] |> pull(death),
      lifetable=="LT_SPWL_M_S34" ~ mdt_LT_SPWL_M_S34[match(now_age, mdt_LT_SPWL_M_S34$age), ] |> pull(death),
      lifetable=="LT_SPWL_M_S44" ~ mdt_LT_SPWL_M_S44[match(now_age, mdt_LT_SPWL_M_S44$age), ] |> pull(death),
      lifetable=="LT_SPWL_M_S54" ~ mdt_LT_SPWL_M_S54[match(now_age, mdt_LT_SPWL_M_S54$age), ] |> pull(death),
      lifetable=="LT_SPWL_M_S64" ~ mdt_LT_SPWL_M_S64[match(now_age, mdt_LT_SPWL_M_S64$age), ] |> pull(death),
      lifetable=="LT_T20_F_NS" ~ mdt_LT_T20_F_NS[match(now_age, mdt_LT_T20_F_NS$age), ] |> pull(death),
      lifetable=="LT_T20_F_S34" ~ mdt_LT_T20_F_S34[match(now_age, mdt_LT_T20_F_S34$age), ] |> pull(death),
      lifetable=="LT_T20_F_S44" ~ mdt_LT_T20_F_S44[match(now_age, mdt_LT_T20_F_S44$age), ] |> pull(death),
      lifetable=="LT_T20_F_S54" ~ mdt_LT_T20_F_S54[match(now_age, mdt_LT_T20_F_S54$age), ] |> pull(death),
      lifetable=="LT_T20_F_S64" ~ mdt_LT_T20_F_S64[match(now_age, mdt_LT_T20_F_S64$age), ] |> pull(death),
      lifetable=="LT_T20_M_NS" ~ mdt_LT_T20_M_NS[match(now_age, mdt_LT_T20_M_NS$age), ] |> pull(death),
      lifetable=="LT_T20_M_S34" ~ mdt_LT_T20_M_S34[match(now_age, mdt_LT_T20_M_S34$age), ] |> pull(death),
      lifetable=="LT_T20_M_S44" ~ mdt_LT_T20_M_S44[match(now_age, mdt_LT_T20_M_S44$age), ] |> pull(death),
      lifetable=="LT_T20_M_S54" ~ mdt_LT_T20_M_S54[match(now_age, mdt_LT_T20_M_S54$age), ] |> pull(death),
      lifetable=="LT_T20_M_S64" ~ mdt_LT_T20_M_S64[match(now_age, mdt_LT_T20_M_S64$age), ] |> pull(death),
      .default = NA
    )) |> 
    # lapse decrement NOT NEEDED no lapse benefit
    #
    # reserve (end of period)
    # SPWL just next year next age premium since single premium
    left_join(techprem_perdb |> mutate(reserve_eop_tech = techprem_perdb) |> filter(issue_year == year),
              by = join_by(Policy.type == policy_type,
                           now_age == issue_age,
                           Sex == sex,
                           Smoker.Status == smoker)) |> 
    rename(lifetable = lifetable.x, techprem_perdb = techprem_perdb.x) |> 
    select(-c(issue_year, lifetable.y, techprem_perdb.y)) |> 
    mutate(reserve_eop_tech = case_when(
      Policy.type=="SPWL" ~ reserve_eop_tech * Face.amount,
      Policy.type=="T20" & Smoker.Status0=="S" ~ assumption_reserve_T20_S * Face.amount,
      Policy.type=="T20" & Smoker.Status0=="NS" ~ assumption_reserve_T20_NS * Face.amount,
      .default = NA
    )) |> 
    left_join(cancerprem_percost |> mutate(reserve_eop_cancer = cancerprem_percost) |> filter(issue_year == year),
              by = join_by(Policy.type == policy_type,
                           now_age == issue_age,
                           Sex == sex,
                           Smoker.Status == smoker)) |> 
    rename(lifetable = lifetable.x, cancerprem_percost = cancerprem_percost.x) |> 
    select(-c(issue_year, lifetable.y, cancerprem_percost.y)) |> 
    mutate(reserve_eop_cancer = case_when(
      Policy.type=="SPWL" ~ reserve_eop_cancer * Face.amount,
      Policy.type=="T20" & Smoker.Status0=="S" ~ assumption_reserve_T20_S * assumption_cancer_cost,
      Policy.type=="T20" & Smoker.Status0=="NS" ~ assumption_reserve_T20_NS * assumption_cancer_cost,
      .default = NA
    )) |> 
    mutate(reserve_eop = reserve_eop_tech + reserve_eop_cancer) |> 
    # reserve last year (start of period)
    mutate(now_age_m1 = now_age - 1) |> 
    left_join(techprem_perdb |> mutate(reserve_sop_tech = techprem_perdb) |> filter(issue_year == year - 1),
              by = join_by(Policy.type == policy_type,
                           now_age_m1 == issue_age,
                           Sex == sex,
                           Smoker.Status == smoker)) |> 
    rename(lifetable = lifetable.x, techprem_perdb = techprem_perdb.x) |> 
    select(-c(issue_year, lifetable.y, techprem_perdb.y)) |> 
    mutate(reserve_sop_tech = case_when(
      Policy.type=="SPWL" ~ reserve_sop_tech * Face.amount,
      Policy.type=="T20" & Smoker.Status0=="S" ~ assumption_reserve_T20_S * Face.amount,
      Policy.type=="T20" & Smoker.Status0=="NS" ~ assumption_reserve_T20_NS * Face.amount,
      .default = NA
    )) |> 
    left_join(cancerprem_percost |> mutate(reserve_sop_cancer = cancerprem_percost) |> filter(issue_year == year - 1),
              by = join_by(Policy.type == policy_type,
                           now_age_m1 == issue_age,
                           Sex == sex,
                           Smoker.Status == smoker)) |> 
    rename(lifetable = lifetable.x, cancerprem_percost = cancerprem_percost.x) |> 
    select(-c(now_age_m1, issue_year, lifetable.y, cancerprem_percost.y)) |> 
    mutate(reserve_sop_cancer = case_when(
      Policy.type=="SPWL" ~ reserve_sop_cancer * Face.amount,
      Policy.type=="T20" & Smoker.Status0=="S" ~ assumption_reserve_T20_S * assumption_cancer_cost,
      Policy.type=="T20" & Smoker.Status0=="NS" ~ assumption_reserve_T20_NS * assumption_cancer_cost,
      .default = NA
    )) |> 
    mutate(reserve_sop = reserve_sop_tech + reserve_sop_cancer) |> 
    # decremented columns
    mutate(reserve_eop_dct = reserve_eop * inforce,
           reserve_sop_dct = reserve_sop * inforce_m1,
           prem_dct = prem * inforce,
           expense_dct = expense * inforce,
           db_dct = Face.amount * death) |> 
    # computed columns
    mutate(cashflow = prem_dct - expense_dct - db_dct,
           reserve_increase = reserve_eop_dct - reserve_sop_dct,
           interest = (prem_dct + reserve_sop_dct) * i_discount[match(year, i_discount$year), ] |> pull(rate),
           profit = cashflow - reserve_increase + interest)
  
  # save the ledger
  scored_existing |> 
    bind_rows(scored_future) |> 
    write_csv(paste0("data/", year, "ledger-with-intervention-best.csv"))
}





