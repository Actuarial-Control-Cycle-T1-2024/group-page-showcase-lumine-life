library(readr)
library(dplyr)


result <- data.frame(year = 2024:2043,
                     summation = NA,
                     min = NA,
                     q1 = NA,
                     median = NA,
                     mean = NA,
                     q3 = NA,
                     max = NA)

# base --------------------------------------------------------------------

result0 <- result
for (i in 2024:2043) {
  print(paste("doing", i))
  
  noprog <- read_csv(paste0("data/", i, "ledger-no-intervention.csv"))
  withprog <- read_csv(paste0("data/", i, "ledger-with-intervention.csv"))
  
  benefit <- withprog$profit - noprog$profit
  
  result0[match(i, result0$year), 2] <- sum(benefit)
  result0[match(i, result0$year), 3] <- summary(benefit)[1]
  result0[match(i, result0$year), 4] <- summary(benefit)[2]
  result0[match(i, result0$year), 5] <- summary(benefit)[3]
  result0[match(i, result0$year), 6] <- summary(benefit)[4]
  result0[match(i, result0$year), 7] <- summary(benefit)[5]
  result0[match(i, result0$year), 8] <- summary(benefit)[6]
}
result0 |> write_csv("data/benefit-base-fixed.csv")

result1 <- result
for (i in 2024:2043) {
  print(paste("doing", i))
  
  noprog <- read_csv(paste0("data/", i, "ledger-no-intervention.csv"))
  withprog <- read_csv(paste0("data/", i, "ledger-with-intervention.csv"))
  
  benefit <- withprog$profit/withprog$Face.amount*1000 - noprog$profit/noprog$Face.amount*1000
  
  result1[match(i, result1$year), 2] <- sum(benefit)
  result1[match(i, result1$year), 3] <- summary(benefit)[1]
  result1[match(i, result1$year), 4] <- summary(benefit)[2]
  result1[match(i, result1$year), 5] <- summary(benefit)[3]
  result1[match(i, result1$year), 6] <- summary(benefit)[4]
  result1[match(i, result1$year), 7] <- summary(benefit)[5]
  result1[match(i, result1$year), 8] <- summary(benefit)[6]
}
result1 |> write_csv("data/benefit-per1kDB-base.csv")

result4 <- result
for (i in 2024:2043) {
  print(paste("doing", i))
  
  noprog <- read_csv(paste0("data/", i, "ledger-no-intervention.csv"))
  withprog <- read_csv(paste0("data/", i, "ledger-with-intervention.csv"))
  
  benefit <- withprog$prem/withprog$Face.amount*1000 - noprog$prem/noprog$Face.amount*1000
  
  result4[match(i, result4$year), 2] <- sum(benefit)
  result4[match(i, result4$year), 3] <- summary(benefit)[1]
  result4[match(i, result4$year), 4] <- summary(benefit)[2]
  result4[match(i, result4$year), 5] <- summary(benefit)[3]
  result4[match(i, result4$year), 6] <- summary(benefit)[4]
  result4[match(i, result4$year), 7] <- summary(benefit)[5]
  result4[match(i, result4$year), 8] <- summary(benefit)[6]
}
result4 |> write_csv("data/prem-change-per1kDB-base.csv")

# worst -------------------------------------------------------------------

result2 <- result
for (i in 2024:2043) {
  print(paste("doing", i))
  
  noprog <- read_csv(paste0("data/", i, "ledger-no-intervention-worst.csv"))
  withprog <- read_csv(paste0("data/", i, "ledger-with-intervention-worst.csv"))
  
  benefit <- withprog$profit/withprog$Face.amount*1000 - noprog$profit/noprog$Face.amount*1000
  
  result2[match(i, result2$year), 2] <- sum(benefit)
  result2[match(i, result2$year), 3] <- summary(benefit)[1]
  result2[match(i, result2$year), 4] <- summary(benefit)[2]
  result2[match(i, result2$year), 5] <- summary(benefit)[3]
  result2[match(i, result2$year), 6] <- summary(benefit)[4]
  result2[match(i, result2$year), 7] <- summary(benefit)[5]
  result2[match(i, result2$year), 8] <- summary(benefit)[6]
}
result2 |> write_csv("data/benefit-per1kDB-worst.csv")

# best --------------------------------------------------------------------

result3 <- result
for (i in 2024:2043) {
  print(paste("doing", i))
  
  noprog <- read_csv(paste0("data/", i, "ledger-no-intervention-best.csv"))
  withprog <- read_csv(paste0("data/", i, "ledger-with-intervention-best.csv"))
  
  benefit <- withprog$profit/withprog$Face.amount*1000 - noprog$profit/noprog$Face.amount*1000
  
  result3[match(i, result3$year), 2] <- sum(benefit)
  result3[match(i, result3$year), 3] <- summary(benefit)[1]
  result3[match(i, result3$year), 4] <- summary(benefit)[2]
  result3[match(i, result3$year), 5] <- summary(benefit)[3]
  result3[match(i, result3$year), 6] <- summary(benefit)[4]
  result3[match(i, result3$year), 7] <- summary(benefit)[5]
  result3[match(i, result3$year), 8] <- summary(benefit)[6]
}
result3 |> write_csv("data/benefit-per1kDB-best.csv")

# base analysis ----------------------------------------------------------------

paths <- sprintf("data/%02dledger-no-intervention-best.csv", 2024:2043)
base <- lapply(paths, read_csv)
paths_int <- sprintf("data/%02dledger-with-intervention-best.csv", 2024:2043)
base_int <- lapply(paths_int, read_csv)

## profit margin
for (i in 1:20) {
  temp <- sum(base[[i]]$cashflow)/sum(base[[i]]$prem)
  result[match(i+2023, result$year), 2] <- sum(temp)
  result[match(i+2023, result$year), 3] <- summary(temp)[1]
  result[match(i+2023, result$year), 4] <- summary(temp)[2]
  result[match(i+2023, result$year), 5] <- summary(temp)[3]
  result[match(i+2023, result$year), 6] <- summary(temp)[4]
  result[match(i+2023, result$year), 7] <- summary(temp)[5]
  result[match(i+2023, result$year), 8] <- summary(temp)[6]
}

## price change per 1000 death benefit
for (i in 1:20) {
  temp <- base_int[[i]]$prem/base_int[[i]]$Face.amount*1000 - base[[i]]$prem/base[[i]]$Face.amount*1000
  result[match(i+2023, result$year), 2] <- sum(temp)
  result[match(i+2023, result$year), 3] <- summary(temp)[1]
  result[match(i+2023, result$year), 4] <- summary(temp)[2]
  result[match(i+2023, result$year), 5] <- summary(temp)[3]
  result[match(i+2023, result$year), 6] <- summary(temp)[4]
  result[match(i+2023, result$year), 7] <- summary(temp)[5]
  result[match(i+2023, result$year), 8] <- summary(temp)[6]
}

## no prem increase
for (i in 2024:2043) {
  print(paste("doing", i))
  
  noprog <- read_csv(paste0("data/", i, "ledger-no-intervention.csv"))
  withprog <- read_csv(paste0("data/", i, "ledger-no-prem-increase.csv"))
  
  benefit <- withprog$profit - noprog$profit
  
  result[match(i, result$year), 2] <- sum(benefit)
  result[match(i, result$year), 3] <- summary(benefit)[1]
  result[match(i, result$year), 4] <- summary(benefit)[2]
  result[match(i, result$year), 5] <- summary(benefit)[3]
  result[match(i, result$year), 6] <- summary(benefit)[4]
  result[match(i, result$year), 7] <- summary(benefit)[5]
  result[match(i, result$year), 8] <- summary(benefit)[6]
}
result |> write_csv("data/benefit-base-no-prem-increase.csv")

## profit change per 1000 death benefit
for (i in 1:20) {
  temp <- base_int[[i]]$profit/base_int[[i]]$Face.amount*1000 - base[[i]]$profit/base[[i]]$Face.amount*1000
  result[match(i+2023, result$year), 2] <- sum(temp)
  result[match(i+2023, result$year), 3] <- summary(temp)[1]
  result[match(i+2023, result$year), 4] <- summary(temp)[2]
  result[match(i+2023, result$year), 5] <- summary(temp)[3]
  result[match(i+2023, result$year), 6] <- summary(temp)[4]
  result[match(i+2023, result$year), 7] <- summary(temp)[5]
  result[match(i+2023, result$year), 8] <- summary(temp)[6]
}
result |> write_csv("data/benefit-perDB-base.csv")