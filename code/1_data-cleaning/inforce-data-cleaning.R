################################### STAGE 1 ##################################
library(readr)
library(dplyr)
library(tidyr)

orig <- read_csv("soa-materials/2024-srcsc-superlife-inforce-dataset.csv", skip = 3)

df <- orig |> 
  # Modify indicator
  replace_na(list(Death.indicator = 0,
                  Lapse.Indicator = "0")) |> 
  # Replace "Y" in indicator to 1
  mutate(Lapse.Indicator = ifelse(Lapse.Indicator == "Y", 1, Lapse.Indicator)) |> 
  # Convert factors
  mutate(across(c(Policy.type, Sex, Smoker.Status, Underwriting.Class, Urban.vs.Rural,
                  Region, Distribution.Channel, Death.indicator, Lapse.Indicator, Cause.of.Death), 
                as.factor)) |> 
  write_csv("data/inforce-data-cleaned.csv")



################################### STAGE 2 ##################################
library(dplyr)
library(ggplot2)

df <- read.csv("data/inforce-data-cleaned.csv", header = TRUE)

View(df)


## Update unknown cause of deaths in data-frame
unknown_death_mask <- is.na(df$Cause.of.Death) & df$Death.indicator == 1
df$Cause.of.Death[unknown_death_mask] <- 'unknown'



################################### OTHER EDA ##################################
df %>%
  group_by(Policy.type) %>%
  count()

# Show that 
df %>%
  filter(Death.indicator == 1, is.na(Year.of.Death)) %>%
  nrow()


# Proportion of Death by underwriting class
df %>%
  group_by(Underwriting.Class) %>%
  summarize(proportion = mean(Death.indicator)) %>%
  select(Underwriting.Class, proportion)  %>%
  mutate(Underwriting.Class = factor(Underwriting.Class, 
                                     levels = c("high risk", "moderate risk", "low risk", "very low risk"))) %>%
  ggplot(aes(x = Underwriting.Class, y = proportion)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Underwriting Class", y = "Proportion of Death Indicators", 
       title = "Proportion of Deaths by Underwriting Class")

df %>%
  group_by(Underwriting.Class) %>%
  summarize(proportion = mean(Lapse.Indicator)) 

# Distribution of policy benefit amounts across policy type
df %>%
  group_by(Policy.type, Face.amount) %>%
  count()


# Check that unknowns are updated
df %>%
  group_by(Cause.of.Death) %>%
  count()

df %>%
  group_by(Death.indicator) %>%
  count()

write.csv(df, "data/inforce-data-cleaned.csv", row.names=FALSE)

df2 <- read.csv("data/inforce-data-cleaned.csv", header = TRUE)
View(df2)
