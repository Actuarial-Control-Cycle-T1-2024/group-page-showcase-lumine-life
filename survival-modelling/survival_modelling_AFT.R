# This file explores both cox & AFT model directly on the age at death or censor of inforce policyholders.
# AFT models were built that satisfied the empirical KM estimates.
# We did not however, finalise in using the AFT generated lifetables but chose alternative approaches.

library(dplyr)
library(ggplot2)
library("survival")
library("survminer")
library("simsurv")
library("flexsurv")
library("eha")
library("simsurv")
library(readr)
library(openxlsx)


df <- read.csv("data/inforce-data-cleaned.csv", header = TRUE)

# Mutate policy tenure
# i.e. length of tenure is either how long someone lived till after starting policy,
# or time till they lapsed
mutate_policy_tenure <- function(year_of_death, year_of_lapse, issue_year, death_indicator, lapse_indicator) {
  result <- numeric(length(year_of_death))  # Initialize a numeric vector to store the results
  
  for (i in seq_along(result)) {
    if (death_indicator[i] == 1) {
      result[i] <- year_of_death[i] - issue_year[i]
    } else if (lapse_indicator[i] == 1) {
      result[i] <- year_of_lapse[i] - issue_year[i]
    } else {
      result[i] <- 2023 - issue_year[i]
    }
  }
  return(result)
}


# Format dataset for modelling
modelling_df <- df %>%
  mutate(policy_tenure = mutate_policy_tenure(Year.of.Death, Year.of.Lapse, Issue.year, Death.indicator, Lapse.Indicator)) %>%
  mutate(age.deathOrLapse = Issue.age + policy_tenure) %>%
  select(-policy_tenure)

# Categorical columns
char_cols <- sapply(modelling_df, is.character)
modelling_df[char_cols] <- lapply(modelling_df[char_cols], factor)
modelling_df['Region'] <- lapply(modelling_df['Region'], factor)


factor_col <- modelling_df %>%
  sapply(is.factor)

# Examine factor levels
modelling_df[factor_col] %>%
  sapply(levels)

# Create backup with all covariates
modelling_df_all <- modelling_df

modelling_df <- modelling_df %>%
  select(-Policy.number, -Issue.year, -Year.of.Death, -Year.of.Lapse, -Cause.of.Death, -Lapse.Indicator, -Issue.age)

# Run a cox-prop model on ALL covariates
mod.all.pop <- coxph(Surv(age.deathOrLapse, Death.indicator) ~
                       .,
                     data=modelling_df)
summary(mod.all.pop)

Base.H <- basehaz(mod.all.pop, centered = FALSE)
plot(Base.H$time, Base.H$hazard, xlab = "t", ylab = "H_0(t)",
     main = "Baseline Hazard Rate", type = "s")

############################# Tenure-Length EDA ################################
# ggplot(modelling_df, aes(x = Issue.age, y = policy_tenure)) +
#   geom_point(aes(color = factor(Death.indicator)))+
#   ggtitle("Censored obs. in red") +
#   theme_bw()

############################# Univariate Analysis: #############################

# REFERENCE: http://www.sthda.com/english/wiki/cox-proportional-hazards-model
covariates <- c("Policy.type", "Sex", "Face.amount", 
                "Smoker.Status", "Urban.vs.Rural",
                "Region")

univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(age.deathOrLapse, Death.indicator)~', x)))

univ_models <- lapply(univ_formulas, function(x){coxph(x, data = modelling_df)})
# Extract data 

univ_results <- lapply(univ_models,
                       function(x){
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         # return(exp(cbind(coef(x),confint(x))))
                       })

res_univariate <- t(as.data.frame(univ_results, check.names = FALSE))
res_df <- as.data.frame(res_univariate)


# Declare Significant Variables -------------------------------------------
sig_var <- res_df %>%
  filter(p.value == 0) %>%
  rownames()

sig_var <- c("Policy.type", "Sex", "Smoker.Status")

sig_var <- sig_var %>%
  append("age.deathOrLapse") %>%
  append("Death.indicator")

######################## Multi-covariate Cox-Model #############################
# Exclude 0 policy_tenure, i.e. people lapsing
final_model_df <- modelling_df[sig_var] 
# %>%
#   filter(policy_tenure != 0)

cox.mod <- coxph(Surv(age.deathOrLapse, Death.indicator) ~
                   .,
                 data=final_model_df)
summary(cox.mod)

ggsurvplot(survfit(cox.mod), palette = "#2E9FDF",
           data=final_model_df,
           ylim=c(0.75, 1),
           ggtheme = theme_minimal()) +
  labs(x="Policy Tenure Till Death")

# Plot Baseline Hazard Rate
Base.H <- basehaz(cox.mod, centered = FALSE)


test.ph <- cox.zph(cox.mod)
##### Fails proportional hazard test!!!!!!!!!!!!
test.ph

ggcoxzph(test.ph)


inforce_surv = Surv(final_model_df$policy_tenure, final_model_df$Death.indicator)
plot(survfit(inforce_surv ~ final_model_df$Smoker.Status), col=c("black", "red"), fun="cloglog")
plot(survfit(inforce_surv ~ final_model_df$Sex), col=c("black", "red"), fun="cloglog")



# Distribution Fits -------------------------------------------------------
# Check distributional fits of different distributions for the data
df_to_evaluate <- T20_model_df

fit_exp<-flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~ 1, dist="exp", data = df_to_evaluate)
fit_weibull<-flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~ 1, dist="weibull", data = df_to_evaluate) # Seems very good fit
fit_gamma<-flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~ 1, dist="gamma", data = df_to_evaluate) # Great fit with warnings
fit_gengamma<-flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~ 1, dist="gengamma", data = df_to_evaluate)
fit_genf<-flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~ 1, dist="genf", data = df_to_evaluate)
fit_lognormal<-flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~1, dist="lnorm", data = df_to_evaluate)
fit_gompertz<-flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~1, dist="gompertz", data = df_to_evaluate)
plot(fit_exp)
plot(fit_weibull)
plot(fit_gamma)
plot(fit_gengamma)
plot(fit_genf)
plot(fit_lognormal)
plot(fit_gompertz)

##################################### AFT-Models ###############################
T20_model_df <- final_model_df %>% 
  filter(Policy.type == 'T20', Sex == 'M') %>%
  select(-Policy.type, -Sex)
# LEFT-OFF

SPWL_model_df <- final_model_df %>% 
  filter(Policy.type == 'SPWL', Sex == 'M') %>% 
  select(-Policy.type, -Sex)



# Segment: T20, Male ------------------------------------------------------
aft_model_df <- final_model_df %>% 
  filter(Policy.type == 'T20', Sex == 'M') %>%
  select(-Policy.type, -Sex)

# Try different distributions for the AFT model
aft_weibull <- flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~ ., dist="weibull", data = aft_model_df)
aft_gengamma <- flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~ ., dist="gengamma", data = aft_model_df)
aft_lnorm <- flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~ ., dist="lnorm", data = aft_model_df)

aft_gengamma$coefficients
fit_gengamma$coefficients

#### Model Evaluation
model_to_evaluate <- aft_gengamma


# coxsnell residue 
cs <- coxsnell_flexsurvreg(model_to_evaluate)
surv <- survfit(Surv(cs$est, aft_model_df$Death.indicator) ~ 1)
plot(surv, fun="cumhaz", xlab='Cox-Snell Residue', main = 'Gengamma T20, Male, Smoker/Non-smoker')
abline(0, 1, col="red")


# Compare weibull and Gamma Fits
plot(aft_gengamma, lwd.obs=1)
residuals(aft_gengamma, type="response")

km <- survfit(Surv(age.deathOrLapse, Death.indicator) ~ Smoker.Status, 
              data=aft_model_df
              )
plot(km, main='Gengamma T20, Male, Smoker/Non-smoker')
lines(aft_gengamma, newdata = data.frame(as.list(c(Smoker.Status = 'S'))), col = 'red')
lines(aft_gengamma, newdata = data.frame(as.list(c(Smoker.Status = 'NS'))), col = 'green')
legend(5, 0.2, legend=c("Smoker", "Non-smoker"), col=c("red", "green"), lty=1:1)


# Segment: SPWL, Male -----------------------------------------------------
aft_model_df <- final_model_df %>% 
  filter(Policy.type == 'SPWL', Sex == 'M') %>% 
  select(-Policy.type, -Sex)

# Try different distributions for the AFT model
aft_weibull <- flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~ ., dist="weibull", data = aft_model_df)
aft_gengamma <- flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~ ., dist="gengamma", data = aft_model_df)

plot(aft_weibull)

AIC_BIC_df <- merge(AIC(aft_weibull, aft_gengamma), BIC(aft_weibull, aft_gengamma), by = 'df')
rownames(AIC_BIC_df) <- c("weibull", "gengamma")
AIC_BIC_df

#### Model Evaluation
model_to_evaluate <- aft_gengamma

# coxsnell residue 
cs <- coxsnell_flexsurvreg(model_to_evaluate)
surv <- survfit(Surv(cs$est, aft_model_df$Death.indicator) ~ 1)
plot(surv, fun="cumhaz", xlab='Cox-Snell Residue', main = 'Gengamma SPWL, Male, Smoker/Non-smoker')
abline(0, 1, col="red")

# Goodness of Fit
km <- survfit(Surv(age.deathOrLapse, Death.indicator) ~ Smoker.Status, 
              data=aft_model_df
)
plot(km, main='Gengamma SPWL, Male, Smoker/Non-smoker')
lines(aft_gengamma, newdata = data.frame(as.list(c(Smoker.Status = 'S'))), col = 'red')
lines(aft_gengamma, newdata = data.frame(as.list(c(Smoker.Status = 'NS'))), col = 'green')

legend(5, 0.2, legend=c("Smoker", "Non-smoker"), col=c("red", "green"), lty=1:1)





# Segment: T20, Female -----------------------------------------------------
aft_model_df <- final_model_df %>% 
  filter(Policy.type == 'T20', Sex == 'F') %>% 
  select(-Policy.type, -Sex)

aft_model_df %>% nrow()

# Try different distributions for the AFT model
aft_weibull <- flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~ ., dist="weibull", data = aft_model_df)
aft_gengamma <- flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~ ., dist="gengamma", data = aft_model_df)
aft_gamma <- flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~ ., dist="gamma", data = aft_model_df)
aft_gompertz <- flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~ ., dist="gompertz", data = aft_model_df)

plot(aft_gengamma)

AIC(aft_weibull, aft_gengamma, aft_gompertz)

#### Model Evaluation
model_to_evaluate <- aft_gengamma

# coxsnell residue 
cs <- coxsnell_flexsurvreg(model_to_evaluate)
surv <- survfit(Surv(cs$est, aft_model_df$Death.indicator) ~ 1)
plot(surv, fun="cumhaz", xlab='Cox-Snell Residue', main = 'Gengamma SPWL, Male, Smoker/Non-smoker')
abline(0, 1, col="red")

# Goodness of Fit
km <- survfit(Surv(age.deathOrLapse, Death.indicator) ~ Smoker.Status, 
              data=aft_model_df
)
plot(km, main='Gengamma T20, Female, Smoker/Non-smoker')
lines(aft_gengamma, newdata = data.frame(as.list(c(Smoker.Status = 'S'))), col = 'red')
lines(aft_gengamma, newdata = data.frame(as.list(c(Smoker.Status = 'NS'))), col = 'green')

legend(5, 0.2, legend=c("Smoker", "Non-smoker"), col=c("red", "green"), lty=1:1)

# Segment: SPWL, Female -----------------------------------------------------
aft_model_df <- final_model_df %>% 
  filter(Policy.type == 'SPWL', Sex == 'F') %>% 
  select(-Policy.type, -Sex)

aft_model_df %>% nrow()

# Try different distributions for the AFT model
aft_weibull <- flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~ ., dist="weibull", data = aft_model_df)
aft_gengamma <- flexsurvreg(Surv(age.deathOrLapse, Death.indicator) ~ ., dist="gengamma", data = aft_model_df)

plot(aft_gengamma)

#### Model Evaluation
model_to_evaluate <- aft_gengamma

# coxsnell residue 
cs <- coxsnell_flexsurvreg(model_to_evaluate)
surv <- survfit(Surv(cs$est, aft_model_df$Death.indicator) ~ 1)
plot(surv, fun="cumhaz", xlab='Cox-Snell Residue', main = 'Gengamma SPWL, Female, Smoker/Non-smoker')
abline(0, 1, col="red")

# Goodness of Fit
km <- survfit(Surv(age.deathOrLapse, Death.indicator) ~ Smoker.Status, 
              data=aft_model_df
)
plot(km, main='Gengamma SPWL, Female, Smoker/Non-smoker')
lines(aft_gengamma, newdata = data.frame(as.list(c(Smoker.Status = 'S'))), col = 'red')
lines(aft_gengamma, newdata = data.frame(as.list(c(Smoker.Status = 'NS'))), col = 'green')

legend(5, 0.2, legend=c("Smoker", "Non-smoker"), col=c("red", "green"), lty=1:1)
# Model Output + Survival Table -------------------------------------------

# data: underlying model data
# model: underlying flexsurvreg model
# new_data: covariates to make prediction on
# mort_data: general lumaria mortality table
create_survival_table <- function(data, model, new_data, mort_data) {
  # Evaluate survival probability
  time_start <- min(data$age.deathOrLapse)
  pr_times <- seq(time_start, 140)
  
  pr <- predict(model, type = 'survival', newdata=new_data, times=pr_times, conf.int =TRUE)
  tdf <- tidyr::unnest(pr, .pred) %>% 
    rename(age = '.time', surv_prob = '.pred_survival', lower_ci = '.pred_lower', upper_ci = '.pred_upper')
  
  S_time_start <- mort_data %>%
    filter(Age == time_start) %>%
    pull(surv_prob)
  
  # Convert S_{x+t} to S_x.
  tdf <- tdf %>%
    mutate(surv_prob = as.numeric(format(surv_prob*S_time_start, scientific=FALSE)),
           lower_ci = format(lower_ci*S_time_start, scientific=FALSE),
           upper_ci = format(upper_ci*S_time_start, scientific=FALSE)
           ) %>% 
    mutate(qx = c(-diff(surv_prob), NA)/surv_prob)
  
  return(tdf)
}

####### T20, Male
T20_Male_NS <- create_survival_table(aft_model_df, aft_gengamma, data.frame(as.list(c(Smoker.Status = 'NS'))), mort_data)
T20_Male_S <- create_survival_table(aft_model_df, aft_gengamma, data.frame(as.list(c(Smoker.Status = 'S'))), mort_data)

write.csv(T20_Male_NS, "mortality_table/T20_Male_NS.csv")
write.csv(T20_Male_S, "mortality_table/T20_Male_S.csv")

####### SPWL, Male
SPWL_Male_NS <- create_survival_table(aft_model_df, aft_gengamma, data.frame(as.list(c(Smoker.Status = 'NS'))), mort_data)
SPWL_Male_S <- create_survival_table(aft_model_df, aft_gengamma, data.frame(as.list(c(Smoker.Status = 'S'))), mort_data)

write.csv(SPWL_Male_NS, "mortality_table/SPWL_Male_NS.csv")
write.csv(SPWL_Male_S, "mortality_table/SPWL_Male_S.csv")

####### T20, Female
T20_Female_NS <- create_survival_table(aft_model_df, aft_gengamma, data.frame(as.list(c(Smoker.Status = 'NS'))), mort_data)
T20_Female_S <- create_survival_table(aft_model_df, aft_gengamma, data.frame(as.list(c(Smoker.Status = 'S'))), mort_data)

write.csv(T20_Female_NS, "mortality_table/T20_Female_NS.csv")
write.csv(T20_Female_S, "mortality_table/T20_Female_S.csv")
####### SPWL, Female
SPWL_Female_NS <- create_survival_table(aft_model_df, aft_gengamma, data.frame(as.list(c(Smoker.Status = 'NS'))), mort_data)
SPWL_Female_S <- create_survival_table(aft_model_df, aft_gengamma, data.frame(as.list(c(Smoker.Status = 'S'))), mort_data)

write.csv(SPWL_Female_NS, "mortality_table/SPWL_Female_NS.csv")
write.csv(SPWL_Female_S, "mortality_table/SPWL_Female_S.csv")
