# This file explores survival modelling using Cox, by mutating just the policy tenure length of a policyholder. In a later file, we explore modelling the age directly.

library(dplyr)
library(ggplot2)
library("survival")
library("survminer")
library("simsurv")
library("flexsurv")
library("eha")
library("simsurv")

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
  mutate(policy_tenure = mutate_policy_tenure(Year.of.Death, Year.of.Lapse, Issue.year, Death.indicator, Lapse.Indicator)) 

# Categorical columns
char_cols <- sapply(modelling_df, is.character)
modelling_df[char_cols] <- lapply(modelling_df[char_cols], factor)


factor_col <- modelling_df %>%
  sapply(is.factor)

# Examine factor levels
modelling_df[factor_col] %>%
  sapply(levels)

# Create backup with all covariates
modelling_df_all <- modelling_df 

modelling_df <- modelling_df %>%
  select(-Policy.number, -Issue.year, -Year.of.Death, -Year.of.Lapse, -Cause.of.Death, -Lapse.Indicator)

# Run a cox-prop model on ALL covariates
mod.all.pop <- coxph(Surv(policy_tenure, Death.indicator) ~
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
covariates <- c("Policy.type", "Issue.age", "Sex", "Face.amount", 
                "Smoker.Status", "Urban.vs.Rural",
                "Region")

univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(policy_tenure, Death.indicator)~', x)))

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

# Look at Distribution.Channel and Underwriting.Class individually since there
# Were some errors if using code above
mod.dischannel <- coxph(Surv(policy_tenure, Death.indicator) ~
                       Distribution.Channel,
                     data=modelling_df)
summary(mod.dischannel)


mod.underwriting <- coxph(Surv(policy_tenure, Death.indicator) ~
                          Underwriting.Class,
                        data=modelling_df)
summary(mod.underwriting)


sig_var <- res_df %>%
  filter(p.value == 0) %>%
  rownames()

sig_var <- sig_var %>%
  append("policy_tenure") %>%
  append("Death.indicator")

######################## Multi-covariate Cox-Model #############################
# Exclude 0 policy_tenure, i.e. people lapsing
final_model_df <- modelling_df[sig_var] %>%
  filter(policy_tenure != 0)

T20_model_df <- final_model_df %>% 
  filter(Policy.type == 'T20', Issue.age==40) %>%
  select(-Policy.type, -Issue.age)
# LEFT-OFF

SPWL_model_df <- final_model_df %>% 
  filter(Policy.type == 'SPWL') %>% 
  select(-Policy.type)


cox.mod <- coxph(Surv(policy_tenure, Death.indicator) ~
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


################### Model effect of smoking on survival probability (Data Visualisation) ###########
### Keeping all other covariates constant.
smoker_df <- with(modelling_df,
                  data.frame(
                    Smoker.Status = c("NS", "S"),
                    Sex = c("F", "F"),
                    Issue.age = rep(mean(Issue.age, na.rm=TRUE), 2),  # Use the calculated mean age
                    Policy.type = c("SPWL", "SPWL")
                  )
)

fit <- survfit(mod.sig.covar, newdata = smoker_df)
ggsurvplot(fit, conf.int = TRUE,
           data = smoker_df,
           legend.labs=c("Smoker=No", "Smoker=Yes"),
           palette = c("#00BA38", "#F8766D"),
           ggtheme = theme_minimal())+
  labs(x= "Policy Tenure until Lapse or Death")

############################## FIT PARA-COX MODELS ################################
# Firstly, fit the SAME cox model using another package
cox <- coxreg(Surv(policy_tenure, Death.indicator) ~
                .,
              data=final_model_df)


### Fit the baseline hazard function according to Wei-bull PropHazard Model

# Warning: Takes long!
cox_weibo <- phreg(Surv(policy_tenure, Death.indicator) ~
                     .,
                   data=final_model_df)

print(cox_weibo)


# Warning: Takes long!
cox_gompertz <- phreg(Surv(policy_tenure, Death.indicator) ~
                        .,
                      data=final_model_df, dist='gompertz')

print(cox_gompertz)
summary(cox_gompertz)

check.dist(cox_weibo, cox)
check.dist(cox_gompertz, cox)
print(cox_gompertz)
AIC(cox_weibo, cox_gompertz, cox)
