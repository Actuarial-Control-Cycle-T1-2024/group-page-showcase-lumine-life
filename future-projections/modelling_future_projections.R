library(dplyr)
library(ggplot2)
library(tidyr)

set.seed(1)

df_all <- read.csv("data/inforce-data-cleaned.csv", header = TRUE)

# SPWL/T20
df <- df_all %>%
  filter(Policy.type == 'T20') # Change policy_type and rerun below code to
                                # compare

# Check issue year
issue_yr_over_time_df <- df %>%
  group_by(Issue.year) %>%
  count()

ggplot(issue_yr_over_time_df, aes(x = Issue.year, y = n)) +
  geom_point() +
  labs(x = "Year", y = "N")

# Investigate how proportion of each significant covariate varies (EDA) --------
# Check distribution of smokers across issue year
smoker_ratio_per_issue_yr <- df %>%
  group_by(Issue.year) %>%
  summarise(smoker_ratio = mean(Smoker.Status == 'S', na.rm = TRUE))

# Decreasing trend across number of smokers over each issue_year
ggplot(smoker_ratio_per_issue_yr, aes(x = Issue.year, y = smoker_ratio)) +
  geom_point() +
  labs(x = "Issue.year", y = "smoker_ratio")

# Check distribution of gender across issue year
gender_ratio_per_issue_yr <- df %>%
  group_by(Issue.year) %>%
  summarise(gender_ratio = mean(Sex == 'M', na.rm = TRUE))

ggplot(gender_ratio_per_issue_yr, aes(x = Issue.year, y = gender_ratio)) +
  geom_point() +
  labs(x = "Issue.year", y = "gender_ratio")
# From predominantly male dominated policy to roughly even gender distribution

# Check for policy face amount
policy_face_per_issue_yr <- df %>%
  # filter(Smoker.Status == 'S') %>% 
  group_by(Issue.year) %>%
  summarise(min = min(Face.amount, na.rm = TRUE),
            max = max(Face.amount, na.rm = TRUE),
            med = median(Face.amount, na.rm = TRUE),
            mean = mean(Face.amount, na.rm = TRUE),
            std = sd(Face.amount, na.rm = TRUE)
  )

# Plotting face-amount
ggplot(policy_face_per_issue_yr, aes(x = Issue.year)) +
  geom_line(aes(y = min, color = "Min")) +
  geom_line(aes(y = max, color = "Max")) +
  geom_line(aes(y = mean, color = "Mean")) +
  geom_line(aes(y = mean + std, color = "Mean + SD"), linetype = "dashed") +
  geom_line(aes(y = mean - std, color = "Mean - SD"), linetype = "dashed") +
  labs(title = "Face.amount Metrics vs. Issue Year",
       y = "Metric Value") +
  scale_color_manual(values = c("Min" = "blue", "Max" = "red", 
                                "Mean" = "orange", "Mean + SD" = "purple", "Mean - SD" = "purple")) +
  theme_minimal()
# Comment: Face-amount does not seem to be changing much overtime, so we will model it with a
# time independent distribution

# Policy splits
policy_types_per_issue_yr <- df %>%
  group_by(Issue.year) %>%
  summarise(SPWL_Ratio = mean(Policy.type == 'SPWL', na.rm = TRUE))

ggplot(policy_types_per_issue_yr, aes(x = Issue.year, y = SPWL_Ratio)) +
  geom_point() +
  labs(x = "Issue.year", y = "SPWL_Ratio")

# Issue age
issue_ages_per_issue_yr <- df %>%
  # filter(Smoker.Status == 'S') %>% # Smokers have very different behaviour
  group_by(Issue.year) %>%
  summarise(min = min(Issue.age, na.rm = TRUE),
            max = max(Issue.age, na.rm = TRUE),
            med = median(Issue.age, na.rm = TRUE),
            mean = mean(Issue.age, na.rm = TRUE),
            std = sd(Issue.age, na.rm = TRUE)
            )

# Plotting
ggplot(issue_ages_per_issue_yr, aes(x = Issue.year)) +
  geom_line(aes(y = min, color = "Min")) +
  geom_line(aes(y = max, color = "Max")) +
  geom_line(aes(y = med, color = "Median")) +
  geom_line(aes(y = mean, color = "Mean")) +
  geom_line(aes(y = mean + std, color = "Mean + SD"), linetype = "dashed") +
  geom_line(aes(y = mean - std, color = "Mean - SD"), linetype = "dashed") +
  labs(title = "Issue Age Metrics vs. Issue Year",
       y = "Metric Value") +
  scale_color_manual(values = c("Min" = "blue", "Max" = "red", "Median" = "green",
                                "Mean" = "orange", "Mean + SD" = "purple", "Mean - SD" = "purple")) +
  theme_minimal()


# Create correlation heat-map
# Convert categorical variables to numerical using one-hot encoding
df_encoded <- df %>%
  select(Policy.type, Issue.year,Issue.age, Sex, Face.amount, Smoker.Status) %>% 
  mutate_if(is.character, as.factor) %>%
  select(-c(Issue.year)) %>%  # Exclude Issue.year if you don't want to include it in the analysis
  mutate_all(funs(as.numeric))

# Calculate correlation matrix
correlation_matrix <- cor(df_encoded)

# Reshape correlation matrix into long format
correlation_long <- as.data.frame(as.table(correlation_matrix))
names(correlation_long) <- c("Var1", "Var2", "Correlation")

# Plot correlation heatmap
ggplot(data = correlation_long, aes(x = Var1, y = Var2, fill = Correlation, label = sprintf("%.2f", Correlation))) +
  geom_tile(color = "white") +
  geom_text(size = 3, color = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed()
# Comment: Issue age seem to have a correlation with policy_type so we should
# not assume independence and simulate them separately. In general, the policy
# type row seems to be the most linearly correlated with any other covariates.
# Also, Smoker seems to have some correlation with everything else.

# Lastly, check smoking status correlation with gender
df_smoker <- df %>%
  filter(Smoker.Status == "S") %>%
  group_by(Issue.year) %>%
  summarise(male_smoker_prop = mean(Sex == 'M', na.rm=TRUE))

smoker_male_prob <- mean(df_smoker$male_smoker_prop) # If person smoker, then around 80% of them Male


# Regression & Trend Lines: -----------------------------------------------

  # SPWL --------------------------------------------------------------------
  # Number of policies in each new issue year
  projection_time_frame <- 20

  # Plotting the data with a trend line
  ggplot(issue_yr_over_time_df, aes(x = Issue.year, y = n)) +
    geom_point() +  # Scatter plot of the data points
    geom_smooth(method = "lm", se = TRUE) +  # Add a linear trend line without confidence interval
    labs(x = "Year", y = "N") +
    ggtitle("Trend of Issue Year Over Time")  # Title of the plot
  
  # Fit a linear regression model
  lm_model_no_policy <- lm(n ~ Issue.year, data = issue_yr_over_time_df)
  
  # Create a data frame for the next 10 years
  future_years <- data.frame(Issue.year = seq(max(issue_yr_over_time_df$Issue.year) + 1, max(issue_yr_over_time_df$Issue.year) + projection_time_frame))
  
  # Predict the values for the next 10 years using the linear regression model
  predicted_values <- predict(lm_model_no_policy, newdata = future_years, interval = "prediction")
  
  # Combine the future years with the predicted values
  # future_policy_issues_SPWL <- cbind(future_years, predicted_values) %>%
  #   rename(no_of_policies = 'fit') %>%
  #   round()
  future_policy_issues_T20 <- cbind(future_years, predicted_values) %>%
    rename(no_of_policies = 'fit') %>%
    round()
  
  print(future_policy_issues_T20)
  
  ########################## Smoker_ratio future projections ###################
  # Fit the exponential model
  smoker_polynomial_model <- lm(smoker_ratio ~ poly(Issue.year, 2), data = smoker_ratio_per_issue_yr) # SPWL
  smoker_polynomial_model <- lm(smoker_ratio ~ poly(Issue.year, 1), data = smoker_ratio_per_issue_yr) # T20
  
  
  # Plot the data and the fitted exponential curve
  plot(smoker_ratio ~ Issue.year, data = smoker_ratio_per_issue_yr,
       main = "Exponential Fit to Smoker Ratio",
       xlab = "Issue Year", ylab = "Smoker Ratio")
  curve(predict(smoker_polynomial_model, newdata = data.frame(Issue.year = x)),
        add = TRUE, col = "blue", lwd = 2, n = 100)
  
  # Generate new data for prediction (Issue years for 10 years in the future)
  new_data <- data.frame(Issue.year = max(smoker_ratio_per_issue_yr$Issue.year) + 1 + 0:projection_time_frame)
  
  # Predict smoker_ratio values with confidence intervals
  predictions <- predict(smoker_polynomial_model, newdata = new_data, interval = "confidence")
  
  # Create a dataframe with predictions and confidence intervals
  smoker_ratio_predictions_df <- data.frame(
    Issue.year = new_data$Issue.year,
    predicted_smoker_ratio = predictions[, 1],
    lower_bound = predictions[, 2],
    upper_bound = predictions[, 3]
  )
  
  # Impute negative ratios as 0
  smoker_ratio_predictions_df <- smoker_ratio_predictions_df %>%
    sapply(function(x) ifelse(x < 0, 0, x))
  
  # Print the dataframe
  print(smoker_ratio_predictions_df)
  
  ############################ Face amount projection ##########################
  # Non-Smoker:
  df %>%
    filter(Smoker.Status == 'NS') %>%
    select(Face.amount) %>%
    pull() %>%
    hist(freq = FALSE, main = "Density Histogram", xlab = "Data", ylab = "Density")
    
  
  # Create a probability mass function (PMF) function for face amount of NS
  Face_Amount_NS_SPWL <- df %>%
    filter(Smoker.Status == 'NS') %>%
    group_by(Face.amount) %>%
    summarise(count = n()) %>%
    mutate(probability = count / sum(count)) %>%
    select(-count)
  
  Face_Amount_NS_T20 <- df %>%
    filter(Smoker.Status == 'NS') %>%
    group_by(Face.amount) %>%
    summarise(count = n()) %>%
    mutate(probability = count / sum(count)) %>%
    select(-count)
  
  # Smoker:
  # For smokers, average over their last three years in trend
  # Create a probability mass function (PMF) function for face amount of NS
  Face_Amount_S_SPWL <- df %>%
    filter(Smoker.Status == 'S', Issue.year %in% c(2021, 2022, 2023)) %>%
    group_by(Face.amount) %>%
    summarise(count = n()) %>%
    mutate(probability = count / sum(count)) %>%
    select(-count)
  
  Face_Amount_S_T20 <- df %>%
    filter(Smoker.Status == 'S', Issue.year %in% c(2021, 2022, 2023)) %>%
    group_by(Face.amount) %>%
    summarise(count = n()) %>%
    mutate(probability = count / sum(count)) %>%
    select(-count)
 
  # Issue Age
  # Very uniform across different issue years.
  df %>%
    filter(Smoker.Status == 'NS') %>%
    select(Issue.age) %>%
    pull() %>%
    hist(freq = FALSE, main = "Density Histogram", xlab = "Data", ylab = "Density")
  
  Issue_Age_NS_SPWL <- df %>%
    filter(Smoker.Status == 'NS') %>%
    group_by(Issue.age) %>%
    summarise(count = n()) %>%
    mutate(probability = count / sum(count)) %>%
    select(-count)
  

  Issue_Age_S_SPWL <- df %>%
    filter(Smoker.Status == 'S', Issue.year %in% c(2021, 2022, 2023)) %>%
    group_by(Issue.age) %>%
    summarise(count = n()) %>%
    mutate(probability = count / sum(count)) %>%
    select(-count)
  
  Issue_Age_NS_T20 <- df %>%
    filter(Smoker.Status == 'NS') %>%
    group_by(Issue.age) %>%
    summarise(count = n()) %>%
    mutate(probability = count / sum(count)) %>%
    select(-count)
  
  
  Issue_Age_S_T20 <- df %>%
    filter(Smoker.Status == 'S', Issue.year %in% c(2021, 2022, 2023)) %>%
    group_by(Issue.age) %>%
    summarise(count = n()) %>%
    mutate(probability = count / sum(count)) %>%
    select(-count)
    
  
  simulate_function <- function(No_policy_df, smoker_ratio_df, issue_age_pmf_NS, face_amount_pmf_NS,
                            issue_age_pmf_S, face_amount_pmf_S, policy_type
                            ) {
    # Initialise simulation_df
    final_df <- data.frame(
      Policy.type = character(),
      Issue.year = numeric(),
      Issue.age = numeric(),
      Sex = character(),
      Face.amount = numeric(),
      Smoker.Status = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (i in 1:nrow(No_policy_df)) {
      issue_year <- No_policy_df[i, 'Issue.year']
      N <- No_policy_df[i, 'no_of_policies']
      N_smokers <- round(runif(1, min = smoker_ratio_df[i, 'lower_bound'], max = smoker_ratio_df[i, 'upper_bound']) * N)
      N_NS <- N - N_smokers
      smoker_gender_probabilities <- c('M' = smoker_male_prob, 'F' = 1 - smoker_male_prob)
      
      
      sim_df_NS <- data.frame(
        Policy.type = rep(policy_type, N_NS),
        Issue.year = rep(issue_year, N_NS),
        Issue.age = sample(issue_age_pmf_NS$Issue.age, size = N_NS, replace = TRUE, prob = issue_age_pmf_NS$probability),
        Sex = sample(c('M', 'F'), size = N_NS, replace = TRUE),
        Face.amount = sample(face_amount_pmf_NS$Face.amount, size = N_NS, replace = TRUE, prob = face_amount_pmf_NS$probability),
        Smoker.Status = rep('NS', N_NS),
        stringsAsFactors = FALSE
      )
      
      sim_df_S <- data.frame(
        Policy.type = rep(policy_type, N_smokers),
        Issue.year = rep(issue_year, N_smokers),
        Issue.age = sample(issue_age_pmf_S$Issue.age, size = N_smokers, replace = TRUE, prob = issue_age_pmf_S$probability),
        Sex = sample(names(smoker_gender_probabilities), size = N_smokers, replace = TRUE, prob = smoker_gender_probabilities), # Change gender
        Face.amount = sample(face_amount_pmf_S$Face.amount, size = N_smokers, replace = TRUE, prob = face_amount_pmf_S$probability),
        Smoker.Status = rep('S', N_smokers),
        stringsAsFactors = FALSE
      )
      
      # Concatenate result
      final_df <- rbind(final_df, sim_df_NS)
      final_df <- rbind(final_df, sim_df_S)
    }
    
    return(final_df)
  }
  
  SPWL_simulation_result_20 <- simulate_function(future_policy_issues_SPWL, smoker_ratio_predictions_df, 
                Issue_Age_NS_SPWL, Face_Amount_NS_SPWL,
                Issue_Age_S_SPWL, Face_Amount_S_SPWL,
                "SPWL"
                )

  SPWL_simulation_result_20 %>% 
    filter(Smoker.Status == 'S') %>%
    group_by(Sex) %>% 
    count()
  
  SPWL_simulation_result_20 %>% nrow()
  
  # write.csv(SPWL_simulation_result_20, "Future-Projections/SPWL_Future_20Y.csv")
  
  ############## Calculate for T20
  
  T20_simulation_result_20 <- simulate_function(future_policy_issues_T20, smoker_ratio_predictions_df, 
                    Issue_Age_NS_T20, Face_Amount_NS_T20,
                    Issue_Age_S_T20, Face_Amount_S_T20,
                    "T20")
  T20_simulation_result_20 %>% View()
  
  write.csv(T20_simulation_result_20, "Future-Projections/T20_Future_20Y.csv")
  
  ############### Combined result
  final_result <- rbind(T20_simulation_result_20, SPWL_simulation_result_20)

  final_result %>% View()
  
  # write.csv(final_result, "Future-Projections/Combined_Future_20Y.csv")
  # write.csv(final_result, "data/Combined_Future_20Y.csv")
  
