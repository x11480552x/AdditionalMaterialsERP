############################### GLM CODE ###############################

# create a new column 'RUC_Category' based on the 'RUC11' column, reducing the granularity
crime_buffer_eps75_NEW <- crime_buffer_eps75_NEW %>%
  mutate(RUC_Category = case_when(
    grepl("Rural", RUC11) ~ "Rural",
    grepl("Urban", RUC11) ~ "Urban",
    TRUE ~ "Other"  
  ))


# histogram of the total_crimes variable
hist_total_crimes <- ggplot(crime_buffer_eps75_NEW, aes(x = total_crimes)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  ggtitle("Histogram of Total Crimes for each DBSCAN Cluster") +
  xlab("Total Crimes") +
  ylab("Frequency") +
  theme_minimal()

# fit the negative binomial model
neg_binomial_model <- glm.nb(total_crimes ~ IMD_Score + ah3ahah_rnk + ah3pubs*pub + 
                               bar * nightclub + convenience + RUC_Category
                             ,
                             data = crime_buffer_eps75_NEW, link='log'
)
summary(neg_binomial_model)
1-pchisq(3968,3264) #model rejects the null hyp that the model is good but this is the best model hahahahaha
qqnorm(residuals(neg_binomial_model))
qqline(residuals(neg_binomial_model), col = "red")
check_model(neg_binomial_model) #general diagnostics for the model

# Fit the Poisson model
poisson_model <- glm(total_crimes ~ IMD_Score + ah3ahah_rnk + ah3pubs*pub + 
                       bar * nightclub + convenience + RUC_Category + area_km2 
                     ,
                     data = crime_buffer_eps75_NEW, family = 'poisson'
)
summary(poisson_model)
1-pchisq(144267,3258) #model doesnt fit well at all!!!
qqnorm(residuals(poisson_model))
qqline(residuals(poisson_model), col = "red")
check_model(poisson_model) #general diagnostics for the model

#negative binomial predictions
predicted_crimes <- predict(neg_binomial_model, newdata = crime_buffer_eps75_NEW)

comparison_df <- data.frame(
  Actual_Crimes = crime_buffer_eps75_NEW$total_crimes,
  Predicted_Crimes = predicted_crimes
)

mae_value <- mae(comparison_df$Actual_Crimes, comparison_df$Predicted_Crimes)
r_squared <- cor(comparison_df$Actual_Crimes, comparison_df$Predicted_Crimes)^2

#evaluation metrics for negative binomial
print(paste("MAE:", mae_value)) #58.832
print(paste("R-squared:", r_squared)) #0.40588

#poisson predictions
predicted_crimes <- predict(poisson_model, newdata = crime_buffer_eps75_NEW)

comparison_df <- data.frame(
  Actual_Crimes = crime_buffer_eps75_NEW$total_crimes,
  Predicted_Crimes = predicted_crimes
)

mae_value <- mae(comparison_df$Actual_Crimes, comparison_df$Predicted_Crimes)
r_squared <- cor(comparison_df$Actual_Crimes, comparison_df$Predicted_Crimes)^2

# evaluation metrics for poisson
print(paste("MAE:", mae_value)) #58.820
print(paste("R-squared:", r_squared)) #0.22852

# new data frame with the scenario values - this is to check that hand calculated values equal R's predict function
new_data <- data.frame(
  IMD_Score = 20,
  ah3ahah_rnk = 5,
  ah3pubs = 2,
  pub = 1,
  bar = 1,
  nightclub = 1,
  convenience = 1,
  RUC_Category = "Urban", 
)

# make predictions using the model
# the 'type = "response"' option gives predictions on the original scale (counts of crimes)
predicted_crimes <- predict(neg_binomial_model, newdata = new_data, type = "response")
print(predicted_crimes)