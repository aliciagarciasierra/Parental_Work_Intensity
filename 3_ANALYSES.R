###########################################################################
########  MAIN ANALYSES ####################################
###########################################################################

# This file runs the main results of the paper

####################################################################################################################
############ STANDARIZATION OF VARIABLES ########################  
####################################################################################################################
# Run after 1_MASTER_NLS.R
data <- data %>%
  mutate(across(c(totalweeksemployed, famsize, recoz, year, ageatint, agechild, wealth, education), 
                ~ as.numeric(scale(.x, center = TRUE, scale = TRUE))))


################################################
################ MODELS ######################
################################################

#-------------- FIRST, OLS  WITH DEMOGRAPHIC CONTROLS
m0 <- feols(recoz~  totalweeksemployed*povstatus  + year
            +ageatint + agechild+ race + sex +famsize + marstatus,data)
etable(m0, cluster="motherID", digits=3)

# Extract the coefficients and confidence intervals
coefficients <- coef(m0)
conf_intervals <- confint(m0, level = 0.95) # 95% confidence intervals

# Combine coefficients and confidence intervals into a table
results_table <- cbind(coefficients, conf_intervals)

# Display the table with three decimal places
print(round(results_table, 3))

#--------------SECOND, OLS  WITH DEMOGRAPHIC CONTROLS AND EDUCATION

m1 <- feols(recoz~ totalweeksemployed*povstatus  + year + race+
              ageatint + agechild+  sex +famsize + marstatus + 
              education,data)
etable(m1, cluster="motherID", digits=3)

# Extract the coefficients and confidence intervals
coefficients <- coef(m1)
conf_intervals <- confint(m1, level = 0.95) # 95% confidence intervals

# Combine coefficients and confidence intervals into a table
results_table <- cbind(coefficients, conf_intervals)

# Display the table with three decimal places
print(round(results_table, 3))

#--------------THIRD, OLS  WITH DEMOGRAPHIC CONTROLS AND EDUCATION AND WEALTH

m2 <- feols(recoz~ totalweeksemployed*povstatus + year+ 
              ageatint+  agechild+race  + sex + famsize + marstatus + 
              wealth + education,data)
etable(m2, cluster="motherID", digits=3)

# Extract the coefficients and confidence intervals
coefficients <- coef(m2)
conf_intervals <- confint(m2, level = 0.95) # 95% confidence intervals

# Combine coefficients and confidence intervals into a table
results_table <- cbind(coefficients, conf_intervals)

# Display the table with three decimal places
print(round(results_table, 3))

#--------------INDIVIDUAL FIXED EFFECTS
m3<- feols(recoz~totalweeksemployed*povstatus +  famsize + marstatus +year |ID,data)
etable(m3, cluster="motherID", digits=3)

# Extract the coefficients and confidence intervals
coefficients <- coef(m3)
conf_intervals <- confint(m3, level = 0.95) # 95% confidence intervals

# Combine coefficients and confidence intervals into a table
results_table <- cbind(coefficients, conf_intervals)

# Display the table with three decimal places
print(round(results_table, 3))

################################################
###############  TABLE 1  ###################
################################################

etable_combined<-etable(m0, m1, m2, m3, digits="r2",
                        signif.code = signif_codes)
print(etable_combined)

################################################################
############### FIGURE 2: INTERACTION GRAPH ###################
################################################################
# Note that this graph can be produced with the original non-standardized variables,
# or with the standardized variables. 
# Results are totally consistent, but the axes range changes.

# Generate predicted values using the original data
data$predicted_recoz <- predict(m3, data)

# Make sure poverty status is a factor
data$povstatus <- as.factor(data$povstatus)

# Plot the interaction effect with confidence intervals
ggplot(data, aes(x = totalweeksemployed, y = predicted_recoz, color = povstatus, group = povstatus)) +
  geom_smooth(method = "lm", se = TRUE,  alpha = 0.2) +
  labs(title = " ",
       x = "Total weeks employed",
       y = "Predicted reading ability",
       color = "Poverty status") +
  scale_color_manual(values = c("0" = "steelblue1", "1" = "tomato2"), 
                     labels = c("0" = "Not in poverty", "1" = "In poverty")) +
  theme_pilot(axis_title_size = 14,
              axis_text_size = 11,
              legend_text_size =12,
              legend_title_size = 12,
              legend_position = "bottom") 


