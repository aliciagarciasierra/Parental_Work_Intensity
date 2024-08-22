###########################################################################
########  MAIN ANALYSES ####################################
###########################################################################

# ThiS file runs the main models of the paper

################################################
################ MODELS ######################
################################################

# FIRST, OLS  WITH DEMOGRAPHIC CONTROLS
m0 <- feols(recoz~  totalweeksemployed*povstatus  + year
            +ageatint + agechild+ race + sex +famsize + marstatus,data)
etable(m0)

# SECOND, OLS  WITH DEMOGRAPHIC CONTROLS AND EDUCATION

m1 <- feols(recoz~ totalweeksemployed*povstatus  + year + race+
              ageatint + agechild+  sex +famsize + marstatus + 
              education,data)
etable(m1)

# THIRD, OLS  WITH DEMOGRAPHIC CONTROLS AND EDUCATION AND WEALTH

m2 <- feols(recoz~ totalweeksemployed*povstatus + year+ 
              ageatint+  agechild+race  + sex + famsize + marstatus + 
              education + wealth ,data)
etable(m2)


# FOURTH, INDIVIDUAL FIXED EFFECTS


m3<- feols(recoz~totalweeksemployed*povstatus +  famsize + marstatus +year| ID  ,data)
etable(m3)


################################################
###############  TABLE  ###################
################################################

etable_combined<-etable(m0, m1, m2, m3, digits="r2",
                        signif.code = signif_codes)
print(etable_combined)

################################################################
###############  INTERACTION GRAPH ###################
################################################################

# Generate predicted values using the original data
data$predicted_recoz <- predict(m3, data)

# Make sure poverty status is a factor
data$povstatus <- as.factor(data$povstatus)

# Plot the interaction effect with confidence intervals
ggplot(data, aes(x = totalweeksemployed, y = predicted_recoz, color = povstatus, group = povstatus)) +
  geom_smooth(method = "lm", se = TRUE,  alpha = 0.2) +
  labs(title = " ",
       x = "Total Weeks Employed",
       y = "Predicted Reading Ability",
       color = "Poverty Status") +
  scale_color_manual(values = c("0" = "steelblue1", "1" = "tomato2"), 
                     labels = c("0" = "Not in Poverty", "1" = "In Poverty")) +
  theme_pilot(axis_title_size = 14,
              axis_text_size = 11,
              legend_text_size =12,
              legend_title_size = 12,
              legend_position = "right") 


