###########################################################################
########  SENSITIVITY CHECKS  ####################################
###########################################################################

####################################################################################################################
############ STANDARIZATION OF VARIABLES ########################  
####################################################################################################################
# Run after 1_MASTER_NLS.R
data <- data %>%
  mutate(across(c(totalweeksemployed, famsize, recoz, year, ageatint, agechild, wealth, education), 
                ~ as.numeric(scale(.x, center = TRUE, scale = TRUE))))


###########################################################################
########################## TABLE A2: STRATIFY BY GENDER #######################
###########################################################################

# Split the sample by child's gender
split<-split(data, f=data$sex)
boys<-split$`1`
girls<-split$`2`

m0<- feols(recoz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , boys)
etable(m0)

m1<- feols(recoz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , girls)
etable(m1)

# Print confidence intervals
coefficients <- coef(m1)
conf_intervals <- confint(m1, level = 0.95) # 95% confidence intervals
# Combine coefficients and confidence intervals into a table
results_table <- cbind(coefficients, conf_intervals)
# Display the table with three decimal places
print(round(results_table, 3))

coefficients <- coef(m0)
conf_intervals <- confint(m0, level = 0.95) # 95% confidence intervals
# Combine coefficients and confidence intervals into a table
results_table <- cbind(coefficients, conf_intervals)
# Display the table with three decimal places
print(round(results_table, 3))


###########################################################################
########################## TABLE A3: STRATIFY BY RACE  #######################
###########################################################################

# Split the sample by child's race
table(data$race)
split<-split(data, f=data$race)
hisp<-split$`1`
black<-split$`2`
white<-split$`3`

# Models
m0<- feols(recoz~totalweeksemployed*povstatus + famsize + marstatus +year , hisp)
etable(m0)

m1<- feols(recoz~totalweeksemployed*povstatus + famsize + marstatus +year, black)
etable(m1)

m2<- feols(recoz~totalweeksemployed*povstatus + famsize + marstatus +year, white)
etable(m2)

# Print confidence intervals
coefficients <- coef(m1)
conf_intervals <- confint(m1, level = 0.95) # 95% confidence intervals
# Combine coefficients and confidence intervals into a table
results_table <- cbind(coefficients, conf_intervals)
# Display the table with three decimal places
print(round(results_table, 3))

coefficients <- coef(m0)
conf_intervals <- confint(m0, level = 0.95) # 95% confidence intervals
# Combine coefficients and confidence intervals into a table
results_table <- cbind(coefficients, conf_intervals)
# Display the table with three decimal places
print(round(results_table, 3))

coefficients <- coef(m2)
conf_intervals <- confint(m2, level = 0.95) # 95% confidence intervals
# Combine coefficients and confidence intervals into a table
results_table <- cbind(coefficients, conf_intervals)
# Display the table with three decimal places
print(round(results_table, 3))


###########################################################################
########################## TABLE A4: STRATIFY BY LENGTH OF EXPOSURE  #######################
###########################################################################

# Create a variable for long exposure:
data$lag_povstatus<-lag(data$povstatus) #create the lag variable
data$long_exposure<-NA
data$long_exposure[data$povstatus==1 & data$lag_povstatus==1]<-1
data$long_exposure[data$povstatus!=1 | data$lag_povstatus!=1]<-0
table(data$long_exposure)

# Models
m0<- feols(recoz~totalweeksemployed*long_exposure+ famsize + marstatus +year|
             ID  , data)
etable(m0)
# Print confidence intervals
coefficients <- coef(m0)
conf_intervals <- confint(m0, level = 0.95) # 95% confidence intervals
# Combine coefficients and confidence intervals into a table
results_table <- cbind(coefficients, conf_intervals)
# Display the table with three decimal places
print(round(results_table, 3))


###########################################################################
######## TABLE A5 and A6.OLS ASSUMPTIONS  ####################################
###########################################################################

# 1. LINEARITY
m0 <- lm(recoz~ totalweeksemployed*povstatus + year+ 
           ageatint+  agechild+race  + sex + famsize + marstatus + 
           wealth + education,data)
summary(m0)

# Extract residuals and fitted values
residuals_m0 <- resid(m0)
fitted_m0 <- fitted(m0)

# Plot residuals vs fitted values
plot_linearity <- ggplot(data = data.frame(fitted = fitted_m0, residuals = residuals_m0), 
       aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Linearity Check: Residuals vs Fitted", 
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# 2. HOMOCEDASTICITY
m1 <- feols(recoz~ totalweeksemployed*povstatus + year+ 
              ageatint+  agechild+race  + sex + famsize + marstatus + 
              wealth + education,data)

etable(m1, se = "hetero")


#3. NORMALITY OF RESIDUALS
# Q-Q plot for residuals
plot_qq <-ggplot(data = data.frame(residuals = residuals_m0), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Normality Check: Q-Q Plot", 
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

grid.arrange(plot_linearity, plot_qq, 
             ncol = 1,  # Arrange plots in 1 column (stacked vertically)
             top = textGrob(" ", 
                            gp = gpar(fontsize = 16, fontface = "bold")),
             bottom = textGrob("", 
                               gp = gpar(fontsize = 12)))
#4. INDEPENDENCE OF RESIGUALS 
# Perform the Durbin-Watson test
durbinWatsonTest(m0)

#5. MULTICOLLINEARITY
# Calculate VIF
vif_values <- car::vif(m0)
print(vif_values)

# Model without the age of the mother at interview
m2 <- feols(recoz~ totalweeksemployed*povstatus + year+ 
               agechild+race  + sex + famsize + marstatus + 
              wealth + education,data)

etable(m2)


###########################################################################
######## TABLE A7. DYNAMIC PANEL DATA MODEL   ####################################
###########################################################################

# Load necessary libraries
library(plm)
library(lmtest)

# Remove rows with missing values
data_filtered<- na.omit(data[, c("ID", "year", "recoz", "totalweeksemployed", "povstatus", "famsize", "marstatus")])

# Remove IDs with only 1 observation
id_counts <- data %>%
  group_by(ID) %>%
  summarise(count = n())
data_filtered <- data %>%
  semi_join(id_counts %>% filter(count > 1), by = "ID")

#Create the lag variable and check it's okay
data_filtered$recoz_lag <- c(NA, head(data_filtered$recoz, -1))  # manually create the lag
head(data_filtered[, c("recoz", "recoz_lag")])

# Prepare the panel data
pdata <- pdata.frame(data_filtered, index = c("ID", "year"))
pdim(pdata)

# Dynamic panel model 
m_dyn <- pgmm(recoz ~ recoz_lag + totalweeksemployed*povstatus + 
                famsize + marstatus | lag(recoz, 2:5),
              data = pdata, effect = "individual", model = "twosteps")

# Summary of results
summary(m_dyn)


###########################################################################
######## TABLE A8. REMOVING LONE PARENTS ####################################
###########################################################################

table(data$marstatus)

#Remove those who are not married with a partner present
data_filtered <- data[data$marstatus != 0, ]

#Run the analysis
m1<- feols(recoz~totalweeksemployed*povstatus +  famsize +year |ID,data_filtered)
etable(m1,cluster="motherID")

###########################################################################
######## TABLE A9. ALTERNATIVE POVERTY THRESHOLD ####################################
###########################################################################

# Calculate equalized income (using the square root of family size as the equivalence scale)
long$equalized_income <- long$income / sqrt(long$famsize)

# Calculate the median equalized income
median_equalized_income <- median(long$equalized_income, na.rm = TRUE)
median_equalized_income

# Define the at-risk-of-poverty threshold as 60% of the median equalized income
long$poverty_threshold <- 0.6 * median_equalized_income

# Assign 1 if in poverty (income < poverty threshold), else 0
long$at_risk_of_poverty <- as.numeric(long$income < long$poverty_threshold)

# Check the distribution of at-risk-of-poverty (1 for in poverty, 0 for not in poverty)
table(long$at_risk_of_poverty)

# Extract sample with new poverty status

myvars<-c( "recoz", "wealth", "mathz", "agechild", "ageatint", "sex", "race",
           "totalweeksemployed", "povstatus", "year", 
           "famsize", "marstatus", "education",
           "ID", "motherID", "at_risk_of_poverty")

data<-long[myvars]
data<-data[complete.cases(data),]

#Standardization
data <- data %>%
  mutate(across(c(totalweeksemployed, famsize, recoz, year, ageatint, agechild, wealth, education), 
                ~ as.numeric(scale(.x, center = TRUE, scale = TRUE))))

# Models
m3<- feols(recoz~totalweeksemployed*at_risk_of_poverty +  famsize + marstatus +year, data)
etable(m3, cluster="motherID")



###########################################################################
######## TABLE A10. INCOME PERCENTILES CONTROL ####################################
###########################################################################

myvars<-c( "recoz", "wealth", "mathz", "agechild", "ageatint", "sex", "race",
           "totalweeksemployed", "povstatus", "year", 
           "famsize", "marstatus", "education",
           "ID", "motherID", "income")

data<-long[myvars]
data<-data[complete.cases(data),]

data <- data %>%
  mutate(across(c(totalweeksemployed, famsize, recoz, year, ageatint, agechild, wealth, education), 
                ~ as.numeric(scale(.x, center = TRUE, scale = TRUE))))

data$income_percentile <- ntile(data$income, 10)

# Models
m2 <- feols(recoz~ totalweeksemployed*povstatus + year+ 
              ageatint+  agechild+race  + sex + famsize + marstatus + 
              wealth + education + income_percentile,data)
etable(m2, cluster="motherID")
