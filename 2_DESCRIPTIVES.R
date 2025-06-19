
###########################################################################
########  DESCRIPTIVES ####################################
###########################################################################

####################################################################################################################
########### FIGURE 1: AVERAGE WEEKS EMPLOYED BY POVOVERTY STATUS AND YEAR ########
####################################################################################################################
average<- data %>%
  group_by(year, povstatus) %>%
  summarise(average = mean(totalweeksemployed, na.rm=TRUE)) %>%
  ungroup()

average<-average[complete.cases(average),]

# Define the plot
ggplot(average, aes(x = year, y = average, color = as.factor(povstatus))) +
  geom_point(size = 4, position = position_jitter(width = 0.1, height = 0)) +
  geom_line() +
  scale_color_manual(values = c("steelblue2", "tomato1"), labels = c("Not in poverty", "In poverty")) +
  scale_x_continuous(breaks = seq(min(average$year), max(average$year), by = 2)) +
  labs(
    title = " ",
    x = "Year",
    y = "Average total weeks employed",
    color = "Poverty status"
  ) +
  theme_pilot(axis_title_size = 14,
              axis_text_size = 12,
              legend_text_size =14,
              legend_title_size = 14,
              legend_position = "bottom") 

####################################################################################################################
########### FIGURE A1: PERCENTAGE OF POVERTY  BY WORKING HOURS########
####################################################################################################################
# Step 1: Ensure that totalweeksemployed has no NA values and falls within the specified range
data <- data %>%
  filter(!is.na(totalweeksemployed) & totalweeksemployed >= 0)  # Remove rows with NA or negative values in totalweeksemployed

# Step 2: Group totalweeksemployed into categories
# Make sure the max value for breaks is not NA and within range
max_value <- max(data$totalweeksemployed, na.rm = TRUE)

data$totalweeksemployed_group <- cut(data$totalweeksemployed, 
                                     breaks = c(0, 10, 30, 50, 70, 90, max_value), 
                                     right = FALSE, 
                                     include.lowest = TRUE)

# Define custom labels for the categories
custom_labels <- c("0-9 weeks", "10-29 weeks", "30-49 weeks", "50-69 weeks", "70-89 weeks", "90+ weeks")

# Set the custom labels as the levels of the factor
levels(data$totalweeksemployed_group) <- custom_labels

# Step 3: Calculate the percentage of individuals in poverty by year and totalweeksemployed_group
percentage_poverty <- data %>%
  group_by(year, totalweeksemployed_group) %>%
  summarise(percentage_povstatus_1 = mean(povstatus == 1) * 100) %>%
  ungroup()

# Step 4: Plot the overtime trend, with the color representing the totalweeksemployed categories
ggplot(percentage_poverty, aes(x = year, y = percentage_povstatus_1, color = totalweeksemployed_group)) +
  geom_point(size = 4, position = position_jitter(width = 0.1, height = 0)) +  # Add jitter for better visualization of points
  geom_line() +  # Connect the points with a line
  scale_color_manual(values = c("steelblue2", "green", "orange", "red", "purple", "brown"), 
                     labels = custom_labels) +  # Custom colors and labels for the categories
  scale_x_continuous(breaks = seq(min(percentage_poverty$year), max(percentage_poverty$year), by = 2)) +  # X-axis with breaks every 2 years
  labs(
    title = " ",
    x = "Year",
    y = "Percentage of the Sample In Poverty",
    color = "Total Weeks Employed"
  ) +
  theme_pilot(axis_title_size = 14,
              axis_text_size = 12,
              legend_text_size = 14,
              legend_title_size = 14,
              legend_position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

####################################################################################################################
############ TABLE A1: DESCRIPTIVE STATISTICS ########################  
####################################################################################################################

#Run this code to get the descriptive statistics for the standardized variables
#data <- data %>%
#mutate(across(c(totalweeksemployed, famsize, recoz, year, ageatint, agechild, wealth, education), 
#  ~ as.numeric(scale(.x, center = TRUE, scale = TRUE))))

# Function to calculate mean, standard deviation, and range
calc_summary_stats <- function(data) {
  data %>%
    summarise(across(where(is.numeric), list(
      Mean = ~round(mean(., na.rm = TRUE), 3),
      SD = ~round(sd(., na.rm = TRUE), 3),
      Min = ~round(min(., na.rm = TRUE), 3),
      Max = ~round(max(., na.rm = TRUE), 3)
    ))) %>%
    pivot_longer(everything(), names_to = c(".value", "Variable"), names_sep = "_")
}

# Apply the function to your data
summary_stats <- calc_summary_stats(data)

# Print the summary statistics
print(summary_stats)

# Export the summary statistics to an Excel file
write_xlsx(summary_stats, "summary_statistics.xlsx")

# Confirm export
cat("Summary statistics exported to 'summary_statistics.xlsx'")

####################################################################################################################
############ UNADJUSTED DESCRIPTIVES ########################  
####################################################################################################################


# Calculate unadjusted descriptive statistics by 'povstatus'
descriptive_stats <- data %>%
  group_by(povstatus) %>%
  summarise(
    Mean = mean(recoz, na.rm = TRUE),           # Mean of recoz
    SD = sd(recoz, na.rm = TRUE),               # Standard deviation of recoz
    N = n(),                                    # Sample size for each group
    .groups = 'drop'                            # To avoid grouping in the result
  )

print(descriptive_stats)

# Perform t-test to compare the means of 'recoz' between the groups of 'povstatus'
ttest_result <- t.test(recoz ~ povstatus, data = data)

# Print the results of the t-test
print(ttest_result)


# Simple linear regression: recoz ~ totalweeksemployed
regression_model <-feols(recoz ~ totalweeksemployed, data = data)
etable(regression_model)

####################################################################################################################
############ NUMBER OF INDIVIDUALS WHO CHANGE THEIR POVERTY STATUS ########################  
####################################################################################################################

# Ensure data is sorted by ID and Year
data <- data[order(data$ID, data$year), ]

# Create a new variable to detect the change in poverty status
data$change_pov <- c(NA, diff(data$povstatus))

# Filter for changes from 0 to 1 (not in poverty to in poverty)
change_from_0_to_1 <- data[data$change_pov == 1, ]

# Filter for changes from 1 to 0 (in poverty to not in poverty)
change_from_1_to_0 <- data[data$change_pov == -1, ]

# Count the number of individuals (ID) who have at least one change from 0 to 1
individuals_with_change_0_to_1 <- length(unique(change_from_0_to_1$ID))

# Count the number of individuals (ID) who have at least one change from 1 to 0
individuals_with_change_1_to_0 <- length(unique(change_from_1_to_0$ID))

# Sum the counts of both transitions
total_individuals_with_changes <- individuals_with_change_0_to_1 + individuals_with_change_1_to_0

# Display the result
total_individuals_with_changes

# Calculate the total number of unique IDs in the dataset
total_unique_ids <- length(unique(data$ID))

# Calculate the percentage of IDs that experienced a change
percentage_with_changes <- (total_individuals_with_changes / total_unique_ids) * 100

# Display the result
percentage_with_changes
