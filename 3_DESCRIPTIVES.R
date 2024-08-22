
###########################################################################
########  DESCRIPTIVES ####################################
###########################################################################

####################################################################################################################
########### FIGURE 1: AVERAGE WEEKS EMPLOYED BY POVOVERTY STATUS AND YEAR ########
####################################################################################################################

#Compute the average values by group

average<- data %>%
  group_by(year, povstatus) %>%
  summarise(average = mean(totalweeksemployed, na.rm=TRUE)) %>%
  ungroup()

average<-average[complete.cases(average),]

# Define the plot
ggplot(average, aes(x = year, y = average, color = as.factor(povstatus))) +
  geom_point(size = 4, position = position_jitter(width = 0.1, height = 0)) +
  geom_line() +
  scale_color_manual(values = c("steelblue2", "tomato1"), labels = c("Not in Poverty", "In Poverty")) +
  scale_x_continuous(breaks = seq(min(average$year), max(average$year), by = 2)) +
  labs(
    title = " ",
    x = "Year",
    y = "Average Total Weeks Employed",
    color = "Poverty Status"
  ) +
  theme_pilot(axis_title_size = 14,
              axis_text_size = 12,
              legend_text_size =14,
              legend_title_size = 14,
              legend_position = "right") 

####################################################################################################################
########### FIGURE A1: PERCENTAGE OF POVERTY  BY WORKING HOURS########
####################################################################################################################

# Create groups of totalweeksemployed every 10 hours
data$totalweeksemployed_group <- cut(data$totalweeksemployed, breaks = seq(0, max(data$totalweeksemployed) + 10, by = 10), right = FALSE)

# Define custom labels
custom_labels <- levels(data$totalweeksemployed_group)
custom_labels <- sub("\\[", "", custom_labels) # Remove the '[' character
custom_labels <- sub("\\)", "", custom_labels) # Remove the ')' character
custom_labels <- sub(",", " to ", custom_labels) # Replace ',' with ' to'

# Set the custom labels as the levels of the factor
levels(data$totalweeksemployed_group) <- custom_labels
# Calculate the percentage of povstatus = 1 for each group
percentages <- data %>%
  group_by(totalweeksemployed_group) %>%
  summarize(percentage_povstatus_1 = mean(povstatus == 1) * 100)

# Create the plot
ggplot(percentages, aes(x = totalweeksemployed_group, y = percentage_povstatus_1)) +
  geom_bar(stat = "identity", fill = "steelblue1") +
 labs(title = " ",
       x = "Total Weeks Employed (groups of 10 hours)",
       y = "Percentage of the Sample In Poverty") +
  theme_pilot(axis_title_size = 14,
              axis_text_size = 11,
              legend_text_size =12,
              legend_title_size = 12,
              legend_position = "right") 

####################################################################################################################
############ DESCRIPTIVE STATISTICS ########################  
####################################################################################################################

# Function to calculate mean, standard deviation, and range
calc_summary_stats <- function(data) {
  data %>%
    summarise(across(where(is.numeric), list(
      Mean = ~mean(., na.rm = TRUE),
      SD = ~sd(., na.rm = TRUE),
      Min = ~min(., na.rm = TRUE),
      Max = ~max(., na.rm = TRUE)
    ))) %>%
    pivot_longer(everything(), names_to = c(".value", "Variable"), names_sep = "_")
}

# Apply the function to your data
summary_stats <- calc_summary_stats(data)

# Print the summary statistics
summary_stats


