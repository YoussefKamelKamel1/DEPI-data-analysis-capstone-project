library(dplyr)
library(ggplot2)

data <- read.csv("MentalHealthSurvey.csv")


str(data)
summary(data)

ggplot(data, aes(x = gender, y = depression)) +
  geom_boxplot() +
  labs(title = "Depression Scores by Gender")

cgpa_to_average <- function(cgpa_range) {
  bounds <- as.numeric(unlist(strsplit(cgpa_range, "-")))
  return(mean(bounds))
}
sleep_to_average <- function(sleep_range) {
  bounds <- as.numeric(unlist(strsplit(gsub(" hrs", "", sleep_range), "-")))
  return(mean(bounds))
}
data <- data %>%
  mutate(
    avg_cgpa = sapply(cgpa, cgpa_to_average),
    avg_sleep = sapply(average_sleep, sleep_to_average)
  )


correlation_matrix <- cor(data %>% select(cgpa, academic_workload, academic_pressure, depression, anxiety))
print(correlation_matrix)





ggplot(data, aes(x = gender, y = avg_cgpa, fill = gender)) +
  geom_boxplot() +
  labs(title = "Average CGPA by Gender", x = "Gender", y = "Average CGPA") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")  # Color palette



ggplot(data, aes(x = academic_year, y = avg_sleep, fill = academic_year)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Sleep Hours by Academic Year", x = "Academic Year", y = "Average Sleep (hrs)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")



ggplot(data, aes(x = avg_sleep, y = avg_cgpa, color = gender)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Average CGPA vs. Average Sleep", x = "Average Sleep (hrs)", y = "Average CGPA") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2")


ggplot(data, aes(x = avg_sleep, fill = gender)) +
  geom_histogram(binwidth = 0.5, position = "dodge", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Average Sleep Hours", x = "Average Sleep (hrs)", y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


ggplot(data, aes(x = avg_cgpa, fill = gender)) +
  geom_histogram(binwidth = 0.1, position = "identity", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Average CGPA by Gender", x = "Average CGPA", y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~ gender)


ggplot(data, aes(x = avg_cgpa)) +
  geom_histogram(binwidth = 0.1, color = "black", fill = "skyblue", alpha = 0.7) +
  labs(title = "Distribution of Average CGPA", x = "Average CGPA", y = "Count") +
  theme_minimal()


ggplot(data, aes(x = avg_sleep)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "lightgreen", alpha = 0.7) +
  labs(title = "Distribution of Average Sleep Hours", x = "Average Sleep (hrs)", y = "Count") +
  theme_minimal()

ggplot(data, aes(x = academic_workload)) +
  geom_histogram(binwidth = 1, color = "black", fill = "salmon", alpha = 0.7) +
  labs(title = "Distribution of Academic Workload", x = "Academic Workload", y = "Count") +
  theme_minimal()



ggplot(data, aes(x = gender, fill = gender)) +
  geom_bar(color = "black", alpha = 0.7) +
  labs(title = "Distribution of Gender", x = "Gender", y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

ggplot(data, aes(x = university, fill = university)) +
  geom_bar(color = "black", alpha = 0.7) +
  labs(title = "Distribution of University", x = "University", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

ggplot(data, aes(x = degree_level, fill = degree_level)) +
  geom_bar(color = "black", alpha = 0.7) +
  labs(title = "Distribution of Degree Level", x = "Degree Level", y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


ggplot(data, aes(x = factor(study_satisfaction), y = avg_cgpa, fill = factor(study_satisfaction))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Average CGPA by Study Satisfaction", x = "Study Satisfaction (1-5)", y = "Average CGPA") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")



ggplot(data, aes(x = factor(academic_workload), y = depression, fill = factor(academic_workload))) +
  geom_violin(alpha = 0.7) +
  labs(title = "Depression by Academic Workload", x = "Academic Workload (1-5)", y = "Depression Level") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


ggplot(data, aes(x = factor(financial_concerns), fill = factor(anxiety))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Anxiety Levels by Financial Concerns", x = "Financial Concerns (1-5)", y = "Proportion") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~ anxiety)


ggplot(data, aes(x = factor(financial_concerns), y = depression, fill = factor(financial_concerns))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Depression Levels by Financial Concerns", x = "Financial Concerns (1-5)", y = "Depression Level") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


ggplot(data, aes(x = factor(study_satisfaction), y = anxiety, fill = factor(study_satisfaction))) +
  geom_violin(alpha = 0.7) +
  labs(title = "Anxiety Levels by Study Satisfaction", x = "Study Satisfaction (1-5)", y = "Anxiety Level") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


ggplot(data, aes(x = factor(campus_discrimination), fill = factor(depression))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Depression Levels by Campus Discrimination", x = "Campus Discrimination (1-5)", y = "Proportion") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~ depression)

ggplot(data, aes(x = factor(academic_year), y = avg_sleep, fill = factor(academic_year))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Average Sleep by Academic Year", x = "Academic Year", y = "Average Sleep (hrs)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

ggplot(data, aes(x = factor(stress_relief_activities), y = anxiety, fill = factor(stress_relief_activities))) +
  geom_violin(alpha = 0.7) +
  labs(title = "Anxiety Levels by Stress Relief Activities", x = "Stress Relief Activities (1-5)", y = "Anxiety Level") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

ggplot(data, aes(x = factor(stress_relief_activities), y = anxiety, fill = factor(stress_relief_activities))) +
  geom_violin(alpha = 0.7) +
  labs(title = "Anxiety Levels by Stress Relief Activities", x = "Stress Relief Activities (1-5)", y = "Anxiety Level") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")




# 1. Are males more than females in IT?
ggplot(data, aes(x = gender, fill = gender)) +
  geom_bar() +
  scale_fill_manual(values = c("blue", "pink")) +
  labs(title = "Count of Males vs. Females in IT", x = "Gender", y = "Count") +
  theme_minimal()

# 2. What is the age distribution in this field?
ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Age Distribution in IT", x = "Age", y = "Count") +
  theme_minimal()

# 3. Which university has more students?
ggplot(data, aes(x = university)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Number of Students per University", x = "University", y = "Count of Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate average CGPA for each group
# Calculate average CGPA for each residential status
cgpa_summary <- data %>%
  group_by(residential_status) %>%
  summarize(mean_cgpa = mean(cgpa, na.rm = TRUE))

# Create a simple bar chart
ggplot(cgpa_summary, aes(x = residential_status, y = mean_cgpa, fill = residential_status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +  # Colors for each group
  labs(title = "Average CGPA: On-Campus vs. Off-Campus Students",
       x = "Residential Status",
       y = "Average Cumulative GPA (CGPA)") +
  theme_minimal()

# 5. Are on-campus students feeling more isolation?
ggplot(data, aes(x = residential_status, y = isolation, fill = residential_status)) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightcoral", "lightcyan")) +
  labs(title = "Isolation Scores by Residential Status", x = "Residential Status", y = "Isolation Score") +
  theme_minimal()

# 6. Is there a specific university with the most depressed students?
ggplot(data %>% filter(depression == 1), aes(x = university, fill = university)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Count of Depressed Students by University", x = "University", y = "Count of Depressed Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7. Is there a social relationships / mental health relation?
ggplot(data, aes(x = social_relationships, y = depression)) +
  geom_point(color = "purple") +
  labs(title = "Social Relationships vs. Depression", x = "Social Relationships Score", y = "Depression Score") +
  theme_minimal()

# 8. Correlation between academic pressure and levels of depression, anxiety, and isolation
correlation_data <- data %>%
  select(academic_pressure, depression, anxiety, isolation)

correlation_matrix <- cor(correlation_data)
heatmap(correlation_matrix, main = "Correlation Heatmap", col = heat.colors(256))

# 9. How do the discrimination acts affect mental health outcomes?
ggplot(data, aes(x = campus_discrimination, fill = factor(depression))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("lightgrey", "darkorange"), 
                    labels = c("Not Depressed", "Depressed")) +
  labs(title = "Mental Health Outcomes by Campus Discrimination", 
       x = "Campus Discrimination (Yes/No)", 
       y = "Count", 
       fill = "Depression Status") +
  theme_minimal()

# 10. Is there a significant relationship between sleep time and academic pressure?
ggplot(data, aes(x = average_sleep, y = academic_pressure)) +
  geom_point(color = "darkblue") +
  labs(title = "Average Sleep vs. Academic Pressure", x = "Average Sleep Time (hours)", y = "Academic Pressure Level") +
  theme_minimal()

# 11. What is the most common stress relief activities and their effectiveness?
stress_relief_effectiveness <- data %>%
  group_by(stress_relief_activities) %>%
  summarize(effectiveness = mean(study_satisfaction, na.rm = TRUE)) # Assuming study_satisfaction represents effectiveness

ggplot(stress_relief_effectiveness, aes(x = reorder(stress_relief_activities, -effectiveness), y = effectiveness, fill = stress_relief_activities)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Effectiveness of Stress Relief Activities", x = "Stress Relief Activity", y = "Effectiveness Score") +
  coord_flip() +
  theme_minimal()

# Split and explode the stress relief activities
data <- data %>%
  mutate(stress_relief_activities = strsplit(as.character(stress_relief_activities), ", ")) %>%
  unnest(stress_relief_activities)  # Explode the list into rows

# Count occurrences of each activity
activity_counts <- data %>%
  count(stress_relief_activities, sort = TRUE)

# Create a count plot
ggplot(activity_counts, aes(y = reorder(stress_relief_activities, n), x = n)) +
  geom_col(fill = "lightblue") +
  labs(title = "Stress Relief Activities Count",
       x = "Count",
       y = "Activities") +
  theme_minimal()




ggplot(data, aes(x = residential_status, y = avg_cgpa)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Comparison of CGPA between On-campus and Off-campus Students",
       x = "Residential Status",
       y = "CGPA") +
  theme_minimal()


ggplot(data, aes(x = university, fill = depression)) +
  geom_bar(position = "dodge") +
  labs(title = "Depression Levels by University",
       x = "University",
       y = "Count of Depressed Students") +
  theme_minimal()


ggplot(data, aes(x = social_relationships, y = depression)) +
  geom_point(aes(color = anxiety, size = isolation)) +
  labs(title = "Relationship between Social Relationships and Mental Health",
       x = "Social Relationships",
       y = "Depression") +
  theme_minimal()


# Correlation plot between academic pressure and mental health indicators (depression, anxiety, isolation)
library(GGally)

# Select relevant columns
mental_health_data <- data[, c("academic_pressure", "depression", "anxiety", "isolation")]

# Correlation matrix using ggpairs
ggpairs(mental_health_data, 
        title = "Correlation between Academic Pressure and Mental Health Issues")


# Box plot for discrimination vs mental health (depression, anxiety, isolation)
ggplot(data, aes(x = campus_discrimination, y = depression)) +
  geom_boxplot(aes(fill = anxiety)) +
  labs(title = "Impact of Discrimination Acts on Mental Health",
       x = "Discrimination Acts",
       y = "Depression") +
  theme_minimal()

# Scatter plot for sleep time vs academic pressure
ggplot(student_data, aes(x = average_sleep, y = academic_pressure)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship between Sleep Time and Academic Pressure",
       x = "Average Sleep (hours)",
       y = "Academic Pressure") +
  theme_minimal()


# Bar plot for most common stress relief activities
ggplot(data, aes(x = stress_relief_activities)) +
  geom_bar(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Most Common Stress Relief Activities",
       x = "Stress Relief Activities",
       y = "Count") +
  theme_minimal()

# Box plot to show effectiveness on reducing depression
ggplot(data, aes(x = stress_relief_activities, y = depression)) +
  geom_boxplot(aes(fill = depression)) +
  labs(title = "Effectiveness of Stress Relief Activities on Reducing Depression",
       x = "Stress Relief Activities",
       y = "Depression Levels") +
  theme_minimal()


# Bar plot for social relationships vs mental health (depression)
ggplot(data, aes(x = factor(social_relationships), fill = factor(depression))) +
  geom_bar(position = "dodge") +
  labs(title = "Social Relationships vs Depression Levels",
       x = "Social Relationships (1 - 5)",
       y = "Count of Students",
       fill = "Depression Level") +
  theme_minimal()


# Bar plot for sleep time vs academic pressure
ggplot(data, aes(x = factor(average_sleep), fill = factor(academic_pressure))) +
  geom_bar(position = "dodge") +
  labs(title = "Average Sleep vs Academic Pressure",
       x = "Average Sleep Time (1 - 5)",
       y = "Count of Students",
       fill = "Academic Pressure Level") +
  theme_minimal()

library(ggplot2)
library(tidyr)
library(dplyr)
library(forcats) 
student_data_split <- data %>%
  mutate(stress_relief_activities = strsplit(as.character(stress_relief_activities), ', ')) %>%
  unnest(stress_relief_activities)

# Create a count plot for stress relief activities
ggplot(student_data_split, aes(y = fct_infreq(stress_relief_activities))) +
  geom_bar(fill = "lightblue", color = "darkblue") +
  labs(title = "Stress Relief Activities Count",
       x = "Count",
       y = "Activities") +
  theme_minimal()



average_depression_by_university <- data %>%
  group_by(university) %>%
  summarise(average_depression = mean(depression, na.rm = TRUE)) %>%
  arrange(average_depression)

# Create a bar plot for average depression level by university
ggplot(average_depression_by_university, aes(x = reorder(university, average_depression), y = average_depression)) +
  geom_bar(stat = "identity", fill = "cyan", color = "black") +
  labs(title = "Average Depression Level by University",
       x = "University",
       y = "Average Depression Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 




ggplot(data, aes(x = residential_status, y = isolation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "On-Campus vs Off-Campus Students Isolation",
       x = "Residential Status",
       y = "Isolation Level") +
  theme_minimal()
