# Install packages if not already installed
install.packages(c("corrplot"))

# Load the packages
library(tidyverse)  # Data manipulation and visualization
library(lubridate)  # Working with date data

library(corrplot)   # Correlation plots
library(ggplot2)    # Advanced visualizations
library(dplyr)      # Data manipulation

# Load dataset from a CSV file (replace 'your_data.csv' with your actual file)
data <- read.csv("C:\\Users\\Mega Store\\Downloads\\ai_job_market_insights.csv")


# View first few rows of the dataset
head(data)

# Check structure of the data
str(data)

summary(data)

sum(is.na(data))  # Total number of missing values

colSums(is.na(data))  # Number of missing values for each column

sum(duplicated(data))  # Total number of duplicate rows


ggplot(data, aes(x = Industry, fill = AI_Adoption_Level)) + 
  geom_bar(position = "dodge") +
  labs(title = "AI Adoption Levels Across Industries", x = "Industry", y = "Count", fill = "AI Adoption Level")

ggplot(data, aes(x = Industry, fill = AI_Adoption_Level)) + 
  geom_bar(position = "dodge") +
  labs(title = "AI Adoption Levels Across Industries", x = "Industry", y = "Count", fill = "AI Adoption Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels by 45 degrees

library(corrplot)
numeric_data <- data %>% select(Salary_USD)
correlation_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(correlation_matrix, method = "color", title = "Correlation Between Salary and Other Factors")


ggplot(data, aes(x = AI_Adoption_Level, y = Salary_USD, fill = AI_Adoption_Level)) + 
  geom_boxplot() +
  labs(title = "Salary Distribution by AI Adoption Level", x = "AI Adoption Level", y = "Salary (USD)")

ggplot(data, aes(x = Job_Title, fill = Automation_Risk)) + 
  geom_bar(position = "dodge") +
  labs(title = "Automation Risk Across Job Titles", x = "Job Title", y = "Count", fill = "Automation Risk") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


remote_data <- data %>% filter(Remote_Friendly == "Yes")
ggplot(remote_data, aes(x = "", fill = AI_Adoption_Level)) + 
  geom_bar(width = 1, stat = "count") +
  coord_polar("y") +
  labs(title = "Remote-Friendly Jobs by AI Adoption Level")

ggplot(data, aes(x = Industry, fill = Job_Growth_Projection)) + 
  geom_bar(position = "stack") +
  labs(title = "Job Growth Projections Across Industries", x = "Industry", y = "Count", fill = "Job Growth Projection") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data, aes(x = Required_Skills, fill = Industry)) + 
  geom_bar() +
  labs(title = "Skills in Demand by Industry", x = "Skills", y = "Job Count", fill = "Industry") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = Company_Size, fill = AI_Adoption_Level)) + 
  geom_bar(position = "fill") +
  labs(title = "AI Adoption Across Company Sizes", x = "Company Size", y = "Proportion", fill = "AI Adoption Level")


ggplot(data, aes(x = Job_Title, fill = Automation_Risk)) + 
  geom_bar(position = "dodge") +
  labs(title = "Automation Risk by Job Title", x = "Job Title", y = "Job Count", fill = "Automation Risk") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data, aes(x = Company_Size, y = Salary_USD, fill = Company_Size)) + 
  geom_boxplot() +
  labs(title = "Salary Distribution by Company Size", x = "Company Size", y = "Salary (USD)")


ggplot(data, aes(x = AI_Adoption_Level, fill = Remote_Friendly)) + 
  geom_bar(position = "dodge") +
  labs(title = "Remote-Friendly Jobs by AI Adoption Level", x = "AI Adoption Level", y = "Job Count", fill = "Remote Friendly")


ggplot(data, aes(x = Industry, fill = Job_Growth_Projection)) + 
  geom_bar(position = "fill") +
  labs(title = "Job Growth Projections Across Industries", x = "Industry", y = "Proportion", fill = "Job Growth Projection") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data, aes(x = Job_Growth_Projection, fill = Automation_Risk)) + 
  geom_bar(position = "dodge") +
  labs(title = "Job Growth Projection vs Automation Risk", x = "Job Growth Projection", y = "Job Count", fill = "Automation Risk")


ggplot(data, aes(x = Location, y = Salary_USD, fill = Location)) + 
  geom_boxplot() +
  labs(title = "Salary Distribution by Location", x = "Location", y = "Salary (USD)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data, aes(x = Required_Skills, fill = AI_Adoption_Level)) + 
  geom_bar(position = "dodge") +
  labs(title = "Skills Based on AI Adoption Levels", x = "Required Skills", y = "Job Count", fill = "AI Adoption Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotates the x-axis labels for better readability

