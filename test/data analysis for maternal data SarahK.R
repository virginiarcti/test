packages <- c("readxl","ggplot2","dplyr","tidyr")
lapply(packages, require, character.only = TRUE)
install.packages(pkg, dependencies = TRUE)
# Set working directory
setwd("C:/data/to/ceilingnet_modeling")
# Check the current directory
getwd() 
data <- read_xlsx("./data/Ndhiwa mos data_baseline.xlsx",sheet = 3)

#Data Visualization for all data without filtering arm
ggplot(data, aes(x = factor(No.of.LLIN))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Household LLINs",
       x = "Number of LLINs",
       y = "Count") +
  theme_minimal()

ggplot(data, aes(x = factor(num_people))) +
  geom_bar(fill = "gray", color = "black") +
  labs(title = "Household Size Count",
       x = "Number of People",
       y = "Count") +
  theme_minimal()

#Data visualization according to arm
ggplot(data, aes(x = num_people, fill = arm)) +
  geom_histogram(binwidth = 5, position = "dodge", alpha=6) +
  labs(title = "Number of People",
       x = "Number of People",
       y = "Count") +
  theme_minimal()

ggplot(data, aes(x = No.of.LLIN, fill = arm)) +
  geom_histogram(binwidth = 5, position = "dodge", alpha=6) +
  labs(title = "Household LLINs",
       x = "Number of LLINs",
       y = "Count") +
  theme_minimal()

filtered_data <- data %>%
  filter(!is.na(No.of.female_anopheles))
View(filtered_data)

ggplot(filtered_data, aes(x = No.of.female_anopheles, fill = arm)) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +
  labs(title = "Distribution of Female Anopheles Mosquitoes",
       x = "Number of Female Anopheles",
       y = "Count") +
  theme_minimal()