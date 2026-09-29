# Install necessary necessary data cleaning packages 
install.packages(c("tidyverse", "janitor"))
library(tidyverse)
library(janitor)
library(readxl)
data<-X241011_1st_deli_Visit_data
view(data)
head(data)
str(data)
summary(data)
#Standardize column names to lowercase and remove spaces
data <- data %>% clean_names() 
#Identify missing values
# Count missing values per column
colSums(is.na(dataf))
#convert categorical variables to factors
library(dplyr)
data <- data %>%
  mutate(across(where(is.character), as.factor))
summary(data)
#dplyr and tidyverse or base R to convert all character variables in a dataset into factors.
# Remove duplicate rows
data <- data %>% distinct() 
#save cleaned data set
write_csv(data, "cleaned_data.csv")
#descriptive
data %>%
  # More detailed summary
  skim(data)
#visualization
install.packages(c("tidyverse", "ggplot2", "corrplot", "ggpubr"))

#lower case
# List of columns to convert to lowercase
colnames(data)
cols_to_lower <- c("history_of_malaria_since_last_visit_enrol", "sleep_under_net_enrol" ,  "blood_group" , "rhesus" , "syphilis_enrol", "tb_screening_enrol" , "hiv_enrol" , "urinalysis_enrol","newborn_outcome_within_6_months")

# Apply tolower() to each of these columns
data[cols_to_lower] <- lapply(data[cols_to_lower], tolower)
#visualization
library(ggplot2)
library(tidyverse)
library(corrplot)
library(ggpubr)
library(dplyr)

# Calculate prevalence of adverse outcomes
library(dplyr)

# Example vector of outcomes
outcomes <- c("alive and well", "alive had malaria for 1 month", "Died within 48 hrs", 
              "Fresh still birth", "alive admitted for pneumonia", "alive and well")
# Ensure outcomes is a data frame or tibble
outcomes_df <- data.frame(outcomes)

# Apply the classification using mutate
updated_outcomes <- outcomes_df %>%
  mutate(classification = case_when(
    grepl("alive and well", outcomes, ignore.case = TRUE) ~ "alive and well",
    grepl("alive.*malaria|admitted|pneumonia|congenital anomalies", outcomes, ignore.case = TRUE) ~ "alive and unwell",
    grepl("died|died within|died due to|died at", outcomes, ignore.case = TRUE) ~ "died after birth",
    grepl("still birth|macerated", outcomes, ignore.case = TRUE) ~ "still birth",
    TRUE ~ NA_character_  # Default for unclassified cases
  ))
# Assuming your original data frame is named 'data' and has a column 'OUTCOME WITHIN 6 MONTHS'
data <- data %>%
  mutate(classification = case_when(
    grepl("alive and well|alive c's due to fibroids", newborn_outcome_within_6_months, ignore.case = TRUE) ~ "alive and well",
    grepl("alive.*malaria|admitted|pneumonia|congenital anomalies", newborn_outcome_within_6_months, ignore.case = TRUE) ~ "alive and unwell",
    grepl("died|died within|died due to|died at", newborn_outcome_within_6_months, ignore.case = TRUE) ~ "died after birth",
    grepl("still birth|macerated", newborn_outcome_within_6_months, ignore.case = TRUE) ~ "still birth",
    TRUE ~ NA_character_  # Default for unclassified cases
  ))
view(data)
# Load dplyr package
library(dplyr)

# Filter the data to remove rows where 'classification' is NA
filtered_data_no_na <- data %>%
  filter(!is.na(classification))

# View the filtered data
print(filtered_data_no_na)
view(filtered_data_no_na)
is.na(filtered_data_no_na$classification)

# View the updated data with the new classification column
head(data)



# Print result
updated_outcomes


####
library(dplyr)
library(janitor)
data <- data %>%clean_names()
view(data)
data<-X241011_1st_deli_Visit_data
data %>%
  summarise( 
    preterm_rate = mean(gestation_by_weeks_delivery < 37, na.rm = TRUE) * 100,
    abortion_rate = mean(newborn_outcome_within_6_months== "ABORTION", na.rm = TRUE) * 100,
    stillbirth_rate = mean(newborn_outcome_within_6_months == "stillbirth", na.rm = TRUE) * 100,
    lbw_rate = mean(birth_weight_of_the_baby_kg< 2.5, na.rm = TRUE) * 1
    