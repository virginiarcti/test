# Load dplyr package
library(dplyr)

# Assuming 'data' is your data frame
mutated_data1 <- data %>%
  mutate(
    classification = case_when(
      # Classify as "alive and well|alive c's due to fibroids"
      grepl("alive and well", newborn_outcome_within_6_months, ignore.case = TRUE) & 
        !grepl("malaria|admitted|pneumonia|congenital anomalies", newborn_outcome_within_6_months, ignore.case = TRUE) ~ "alive_and_well",
      
      # Classify as "alive and unwell"
      grepl("alive", newborn_outcome_within_6_months, ignore.case = TRUE) & 
        grepl("malaria|admitted|pneumonia|congenital anomalies", newborn_outcome_within_6_months, ignore.case = TRUE) ~ "alive_and_unwell",
      
      # Classify as "died after birth"
      grepl("died|died within|died due to|died at", newborn_outcome_within_6_months, ignore.case = TRUE) ~ "died_after_birth",
      
      # Classify as "still birth"
      grepl("still birth|macerated", newborn_outcome_within_6_months, ignore.case = TRUE) ~ "still_birth",
      
      # Classify as "abortions"
      grepl("abortion|miscarried|miscarriage|aborton", gestation_by_weeks, ignore.case = TRUE) ~ "abortions",
      
      # Handle incorrect gestation values ("wrong number" cases)
      grepl("WRONG NO", gestation_by_weeks, ignore.case = TRUE) ~ NA,
      
      # Default to "unclassified" if no match
      TRUE ~ NA
    )
   
  )
classified_outcomes <- as.factor(data$classification)
summary(classified_outcomes)
# prevalence

mutated_data2 <- mutated_data1 %>%
  mutate(birth_weight_of_the_baby_kg = as.numeric(as.character(birth_weight_of_the_baby_kg)))

mutated_data2 %>%
  summarise( 
    preterm_rate = mean(gestation_by_weeks_delivery < 37, na.rm = TRUE) * 100,
    abortion_rate = mean(classification== "abortions", na.rm = TRUE) * 100,
    stillbirth_rate = mean(classification == "still_birth", na.rm = TRUE) * 100,
    lbw_rate = mean(birth_weight_of_the_baby_kg < 2.5, na.rm = TRUE) * 100)
#relationship
mutated_data2 %>% as.data.frame(mutated_data2)
mutated_data2$lowbirthweight <- ifelse(is.na(mutated_data2$birth_weight_of_the_baby_kg), 
                                       NA, 
                                       ifelse(mutated_data2$birth_weight_of_the_baby_kg < 2.5, 1, 0))
mutated_data2 <- mutated_data2[!is.na(mutated_data2$lowbirthweight), ]

table(mutated_data2$parity)
table(mutated_data2$blood_group)


model <- glm(lowbirthweight ~ age_enrol + gravida+parity + blood_group + history_of_malaria_since_last_visit_enrol, 
             data = mutated_data2, 
             family = binomial)
summary(model)

    
# Combine rare categories in 'parity'
mutated_data2$parity <- factor(mutated_data2$parity, 
                               levels = c("0+0", "0+1", "0+2", "1+0", "1+1", "2+0", "2+1", "3+0","4+0", "3+1",  "5+0", "0+3", "1+4","1+2", "2+2","3+2"),
                               labels = c("0+0", "0+1", "0+2", "1+0", "1+1", "2+0", "2+1","3+0","4+0", "Other", "Other", "Other", "Other", "Other", "Other", "Other"))

# Combine rare categories in 'blood_group'
mutated_data2$blood_group <- factor(mutated_data2$blood_group, 
                                    levels = c("a", "ab", "b", "o", "blank"),
                                    labels = c("a", "ab", "b", "o", NA))


# Random Forest Example
install.packages("randomForest")
library(randomForest)
rf_model <- randomForest(lowbirthweight ~ age_enrol + parity + gravida + blood_group + history_of_malaria_since_last_visit_enrol, 
                         data = mutated_data2, importance = TRUE)

# Print and plot Random Forest Model
print(rf_model)
plot(rf_model)

