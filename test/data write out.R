rm(list=ls())

#importdataset
library(readxl)
cleaned_data <- read_excel("C:/Users/VIRGINIAH/Desktop/final/cleaned data 23 May.xlsx", 
                                  sheet = "cleaned data 23 May")
View(cleaned_data)
##clean names
library(janitor)
library(dplyr)

cleaned_data <- cleaned_data %>%
  clean_names() %>%
  mutate(across(where(is.factor), ~ tolower(as.character(.)))) %>%  # factors → char → lower
  mutate(across(where(is.character), tolower))                      # chars → lower


#convert all char to factors
cleaned_data <- cleaned_data %>%
  mutate(across(where(is.character), as.factor))
cleaned_data <- cleaned_data %>%
  mutate(anemic = factor(anemic, levels = c(0, 1), labels = c("No", "Yes")))

library(dplyr)

gravidity_category <- cleaned_data %>%
  mutate(gravidity = as.numeric(gravida)) %>%
  mutate(gravidity_category = case_when(
    gravida == 1 ~ "Primigravida",
    gravida >= 2 ~ "Multigravida",
    TRUE ~ NA_character_
  ))

#AGE GROUP VS BABY DEAD OR ALIVE
library(dplyr)
library(tidyr)

analyze_age_vs_baby_status <- function(data, var1 = "age_group", var2 = "baby_dead_alive_in_6_months") {
  # Filter out rows with NA in either variable
  filtered_data <- data %>%
    filter(!is.na(.data[[var1]]), !is.na(.data[[var2]]))
  
  # Create contingency table
  tab <- table(filtered_data[[var1]], filtered_data[[var2]], useNA = "no")
  
  # Counts with totals
  tab_df <- as.data.frame.matrix(tab) %>%
    mutate(Total = rowSums(.))
  col_totals <- colSums(tab_df)
  tab_df <- bind_rows(tab_df, col_totals)
  rownames(tab_df)[nrow(tab_df)] <- "Column Totals"
  print(tab_df)
  
  # Column proportions with totals
  prop_tab <- prop.table(tab, margin = 2)
  prop_df <- as.data.frame.matrix(prop_tab) %>%
    mutate(Total = rowSums(.))
  prop_col_totals <- colSums(prop_df)
  prop_df <- bind_rows(prop_df, prop_col_totals)
  rownames(prop_df)[nrow(prop_df)] <- "Column Totals"
  print(round(prop_df, 3))
  
  # Fisher's exact test
  fisher_result <- fisher.test(tab)
  print(fisher_result)
  
  invisible(list(counts = tab_df, proportions = prop_df, fisher_test = fisher_result))
}
result <- analyze_age_vs_baby_status(cleaned_data)
#VISUALISATION 
library(ggplot2)
library(dplyr)

# Filter data
filtered_data <- cleaned_data %>%
  filter(!is.na(age_group), !is.na(baby_dead_alive_in_6_months))

# Optional mean line (commented out for now)
mean_count <- filtered_data %>%
  group_by(baby_dead_alive_in_6_months) %>%
  summarise(mean_count = n() / n_distinct(age_group))

# Plot
plot1<-ggplot(filtered_data, aes(x = age_group, fill = baby_dead_alive_in_6_months)) +
  geom_bar(position = position_dodge()) +
  labs(
    y = "Number of Births",
    x = "Age Group",
    fill = "Baby Status",
    title = "Birth Status per Age Group"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c("alive" = "darkgreen", "dead" = "red"),
    labels = c("Alive", "Dead")
  )
#age_group vs low_birth_weight
library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Filter and factor
filtered_data <- cleaned_data %>%
  filter(!is.na(age_group), !is.na(low_birth_weight)) %>%
  mutate(low_birth_weight = factor(low_birth_weight))  # Keep 0/1 as factors

# Step 2: Contingency table + totals
tab <- table(filtered_data$age_group, filtered_data$low_birth_weight, useNA = "no")
tab_df <- as.data.frame.matrix(tab) %>%
  mutate(Total = rowSums(.))
col_totals <- colSums(tab_df)
tab_df <- bind_rows(tab_df, col_totals)
rownames(tab_df)[nrow(tab_df)] <- "Column Totals"
print(tab_df)

# Step 3: p-value
p_val <- if (any(tab < 5)) {
  fisher.test(tab)$p.value
} else {
  chisq.test(tab)$p.value
}
cat("\nP-value:", round(p_val, 4), "\n")

# Step 4: Assign plot
plot2 <- ggplot(filtered_data, aes(x = age_group, fill = low_birth_weight)) +
  geom_bar(position = position_dodge()) +
  labs(
    y = "Number of Births",
    x = "Age Group",
    fill = "Low Birth Weight",
    title = paste0("Low Birth Weight by Age Group (p = ", round(p_val, 4), ")")
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("1" = "purple", "0" = "orange"),
                    labels = c("1" = "Yes", "0" = "No"))

# Display plot
plot2
#age groups vs preterm
library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Filter data and convert preterm to factor (keep 0 and 1)
filtered_data <- cleaned_data %>%
  filter(!is.na(age_group), !is.na(preterm)) %>%
  mutate(preterm = factor(preterm))  # Keep 0/1 as factor

# Step 2: Create frequency table with row and column totals
tab <- table(filtered_data$age_group, filtered_data$preterm, useNA = "no")
tab_df <- as.data.frame.matrix(tab) %>%
  mutate(Total = rowSums(.))
col_totals <- colSums(tab_df)
tab_df <- bind_rows(tab_df, col_totals)
rownames(tab_df)[nrow(tab_df)] <- "Column Totals"
print(tab_df)

# Step 3: Statistical test
p_val <- if (any(tab < 5)) {
  fisher.test(tab)$p.value
} else {
  chisq.test(tab)$p.value
}
cat("\nP-value:", round(p_val, 4), "\n")

# Step 4: Plot and assign to plot3
plot3 <- ggplot(filtered_data, aes(x = age_group, fill = preterm)) +
  geom_bar(position = position_dodge()) +
  labs(
    y = "Number of Births",
    x = "Age Group",
    fill = "Preterm",
    title = paste0("Preterm Births by Age Group (p = ", round(p_val, 4), ")")
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("1" = "tomato", "0" = "steelblue"),
                    labels = c("1" = "Yes", "0" = "No"))

# View the plot
plot3
#blood group full vs birth status
library(dplyr)
library(tidyr)
library(ggplot2)

# Filter and prepare data using correct bloodgroup_full column
filtered_data <- cleaned_data %>%
  filter(!is.na(bloodgroup_full), !is.na(baby_dead_alive_in_6_months)) %>%
  mutate(baby_dead_alive_in_6_months = factor(baby_dead_alive_in_6_months))

# Create contingency table
tab <- table(filtered_data$bloodgroup_full, filtered_data$baby_dead_alive_in_6_months, useNA = "no")

# Convert to data frame and add totals
tab_df <- as.data.frame.matrix(tab) %>%
  mutate(Total = rowSums(.))

col_totals <- colSums(tab_df)
tab_df <- bind_rows(tab_df, col_totals)
rownames(tab_df)[nrow(tab_df)] <- "Column Totals"

print(tab_df)

# Perform Chi-squared or Fisher's exact test
p_val <- if(any(tab < 5)) {
  fisher.test(tab)$p.value
} else {
  chisq.test(tab)$p.value
}
cat("\nP-value:", round(p_val, 4), "\n")

# Plot with proper blood group levels, assign to plot4
plot4 <- ggplot(filtered_data, aes(x = bloodgroup_full, fill = baby_dead_alive_in_6_months)) +
  geom_bar(position = position_dodge()) +
  labs(
    y = "Number of Births",
    x = "Blood Group",
    fill = "Baby Status",
    title = paste0("Baby Status by Blood Group (p = ", round(p_val, 4), ")")
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("alive" = "forestgreen", "dead" = "firebrick"),
                    labels = c("Alive", "Dead"))

plot4
#blood group full vs low birth weight
library(dplyr)
library(tidyr)
library(ggplot2)

plot_low_birth_weight_vs_bloodgroup <- function(data) {
  # Prepare data
  filtered_data <- data %>%
    filter(!is.na(bloodgroup_full), !is.na(low_birth_weight)) %>%
    mutate(low_birth_weight = factor(low_birth_weight, levels = c(0,1), labels = c("No", "Yes")))
  
  # Contingency table
  tab <- table(filtered_data$bloodgroup_full, filtered_data$low_birth_weight)
  
  # Add totals for display
  tab_df <- as.data.frame.matrix(tab) %>%
    mutate(Total = rowSums(.))
  
  col_totals <- colSums(tab_df)
  tab_df <- bind_rows(tab_df, col_totals)
  rownames(tab_df)[nrow(tab_df)] <- "Column Totals"
  
  print(tab_df)
  
  # Statistical test: Fisher if any cell < 5, else Chi-square
  p_val <- if(any(tab < 5)) {
    fisher.test(tab)$p.value
  } else {
    chisq.test(tab)$p.value
  }
  cat("\nP-value:", round(p_val, 4), "\n")
  
  # Plot
  plot5 <- ggplot(filtered_data, aes(x = bloodgroup_full, fill = low_birth_weight)) +
    geom_bar(position = position_dodge()) +
    labs(
      y = "Number of births",
      x = "Blood Group",
      fill = "Low Birth Weight",
      title = paste0("Distribution of Low Birth Weight by Blood Group (p = ", round(p_val, 4), ")")
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("No" = "orange", "Yes" = "purple"))
  
  return(plot5)
}
plot5 <- plot_low_birth_weight_vs_bloodgroup(cleaned_data)
print(plot5)

#blood group full vs preterm
library(dplyr)
library(ggplot2)

plot_bloodgroup_vs_preterm <- function(data) {
  filtered_data <- data %>%
    filter(!is.na(bloodgroup_full), !is.na(preterm)) %>%
    mutate(preterm = factor(preterm, levels = c(0,1), labels = c("No", "Yes")))
  
  # Contingency table
  tab <- table(filtered_data$bloodgroup_full, filtered_data$preterm)
  
  # Add totals for display (optional)
  tab_df <- as.data.frame.matrix(tab) %>%
    mutate(Total = rowSums(.))
  
  col_totals <- colSums(tab_df)
  tab_df <- bind_rows(tab_df, col_totals)
  rownames(tab_df)[nrow(tab_df)] <- "Column Totals"
  
  print(tab_df)
  
  # Statistical test: Fisher if any cell < 5, else Chi-sq
  p_val <- if(any(tab < 5)) {
    fisher.test(tab)$p.value
  } else {
    chisq.test(tab)$p.value
  }
  cat("\nP-value:", round(p_val, 4), "\n")
  
  # Plot
  ggplot(filtered_data, aes(x = bloodgroup_full, fill = preterm)) +
    geom_bar(position = position_dodge()) +
    labs(
      y = "Number of births",
      x = "Blood Group",
      fill = "Preterm Status",
      title = paste0("Preterm Birth by Blood Group (p = ", round(p_val, 4), ")")
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("No" = "skyblue", "Yes" = "tomato"))
}
plot6 <- plot_bloodgroup_vs_preterm(cleaned_data)
print(plot6)

#weight category vs birth status
library(dplyr)
library(ggplot2)
library(tidyr)

# Prepare data: filter and factorize
plot7_data <- cleaned_data %>%
  filter(!is.na(weight_category), !is.na(baby_dead_alive_in_6_months)) %>%
  mutate(
    weight_category = factor(weight_category, levels = c("normal", "obese", "overweight", "underweight")),
    baby_status = factor(baby_dead_alive_in_6_months, levels = c(0, 1), labels = c("Alive", "Dead"))
  )

# Contingency table
tab <- table(plot7_data$weight_category, plot7_data$baby_dead_alive_in_6_months)

# Summarize by baby_status (column totals) — percent within baby status groups
summary_table <- as.data.frame.matrix(tab) %>%
  mutate(Total = rowSums(.)) %>%
  bind_rows(colSums(.)) %>%
  tibble::rownames_to_column(var = "weight_category")

# Calculate percentages within each baby_status (column-wise percentages)
percent_table <- prop.table(tab, margin = 2) * 100
percent_df <- as.data.frame(as.table(percent_table)) %>%
  tidyr::pivot_wider(names_from = Var2, values_from = Freq) %>%
  rename(weight_category = Var1)

print(percent_df)

# Significance test: Fisher if any expected <5 else Chi-square
p_val <- if(any(tab < 5)) {
  fisher.test(tab)$p.value
} else {
  chisq.test(tab)$p.value
}

cat("P-value:", round(p_val, 4), "\n")

# Plot without text labels
plot7 <- ggplot(plot7_data, aes(x = weight_category, fill = baby_dead_alive_in_6_months)) +
  geom_bar(position = position_dodge(width = 0.9), width = 0.7) +
  labs(
    y = "Number of Births",
    x = "Weight Category",
    fill = "Baby Status",
    title = paste0("Baby Status by Weight Category")
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("alive" = "skyblue", "dead" = "firebrick"))

plot7

#weight category vs low birth weight

plot8_data <- cleaned_data %>%
  filter(!is.na(weight_category), !is.na(baby_dead_alive_in_6_months), !is.na(low_birth_weight)) %>%
  mutate(
    weight_category = factor(weight_category, levels = c("normal", "obese", "overweight", "underweight")),
    baby_status = factor(baby_dead_alive_in_6_months, levels = c(0, 1), labels = c("Alive", "Dead")),
    low_birth_weight = factor(low_birth_weight, levels = c(0, 1), labels = c("No", "Yes"))
  )

plot7

#weight category vs birth status
library(dplyr)
library(ggplot2)
library(tidyr)

library(dplyr)
library(tidyr)

# Prepare data
plot7_data <- cleaned_data %>%
  filter(!is.na(weight_category), !is.na(baby_dead_alive_in_6_months)) %>%
  mutate(
    weight_category = factor(weight_category, levels = c("normal", "obese", "overweight", "underweight")),
    baby_status = factor(baby_dead_alive_in_6_months, levels = c(0, 1), labels = c("Alive", "Dead"))
  )

# Create summary table with percentages
summary_table <- plot7_data %>%
  group_by(weight_category, baby_status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(weight_category) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  select(-n) %>%
  pivot_wider(names_from = baby_status, values_from = percent, values_fill = 0)

print(summary_table)

# Contingency table for significance test
contingency <- table(plot7_data$weight_category, plot7_data$baby_status)

# Test for significance
p_val <- if(any(contingency < 5)) {
  fisher.test(contingency)$p.value
} else {
  chisq.test(contingency)$p.value
}
cat("\nP-value:", round(p_val, 4), "\n")


###    Plot All in a grid


######   Outcome: Birth Status (Dead or alive)

generate_plots_deadoralive <- function(data, predictors) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(glue)
  library(cowplot)
  
  outcome <- "baby_dead_alive_in_6_months"  # fixed outcome column name
  
  plot_list <- list()
  pvalues <- data.frame(predictor = character(), p_value = numeric(), stringsAsFactors = FALSE)
  
  for (pred in predictors) {
    # filter NA values explicitly on the columns:
    plot_data <- data %>%
      filter(!is.na(.data[[pred]]) & !is.na(.data[[outcome]])) %>%
      mutate(
        predictor = as.factor(.data[[pred]]),
        outcome_factor = factor(.data[[outcome]], levels = c("alive", "dead"), labels = c("Alive", "Dead"))
      )
    
    # summary table
    summary_table <- plot_data %>%
      group_by(predictor, outcome_factor) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(predictor) %>%
      mutate(percent = round(n / sum(n) * 100, 1)) %>%
      select(-n) %>%
      pivot_wider(names_from = outcome_factor, values_from = percent, values_fill = 0)
    
    print(glue("Summary Table for {pred} vs {outcome}:"))
    print(summary_table)
    
    # contingency test
    contingency <- table(plot_data$predictor, plot_data$outcome_factor)
    p_val <- if(any(contingency < 5)) {
      fisher.test(contingency)$p.value
    } else {
      chisq.test(contingency)$p.value
    }
    
    # Append p-value to data frame
    pvalues <- rbind(pvalues, data.frame(predictor = pred, p_value = p_val))
    
    # plot
    p <- ggplot(plot_data, aes(x = predictor, fill = outcome_factor)) +
      geom_bar(position = "fill") +
      labs(
        # title = paste0(outcome, " by ", pred, " (p = ", round(p_val, 4), ")"),
        x = pred,
        y = "Proportion",
        fill = outcome
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot_list[[pred]] <- p
  }
  
  return(list(plots = plot_list, pvalues = pvalues))
}


predictors <- c(
  "general_examination_enrol", "respiratory_enrol","cardiovascular_enrol",
  "history_of_malaria_since_last_visit_enrol", "if_yes_indicate_treatment",
  "sleep_under_net_enrol", "syphilis_enrol", "hiv_enrol", "urinalysis_enrol",
  "anemic", "random_sugar_class", "parasitemia_ul_enrol_category","weight_category",
  "age_group","bloodgroup_full"
)

plots.deadoralive <- generate_plots_deadoralive (cleaned_data, predictors)

pvalues1<-plots.deadoralive$pvalues
pvalues1

plot_grid(plotlist=plots.deadoralive$plots[1:9], ncol=3, nrow = 3)
plot_grid(plotlist=plots.deadoralive$plots[10:15], ncol=3, nrow = 3)

######   Outcome: Low birth weight

generate_plots_lowbirthweight <- function(data, predictors) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(glue)
  library(cowplot)
  
  outcome <- "low_birth_weight"  # fixed outcome column name
  
  plot_list <- list()
  pvalues <- data.frame(predictor = character(), p_value = numeric(), stringsAsFactors = FALSE)
  
  for (pred in predictors) {
    # filter NA values explicitly on the columns:
    plot_data <- data %>%
      filter(!is.na(.data[[pred]]) & !is.na(.data[[outcome]])) %>%
      mutate(
        predictor = as.factor(.data[[pred]]),
        outcome_factor = factor(.data[[outcome]], levels = c(0, 1), labels = c("No", "Yes"))
      )
    
    # summary table
    summary_table <- plot_data %>%
      group_by(predictor, outcome_factor) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(predictor) %>%
      mutate(percent = round(n / sum(n) * 100, 1)) %>%
      select(-n) %>%
      pivot_wider(names_from = outcome_factor, values_from = percent, values_fill = 0)
    
    print(glue("Summary Table for {pred} vs {outcome}:"))
    print(summary_table)
    
    # contingency test
    contingency <- table(plot_data$predictor, plot_data$outcome_factor)
    p_val <- if(any(contingency < 5)) {
      fisher.test(contingency)$p.value
    } else {
      chisq.test(contingency)$p.value
    }
    
    # Append p-value to data frame
    pvalues <- rbind(pvalues, data.frame(predictor = pred, p_value = p_val))
    
    # plot
    p <- ggplot(plot_data, aes(x = predictor, fill = outcome_factor)) +
      geom_bar(position = "fill") +
      labs(
        # title = paste0(outcome, " by ", pred, " (p = ", round(p_val, 4), ")"),
        x = pred,
        y = "Proportion",
        fill = outcome
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot_list[[pred]] <- p
  }
  
  return(list(plots = plot_list, pvalues = pvalues))
}


predictors <- c(
  "general_examination_enrol", "respiratory_enrol","cardiovascular_enrol",
  "history_of_malaria_since_last_visit_enrol", "if_yes_indicate_treatment",
  "sleep_under_net_enrol", "syphilis_enrol", "hiv_enrol", "urinalysis_enrol",
  "anemic", "random_sugar_class", "parasitemia_ul_enrol_category","weight_category",
  "age_group","bloodgroup_full"
)

plots.lowbirthweight <- generate_plots_lowbirthweight(cleaned_data, predictors)

pvalues2<-plots.lowbirthweight$pvalues
pvalues2

plot_grid(plotlist=plots.lowbirthweight$plots[1:9], ncol=3, nrow = 3)
plot_grid(plotlist=plots.lowbirthweight$plots[10:15], ncol=3, nrow = 3)


######   Outcome: Preterm

generate_plots_preterm <- function(data, predictors) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(glue)
  library(cowplot)
  
  outcome <- "preterm"  # fixed outcome column name
  
  plot_list <- list()
  pvalues <- data.frame(predictor = character(), p_value = numeric(), stringsAsFactors = FALSE)
  
  for (pred in predictors) {
    # filter NA values explicitly on the columns:
    plot_data <- data %>%
      filter(!is.na(.data[[pred]]) & !is.na(.data[[outcome]])) %>%
      mutate(
        predictor = as.factor(.data[[pred]]),
        outcome_factor = factor(.data[[outcome]], levels = c(0, 1), labels = c("No", "Yes"))
      )
    
    # summary table
    summary_table <- plot_data %>%
      group_by(predictor, outcome_factor) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(predictor) %>%
      mutate(percent = round(n / sum(n) * 100, 1)) %>%
      select(-n) %>%
      pivot_wider(names_from = outcome_factor, values_from = percent, values_fill = 0)
    
    print(glue("Summary Table for {pred} vs {outcome}:"))
    print(summary_table)
    
    # contingency test
    contingency <- table(plot_data$predictor, plot_data$outcome_factor)
    p_val <- if(any(contingency < 5)) {
      fisher.test(contingency)$p.value
    } else {
      chisq.test(contingency)$p.value
    }
    
    # Append p-value to data frame
    pvalues <- rbind(pvalues, data.frame(predictor = pred, p_value = p_val))
    
    # plot
    p <- ggplot(plot_data, aes(x = predictor, fill = outcome_factor)) +
      geom_bar(position = "fill") +
      labs(
        # title = paste0(outcome, " by ", pred, " (p = ", round(p_val, 4), ")"),
        x = pred,
        y = "Proportion",
        fill = outcome
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot_list[[pred]] <- p
  }
  
  return(list(plots = plot_list, pvalues = pvalues))
}

predictors <- c(
  "general_examination_enrol", "respiratory_enrol","cardiovascular_enrol",
  "history_of_malaria_since_last_visit_enrol", "if_yes_indicate_treatment",
  "sleep_under_net_enrol", "syphilis_enrol", "hiv_enrol", "urinalysis_enrol",
  "anemic", "random_sugar_class", "parasitemia_ul_enrol_category","weight_category",
  "age_group","bloodgroup_full"
)

plots.preterm <- generate_plots_preterm(cleaned_data, predictors)

pvalues3<-plots.preterm$pvalues
pvalues3

plot_grid(plotlist=plots.preterm$plots[1:9], ncol=3, nrow = 3)
plot_grid(plotlist=plots.preterm$plots[10:15], ncol=3, nrow = 3)

pvalues<-cbind(pvalues1,pvalues2,pvalues3)
write.csv(pvalues,"C:/Users/VIRGINIAH/Desktop/final/P Values.csv")
