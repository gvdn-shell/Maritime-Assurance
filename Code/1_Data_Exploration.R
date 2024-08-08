# Remove all objects in R workspace
rm(list = ls())

ncpus <- 12 # Check according to number of cpus of system
packages <- c("plm","ggplot2","lattice","nlme","DBI","odbc","readxlsb","here","RODBC","plyr","dplyr",
              "tidyr","plotly","here","mice","tidyr","parallel","foreach","doParallel","purrr", "zoo",
              "here", "openxlsx", "WDI", "gapminder", "rworldmap",
              "systemfit", "flexmix", "stringr", "betareg", "extrafont", "sysfonts", "showtext",
              "summarytools",
              "skimr",
              "DataExplorer") #
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages],dependencies = TRUE, Ncpus = ncpus)
}
# Load required packages
invisible(lapply(packages, library, character.only = TRUE))

set.seed(1652)

# Access the proxy settings from environment variables
http_proxy <- Sys.getenv("http_proxy")
Sys.setenv(http_proxy = http_proxy)
Sys.setenv(https_proxy = http_proxy)

###################################################################################################################
###################################################################################################################
###################################################################################################################
# https://dplyr.tidyverse.org/articles/programming.html

# Data Import

data.all <- (read.csv(file = here::here("Data", "Martime_Confidential.csv"),
                      header = TRUE))

head(data.all)

glimpse(data.all)

# dfSummary(data.all)
# 
# skim(data.all)
# 
# create_report(data.all)


# Appears to be a lot of repeats of the same information

data.all <- data.all %>%
  distinct(.keep_all = TRUE) # Remove repeats

contingency_table <- data.all %>%
  filter(!is.na(OBSERVED_RISK_VALUE)) %>%
  count(LAST_UPDATED_BY_USER_ID, OBSERVED_RISK_VALUE) %>%
  group_by(LAST_UPDATED_BY_USER_ID) %>%
  mutate(Percent = n / sum(n)) %>%
  select(-n) %>%
  ungroup() %>%
  pivot_wider(names_from = OBSERVED_RISK_VALUE, values_from = Percent, values_fill = list(Percent = 0)) 

# Calculate mode for each LAST_UPDATED_BY_USER_ID
mode_table <- data.all %>%
  filter(!is.na(OBSERVED_RISK_VALUE)) %>%
  count(LAST_UPDATED_BY_USER_ID, OBSERVED_RISK_VALUE) %>%
  group_by(LAST_UPDATED_BY_USER_ID) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  ungroup() %>%
  rename(Mode_OBSERVED_RISK_VALUE = OBSERVED_RISK_VALUE,
         Mode_Count = n)

# Combine contingency table with mode information
contingency_table_with_mode <- contingency_table %>%
  left_join(mode_table, by = "LAST_UPDATED_BY_USER_ID")


# Melt the contingency table for ggplot
melted_table <- contingency_table %>%
  pivot_longer(cols = -LAST_UPDATED_BY_USER_ID, names_to = "OBSERVED_RISK_VALUE", values_to = "Percent")
melted_table$OBSERVED_RISK_VALUE <- factor(melted_table$OBSERVED_RISK_VALUE, levels = c(0, 1, 3, 10))

# Create the heatmap
ggplot(melted_table, aes(x = OBSERVED_RISK_VALUE, y = LAST_UPDATED_BY_USER_ID, fill = Percent)) +
  geom_tile() +
  # scale_fill_gradientn(
  #   colors = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728"),
  #   values = scales::rescale(c(0, 0.25, 0.5, 1.0))
  # ) +
  scale_fill_gradient(low = "white", high = "#D62728") +
  theme_minimal() +
  labs(title = "Heatmap of OBSERVED_RISK_VALUE Percentages by User",
       x = "Observed Risk Value",
       y = "User ID",
       fill = "Percent")

# Create the bar plot
ggplot(melted_table, aes(x = OBSERVED_RISK_VALUE, y = Percent, fill = OBSERVED_RISK_VALUE)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~LAST_UPDATED_BY_USER_ID) +
  theme_minimal() +
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728")) +
  labs(title = "Distribution of OBSERVED_RISK_VALUE Percentages by User",
       x = "Observed Risk Value",
       y = "Percent",
       fill = "Observed Risk Value")

# Create the dot plot
ggplot(melted_table, aes(x = OBSERVED_RISK_VALUE, y = Percent, color = OBSERVED_RISK_VALUE)) +
  geom_point() +
  facet_wrap(~LAST_UPDATED_BY_USER_ID) +
  theme_minimal() +
  scale_color_manual(values = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728")) +
  labs(title = "Percentage of Each OBSERVED_RISK_VALUE by User",
       x = "Observed Risk Value",
       y = "Percent",
       color = "Observed Risk Value")

mean.diff <- data.all %>%
  group_by(LAST_UPDATED_BY_USER_ID, QUESTION_NUMBER) %>%
  filter(!is.na(OBSERVED_RISK_VALUE), !is.na(FINAL_RISK_VALUE)) %>%
  mutate(Obs_vs_Final_Difference = FINAL_RISK_VALUE - OBSERVED_RISK_VALUE) %>%
  ungroup() %>%
  mutate(Question_Main = as.numeric(str_extract(QUESTION_NUMBER, "^[0-9]+"))) %>%
  group_by(LAST_UPDATED_BY_USER_ID, Question_Main) %>%
  summarize(Mean_Risk_Difference = mean(Obs_vs_Final_Difference, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Question_Main, values_from = Mean_Risk_Difference, values_fill = list(Mean_Risk_Difference = 0)) %>%
  arrange(LAST_UPDATED_BY_USER_ID) %>%
  select(LAST_UPDATED_BY_USER_ID, sort(as.numeric(names(.)[-1]))) %>%
  rename_with(~ paste0("Q", .), -LAST_UPDATED_BY_USER_ID)

# Reshape data for heatmap
heatmap_data <- mean.diff %>%
  pivot_longer(cols = -LAST_UPDATED_BY_USER_ID, 
               names_to = "Question_Main", 
               values_to = "Mean_Risk_Difference") #%>%
  #mutate(Question_Main = as.numeric(str_remove(Question_Main, "^Q")))

# Plot heatmap
ggplot(heatmap_data, aes(x = Question_Main, y = LAST_UPDATED_BY_USER_ID, fill = Mean_Risk_Difference)) +
  geom_tile() +
  scale_fill_gradient(low = "#D62728", high = "#2CA02C", name = "Mean Difference (Final - Observed)") +
  labs(x = "Question Number", y = "User ID", title = "Heatmap of Risk Differences") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
###################################################################
# Yes versus No on Questions

summary_count <- data.all %>%
  group_by(LAST_UPDATED_BY_USER_ID, QUESTION_NUMBER) %>%
  #filter(!is.na(OBSERVED_RISK_VALUE), !is.na(FINAL_RISK_VALUE)) %>%
  #mutate(Obs_vs_Final_Difference = FINAL_RISK_VALUE - OBSERVED_RISK_VALUE) %>%
  ungroup() %>%
  mutate(Question_Main = as.numeric(str_extract(QUESTION_NUMBER, "^[0-9]+"))) %>%
  # Group by LAST_UPDATED_BY_USER_ID and Question_Main
  group_by(LAST_UPDATED_BY_USER_ID, Question_Main, ANSWER_CODE) %>%
  # Count the number of occurrences of each ANSWER_CODE
  summarize(Count = n(), .groups = 'drop') %>%
  # Pivot the data to have YES/NO counts in separate columns
  pivot_wider(names_from = ANSWER_CODE, values_from = Count, values_fill = list(Count = 0))

# View the result
print(summary_count)
  


# Plot heatmap for YES counts
ggplot(summary_count, aes(x = as.factor(Question_Main), y = as.factor(LAST_UPDATED_BY_USER_ID))) +
  geom_tile(aes(fill = YES), color = "white") +
  scale_fill_gradient(low = "white", high = "blue", name = "YES Count") +
  labs(x = "Question Main", y = "User ID", title = "Heatmap of YES Counts by Question and User") +
  theme_minimal()

# Plot heatmap for NO counts
ggplot(summary_count, aes(x = as.factor(Question_Main), y = as.factor(LAST_UPDATED_BY_USER_ID))) +
  geom_tile(aes(fill = NO), color = "white") +
  scale_fill_gradient(low = "white", high = "red", name = "NO Count") +
  labs(x = "Question Main", y = "User ID", title = "Heatmap of NO Counts by Question and User") +
  theme_minimal()



#####################################################

summary_table <- data.all %>%
  group_by(REPORT_ID, QUESTION_NUMBER) %>% # Group by REPORT_ID and Location
  summarize(Count = n(), .groups = 'drop') %>% # Count occurrences and drop grouping
  pivot_wider(names_from = QUESTION_NUMBER, values_from = Count, values_fill = list(Count = 0)) # Pivot to wide format
print(summary_table)

summary_table <- data.all %>%
  group_by(LAST_UPDATED_BY_USER_ID, ASSET_ID) %>% # Group by REPORT_ID and Location
  summarize(Count = n(), .groups = 'drop') %>% # Count occurrences and drop grouping
  pivot_wider(names_from = ASSET_ID, values_from = Count, values_fill = list(Count = 0)) # Pivot to wide format
print(summary_table)


summary_table <- data.all %>%
  group_by(REPORT_ID, ASSET_ID) %>% # Group by REPORT_ID and Location
  summarize(Count = n(), .groups = 'drop') %>% # Count occurrences and drop grouping
  pivot_wider(names_from = ASSET_ID, values_from = Count, values_fill = list(Count = 0)) # Pivot to wide format
print(summary_table)

summary_table <- data.all %>%
  group_by(REPORT_ID) %>%          # Group by REPORT_ID
  summarize(Count = n()) %>%       # Count the number of occurrences
  ungroup()  
print(summary_table)

summary_table <- data.all %>%
  group_by(LAST_UPDATED_BY_USER_ID, QUESTION_NUMBER) %>% 
  mutate(Difference = OBSERVED_RISK_VALUE - FINAL_RISK_VALUE) %>% # If positive, being overly cautious
  summarize(Mean_Risk_Difference = mean(Difference, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = QUESTION_NUMBER, values_from = Mean_Risk_Difference, values_fill = list(Count = 0))


  summarize(Count = n(), .groups = 'drop') %>% # Count occurrences and drop grouping
  pivot_wider(names_from = ASSET_ID, values_from = Count, values_fill = list(Count = 0)) # Pivot to wide format
print(summary_table)


my_summarise <- function(.data, ...) {
  .data %>%
    group_by(...) %>%
    summarise(mass = mean(mass, na.rm = TRUE), height = mean(height, na.rm = TRUE))
}

starwars %>% my_summarise(sex, gender)
