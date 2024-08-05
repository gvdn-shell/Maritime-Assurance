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

dfSummary(data.all)

skim(data.all)

create_report(data.all)

summary_table <- data.all %>%
  group_by(REPORT_ID, ASSET_ID) %>% # Group by REPORT_ID and Location
  summarize(Count = n(), .groups = 'drop') %>% # Count occurrences and drop grouping
  pivot_wider(names_from = ASSET_ID, values_from = Count, values_fill = list(Count = 0)) # Pivot to wide format

summary_table <- data.all %>%
  group_by(REPORT_ID) %>%          # Group by REPORT_ID
  summarize(Count = n()) %>%       # Count the number of occurrences
  ungroup()  
print(summary_table)


my_summarise <- function(.data, ...) {
  .data %>%
    group_by(...) %>%
    summarise(mass = mean(mass, na.rm = TRUE), height = mean(height, na.rm = TRUE))
}

starwars %>% my_summarise(sex, gender)
