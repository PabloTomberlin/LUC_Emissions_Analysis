#Pablo Tomberlin
#Modeling Land-Use Change Emissions with Hector

#Read in data and load necessary packages
data <- read.csv("selected_GCP.csv")
observations <- read.csv("GML_observations.csv")
data <- data %>% filter(!(is.na(year)))
library(tidyverse)
library(Metrics)
library(ggsci)
library(matilda)
#theme_set(theme_bw())

#Basic Hector run
ini_file <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini_file)
run(core)
out_default <- fetchvars(core, 1959:2100, vars = CONCENTRATIONS_CO2())
out_default$GCP <- "Hector Default"

#Create list and for loop to run all carbon budgets individually
all_gcp_years <- unique(data$GCP)
results <- list()
for(gcp_year in all_gcp_years){
  isolated_data <- data %>% filter(GCP == gcp_year)
  message("running ", gcp_year)
  message("I see ", nrow(isolated_data), " rows of data")
  setvar(core, isolated_data$year, LUC_EMISSIONS(), isolated_data$value,
         getunits(LUC_EMISSIONS()))
  reset(core)
  run(core)
  results[[as.character(gcp_year)]] <- fetchvars(core, 1959:2100, vars = CONCENTRATIONS_CO2())
  
}

#Matilda Analysis
param_sets <- generate_params(core, draws = 25)
print(param_sets)
results <- iterate_model(core, 
                         params = param_sets,
                         save_vars = c("CO2_concentration"), save_years = 1850:2100)
head(results)
ggplot() +
  geom_line(data = results,
            aes(year, value)) +
  xlab("Year") +
  ylab("Value (ppmv CO2)")

results %>% filter(year == 2100) %>%
  group_by(variable) %>%
  summarise(
    mean_value = mean(value),
    sd_value = sd(value),
    spread = max(value) - min(value),
    CV = sd_value/mean_value,
    spread_pct = spread/mean_value * 100
  ) -> summary_2100
