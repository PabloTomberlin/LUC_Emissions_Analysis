#Pablo Tomberlin
#Modeling Land-Use Change Emissions with Hector

#Read in data and load necessary packages
data <- read.csv("selected_GCP.csv")
observations <- read.csv("GML_observations.csv")
library(tidyverse)
library(Metrics)
library(ggsci)
data <- data %>% filter(!(is.na(year)))
#theme_set(theme_bw())

#Plot LUC emissions data
LUC_results <- data %>% filter(GCP == c(2007, 2015, 2024))
tail(LUC_results)
ggplot() +
  geom_line(data = LUC_results,
            aes(year, value, group = GCP),
            linewidth = 0.8) + 
  ggtitle("LUC Emissions") +
  xlab("Year") +
  ylab("Value (ppmv CO2)") +
  theme(text = element_text(size = 12, family = "mono", face = "bold")) +
  scale_color_distiller()
  scale_color_tron()

#Basic Hector run
library(hector)
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

#Combined data frame
results_df <- bind_rows(results, .id = "GCP")
results_df <- bind_rows(results_df, out_default)
head(results_df)

#Plot LUC emissions data
results_df %>%
  filter(GCP == c(2007, 2015, 2024)) -> LUC_results
head(LUC_results)
ggplot() +
  geom_line(data = comparison_plot,
            aes(year, value, color = scenario),
            linewidth = 0.8) + 
  facet_wrap("variable", scales = "free") +
  ggtitle("LUC Emissions") +
  xlab("Year") +
  ylab("Value (ppmv CO2)") +
  theme(text = element_text(size = 12, family = "mono", face = "bold")) +
  scale_color_tron()
#Matilda Analysis
library(matilda)
ini_file <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini_file)
param_sets <- generate_params(core, draws = 100)
print(param_sets)
results <- iterate_model(core, 
                         params = param_sets,
                         save_vars = c("CO2_concentration"), save_years = 1850:2100)
head(results)
ggplot() +
  geom_line(data = results,
            aes(year, value, group = run_number)) +
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
