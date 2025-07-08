#Pablo Tomberlin
#Modeling Land-Use Change Emissions with Hector

#Read in data and load necessary packages
data <- read.csv("selected_GCP.csv")
data_FF <- read.csv("Fossil_Fuels.csv")
observations <- read.csv("GML_observations.csv")
library(tidyverse)
library(Metrics)
library(ggsci)
data <- data %>% filter(!(is.na(year)))
#theme_set(theme_bw())

#Fossil fuel visualization of a lengthening time series(2014, 2019, 2024)
ggplot() +
  geom_line(data = data_FF,
            aes(year, value, color = as.factor(GCP),
            linewidth = 0.8)) +
  facet_wrap("GCP", scales = "fixed") +
  scale_color_gradient()
ggtitle("Fossil Fuel Emissions") +
  xlab("Year") +
  ylab("Billion tonnes C (GtC/yr)") +
  theme(text = element_text(size = 12, family = "mono", face = "bold"))

#Plot LUC emissions data for 3 key GCP releases (first year, historical extension, and most recent year)
LUC_results <- data %>% filter(GCP == c(2007, 2015, 2024))
tail(LUC_results)
ggplot() +
  geom_line(data = LUC_results,
            aes(year, value, group = GCP, color = as.character(GCP)),
            linewidth = 0.8) +
  ggtitle("LUC Emissions") +
  xlab("Year") +
  ylab("Billion tonnes C (GtC/yr)") +
  theme(text = element_text(size = 12, family = "mono", face = "bold")) +
  scale_color_distiller()
  #scale_color_tron()

#Basic Hector run
library(hector)
ini_file <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini_file)
run(core)
out_default <- fetchvars(core, 1959:2100, vars = c(CONCENTRATIONS_CO2(),
                                                   GLOBAL_TAS(),
                                                   VEG_C(),
                                                   SOIL_C()))
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
  results[[as.character(gcp_year)]] <- fetchvars(core, 1959:2100, vars = c(CONCENTRATIONS_CO2(),
                                                                           GLOBAL_TAS(),
                                                                           VEG_C(),
                                                                           SOIL_C()))
  
}

#Combined data frame
results_df <- bind_rows(results, .id = "GCP")
results_df <- bind_rows(results_df, out_default)
head(results_df)

#Plot CO2 concentrations projections and global temperatures
projections <- results_df %>% filter(variable == c("CO2_concentration",
                                                   "global_tas"))

ggplot() +
  geom_line(data = projections,
            aes(year, value, color = GCP),
            linewidth = 0.8) +
  #geom_line(data = projections,
            #aes(year, value, color = GCP),
            #linewidth = 0.8) +
  facet_wrap("variable", scales = "free") +
  ggtitle("CO2 concentrations and global temperatures projections") +
  xlab("Year") +
  ylab("Value (ppmv CO2)") +
  theme(text = element_text(size = 12, family = "mono", face = "bold")) +
  scale_color_tron()

#Uncertainty pt 2
summary_temp <- results_df %>%
  filter(variable == "global_tas") %>%
  filter(year == 2100) %>%
  summarise(
    mean_value = mean(value),
    sd_value = sd(value),
    spread = max(value) - min(value),
    CV = sd_value/mean_value,
    spread_pct = spread/mean_value * 100
  )

summary_CO2 <- results_df %>%
  filter(variable == "CO2_concentration") %>%
  filter(year == 2100) %>%
  summarise(
    mean_value = mean(value),
    sd_value = sd(value),
    spread = max(value) - min(value),
    CV = sd_value/mean_value,
    spread_pct = spread/mean_value * 100
  )

#Uncertainty Analysis (mean, sd, spread, coefficient of variation, spread %)
summary_2100 <- results_df %>%
  filter(year == c(2100, 2024)) %>%
  group_by(variable, year) %>%
  summarise(
    mean_value = mean(value),
    sd_value = sd(value),
    spread = max(value) - min(value),
    CV = sd_value/mean_value,
    spread_pct = spread/mean_value * 100
  )
  

#Visualization for differences in temperatures
results_temp <- results_df %>%
  filter(variable == "global_tas")
results_temp
results_avg <- results_temp %>%
  group_by(year) %>%
  summarize(mean_value = mean(value))
full_df <- left_join(results_temp, results_avg, by = "year")
full_df
diff <- mutate(full_df, subtract = value - mean_value)
tail(diff)

ggplot() +
  geom_line(data = diff,
            aes(year, subtract, color = GCP),
            linewidth = 0.8) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "yellow", linewidth = 0.8) +
  xlab("Year") +
  ylab("Degrees (C)") +
  ggtitle("Difference from average projected temperature") +
  theme(text = element_text(size = 12, family = "sans", face = "bold")) +
  theme_bw()

#Soil and vegetation carbon plots, should go back further in history
soil_veg <- results_df %>%
  filter(variable == c("soil_c", "veg_c"))
ggplot() +
  geom_line(data = soil_veg,
          aes(year, value, color = GCP),
          linewidth = 0.8) +
  facet_wrap("variable", scales = "free") +
  ggtitle("Soil and vegetation carbon projections") +
  xlab("Year") +
  ylab("Pg C") +
  theme(text = element_text(size = 12, family = "mono", face = "bold")) +
  theme_bw()

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
  ) -> summary_matilda

results %>% filter(year == 2024) %>%
  group_by(variable) %>%
  summarise(
    mean_value = mean(value),
    sd_value = sd(value),
    spread = max(value) - min(value),
    CV = sd_value/mean_value,
    spread_pct = spread/mean_value * 100
  ) -> summary_matilda_2
