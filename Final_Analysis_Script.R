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

data_FF$GCP <- as.factor(data_FF$GCP)

#Fossil fuel visualization of a lengthening time series(2014, 2019, 2024)
ggplot() +
  geom_line(data = data_FF,
            aes(year, value, color = GCP),
            linewidth = 0.8) +
  facet_wrap("GCP", scales = "fixed") +
  scale_color_discrete() +
  ggtitle("Fossil Fuel Emissions") +
  xlab("Year") +
  ylab("Billion tonnes C (GtC/yr)") +
  theme_bw() +
  theme(text = element_text(size = 12, family = "mono", face = "bold"))

#Plot LUC emissions data for 3 key GCP releases (first year, historical extension, and most recent year)
LUC_results <- data %>% filter(GCP %in% c(2007, 2015, 2024))
LUC_results$GCP <- as.factor(LUC_results$GCP)
ggplot() +
  geom_line(data = LUC_results,
            aes(year, value, group = GCP, color = GCP),
            linewidth = 0.8) +
  ggtitle("LUC Emissions") +
  xlab("Year") +
  ylab("Billion tonnes C (GtC/yr)") +
  theme_bw() +
  theme(text = element_text(size = 12, family = "mono", face = "bold")) +
  scale_color_tron()

#Basic Hector run
library(hector)
ini_file <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini_file)
run(core)
out_default <- fetchvars(core, 1850:2100, vars = c(CONCENTRATIONS_CO2(),
                                                   GLOBAL_TAS(),
                                                   VEG_C(),
                                                   SOIL_C(),
                                                   LUC_EMISSIONS()))
out_default$GCP <- "Hector Default"

#LUC plot with Hector Default as separate line
LUC_default <- out_default %>%
  filter(variable == "luc_emissions")
ggplot() +
  geom_line(data = LUC_results,
            aes(year, value, group = GCP),
            color = "gray",
            linewidth = 0.8) +
  geom_label_repel(aes(label = "Hector Unmodified LUC"),
                   x = 2070,
                   y = 0.1,
                   color = "blue") +
  facet_wrap("variable", scales = "free") +
  geom_line(data = LUC_default,
            aes(year, value, group = GCP),
            color = "blue",
            linewidth = 0.8) +
  ggtitle("LUC Emissions") +
  xlab("Year") +
  ylab("Billion tonnes C (GtC/yr)") +
  theme_bw() +
  theme(text = element_text(size = 12, family = "mono", face = "bold"))

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
  results[[as.character(gcp_year)]] <- fetchvars(core, 1850:2100, vars = c(CONCENTRATIONS_CO2(),
                                                                           GLOBAL_TAS(),
                                                                           VEG_C(),
                                                                           SOIL_C(),
                                                                           LUC_EMISSIONS()))
  
}

#Combined data frame
results_gcp <- bind_rows(results, .id = "GCP")
results_df <- bind_rows(results_gcp, out_default)
head(results_df)

#Plot CO2 concentrations projections and global temperatures
projections <- results_df %>% filter(variable %in% c("CO2_concentration",
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

#Uncertainty calculations
temp_2100 <- results_df %>%
  filter(variable == "global_tas") %>%
  filter(year == 2100) %>%
  summarise(
    mean_value = mean(value),
    sd_value = sd(value),
    margin_error = 1.96 * (sd_value/sqrt(length(unique(results_df$GCP)))),
    spread = max(value) - min(value)
  )

temp_2024 <- results_df %>%
  filter(variable == "global_tas") %>%
  filter(year == 2024) %>%
  summarise(
    mean_value = mean(value),
    sd_value = sd(value),
    margin_error = 1.96 * (sd_value/sqrt(length(unique(results_df$GCP)))),
    spread = max(value) - min(value)
  )

CO2_2100 <- results_df %>%
  filter(variable == "CO2_concentration") %>%
  filter(year == 2100) %>%
  summarise(
    mean_value = mean(value),
    sd_value = sd(value),
    margin_error = 1.96 * (sd_value/sqrt(length(unique(results_df$GCP)))),
    spread = max(value) - min(value)
  )

CO2_2024 <- results_df %>%
  filter(variable == "CO2_concentration") %>%
  filter(year == 2024) %>%
  summarise(
    mean_value = mean(value),
    sd_value = sd(value),
    margin_error = 1.96 * (sd_value/sqrt(length(unique(results_df$GCP)))),
    spread = max(value) - min(value)
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
  theme_bw() +
  theme(text = element_text(size = 12, family = "sans", face = "bold"))
  

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
  theme_bw() +
  theme(text = element_text(size = 12, family = "mono", face = "bold"))

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
    margin_error = 1.96 * (sd_value/10),
    spread = max(value) - min(value),
    CV = sd_value/mean_value,
    spread_pct = spread/mean_value * 100
  ) -> summary_matilda

results %>% filter(year == 2024) %>%
  group_by(variable) %>%
  summarise(
    mean_value = mean(value),
    sd_value = sd(value),
    margin_error = 1.96 * (sd_value/10),
    spread = max(value) - min(value),
    CV = sd_value/mean_value,
    spread_pct = spread/mean_value * 100
  ) -> summary_matilda_2

#95 confidence interval
