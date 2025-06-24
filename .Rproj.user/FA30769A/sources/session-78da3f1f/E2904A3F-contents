#RMSE shorter script

data <- read.csv("selected_GCP.csv")
observations <- read.csv("GML_observations.csv")
library(tidyverse)
library(Metrics)
data <- data %>% filter(!(is.na(year)))

#basic Hector run
library(hector)
ini_file <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini_file)
run(core)
out_default <- fetchvars(core, 1850:2050, vars = CONCENTRATIONS_CO2())
out_default$GCP <- "Hector Default"
head(out_default)
head(observations)
error <- merge(out_default, observations, by = "year")
rmse(error$value, error$mean)#1.912

#Run all GCP data releases individually
all_gcp_years <- unique(data$GCP)
results <- list()

#for loop
for(gcp_year in all_gcp_years){
  isolated_data <- data %>% filter(GCP == gcp_year)
  message("running ", gcp_year)
  message("I see ", nrow(isolated_data), " rows of data")
  setvar(core, isolated_data$year, LUC_EMISSIONS(), isolated_data$value,
         getunits(LUC_EMISSIONS()))
  reset(core)
  run(core)
  results[[as.character(gcp_year)]] <- fetchvars(core, isolated_data$year, vars = CONCENTRATIONS_CO2())
  #message("I got", results, "emissions")
  #error <- merge(results, observations, by = "year")
  #rmse_values[gcp_year] <- rmse(error$value, error$mean)
}

#rmse
rmse_list <- lapply(results, function(df){
  df_merge <- merge(df, observations, by = "year")
  df_rmse <- rmse(df_merge$value, df_merge$mean)
  return(df_rmse)
})

results_df <- bind_rows(results, .id = "GCP")
results_df <- bind_rows(results_df, out_default)
results_df

#plot
ggplot() +
  geom_line(data = results_df,
            aes(year, value, color = GCP)) +
  facet_wrap("variable", scales = "free") +
  geom_line(data = observations,
            aes(year, mean, color = observations),
            color = "black") +
  ggtitle("CO2 Concentration") +
  xlab("Year") +
  ylab("Value (Pg C/yr)") +
  labs("GCP Release") +
  theme(text = element_text(size = 12, family = "mono", face = "bold"))

