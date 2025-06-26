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
out_default <- fetchvars(core, 1979:2024, vars = CONCENTRATIONS_CO2())
prediction_co2 <- tail(out_default, 1)$value
prediction_co2
out_default$GCP <- "Hector Default"

head(out_default)
head(observations)
error <- merge(out_default, observations, by = "year")
rmse(error$value, error$mean)#1.912

#Run all GCP data releases individually
all_gcp_years <- unique(data$GCP)
results <- list()
#prediction_values <- seq(NA, length(all_gcp_years))
#names(rmse_values) <- all_gcp_year

#for loop
for(gcp_year in all_gcp_years){
  isolated_data <- data %>% filter(GCP == gcp_year)
  message("running ", gcp_year)
  message("I see ", nrow(isolated_data), " rows of data")
  setvar(core, isolated_data$year, LUC_EMISSIONS(), isolated_data$value,
         getunits(LUC_EMISSIONS()))
  reset(core)
  run(core)
  results[[as.character(gcp_year)]] <- fetchvars(core, 1979:2024, vars = CONCENTRATIONS_CO2())
  #calculate ppm and % difference at end of century num/real
  #get value at 2100 and put it in
  #prediction_values[] <- tail(out_default, 1)$value
  #message("I got", results, "emissions")
  #error <- merge(results, observations, by = "year")
  #rmse_values[gcp_year] <- rmse(error$value, error$mean)
}

#rmse calculation
rmse_list <- lapply(results, function(df){
  df_merge <- merge(df, observations, by = "year")
  df_rmse <- rmse(df_merge$value, df_merge$mean)
  return(df_rmse)
})

#table for rmse values
df <- as.data.frame(rmse_list)
df <- signif(df, digits = 4)
df_table <- as.data.frame(table(df))

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
  annotate(geom = "table",
           x = 1982,
           y = 440,
           label = list(df_table),
           size = 3) +
  theme(text = element_text(size = 12, family = "mono", face = "bold"))

