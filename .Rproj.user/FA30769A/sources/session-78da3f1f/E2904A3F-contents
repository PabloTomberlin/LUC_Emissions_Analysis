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
out_default <- fetchvars(core, 1850:2050, vars = LUC_EMISSIONS())
out_default$GCP <- "Hector Default"
error <- merge(out_default, observations, by = "year")
rmse(error$value, error$mean)#1.912

#Run all GCP data releases individually
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
  results[[as.character(gcp_year)]] <- fetchvars(core, isolated_data$year, vars = LUC_EMISSIONS())
}
results_df <- bind_rows(results, .id = "GCP")
results_df <- bind_rows(results_df, out_default)
results_df

ggplot() +
  geom_line(data = results_df,
            aes(year, value, color = GCP)) +
  facet_wrap("variable", scales = "free") +
  ggtitle("LUC Emissions") +
  xlab("Year") +
  ylab("Value (Pg C/yr)") +
  labs("GCP Release") +
  theme(text = element_text(size = 12, family = "mono", face = "bold"))

