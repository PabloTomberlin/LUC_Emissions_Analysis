#Soil and veg carbon

data <- read.csv("selected_GCP.csv")
observations <- read.csv("GML_observations.csv")
library(tidyverse)
data <- data %>% filter(!(is.na(year)))

#basic Hector run
library(hector)
ini_file <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini_file)
run(core)
out_default <- fetchvars(core, 1850:2100, vars = c(VEG_C(), SOIL_C()))
out_default$GCP <- "Hector Default"

#Run all GCP data releases individually
all_gcp_years <- unique(data$GCP)
results <- list()
difference <- list()

#for loop
for(gcp_year in all_gcp_years){
  isolated_data <- data %>% filter(GCP == gcp_year)
  message("running ", gcp_year)
  message("I see ", nrow(isolated_data), " rows of data")
  setvar(core, isolated_data$year, LUC_EMISSIONS(), isolated_data$value,
         getunits(LUC_EMISSIONS()))
  reset(core)
  run(core)
  results[[as.character(gcp_year)]] <- fetchvars(core, 1850:2100, vars = c(VEG_C(), SOIL_C()))
}


results_df <- bind_rows(results, .id = "GCP")
results_df <- bind_rows(results_df, out_default)
head(results_df)


#plot
ggplot() +
  geom_line(data = results_df,
            aes(year, value, color = GCP)) +
  facet_wrap("variable", scales = "fixed") +
  ggtitle("CO2 Concentration") +
  xlab("Year") +
  ylab("Value (Pg C/yr)") +
  theme(text = element_text(size = 12, family = "mono", face = "bold"))

