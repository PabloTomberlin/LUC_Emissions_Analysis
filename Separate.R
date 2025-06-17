#Pablo Tomberlin, separate attempt
setwd("~/LUC_Emissions")
data <- read.csv("selected_GCP.csv")
library(tidyverse)

#basic run
library(hector)
ini_file <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini_file)
run(core)
out_default <- fetchvars(core, 1950:2100, vars = c(LUC_EMISSIONS(),
                                                   CONCENTRATIONS_CO2()))

#2007 run, try setting to 2024 or setting to 2007, also with fetch
isolated_data <- data %>% filter(GCP == 2007)
setvar(core, 1959:2006, LUC_EMISSIONS(), isolated_data$value,
       getunits(LUC_EMISSIONS()))
reset(core)
run(core)
out_07_emissions <- fetchvars(core, 1959:2006, vars = LUC_EMISSIONS())
out_07_concentration <- fetchvars(core, 1900:2100, vars = CONCENTRATIONS_CO2())
head(out_07_concentration)

#plot
out_default[["scenario"]] <- "Hector Default"
out_07_emissions[["scenario"]] <- "2007 Data"
out_07_concentration[["scenario"]] <- "2007 Data"
comparison_plot <- rbind(out_default, out_07_emissions, out_07_concentration)
ggplot(data = comparison_plot, aes(year, value, color = scenario,
                                   linetype = scenario)) + 
  geom_line() + 
  facet_wrap("variable", scales = "free") + ggtitle("LUC Emissions")
ggsave("LUC_vs_Concentrations.png", width = 8, height = 5)
