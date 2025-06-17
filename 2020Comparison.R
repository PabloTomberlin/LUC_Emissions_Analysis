#Pablo Tomberlin, predicting the future again
setwd("~/LUC_Emissions")
data <- read.csv("selected_GCP.csv")
library(tidyverse)
#initial plot
#data %>%
#ggplot(aes(x = year, y = value, group = GCP, color = GCP)) + geom_line() +
#minimal_theme()

#Isolate one GCP release
isolated_data <- data %>% filter(GCP == 2024)
head(isolated_data)

#basic run, I choose the years
library(hector)
ini_file <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini_file)
run(core)
#error below
out1 <- fetchvars(core, isolated_data$year:2023, vars = c(LUC_EMISSIONS(),
                                                          CONCENTRATIONS_CO2()))
head(out1)
out1
#ggplot(data = out1, aes(year, value)) + 
#geom_line()

#modified data from 2024 GCP
setvar(core, isolated_data$year:2023, LUC_EMISSIONS(), isolated_data$value,
       getunits(LUC_EMISSIONS()))
core
reset(core)
run(core)

#test it out
out2 <- fetchvars(core, 1850:2023, vars = c(LUC_EMISSIONS(),
                                                          CONCENTRATIONS_CO2()))
head(out2)
#2007
isolated_data_2 <- data %>% filter(GCP == 2007)
head(isolated_data_2)
isolated_data_2
setvar(core, isolated_data_2$year, LUC_EMISSIONS(), isolated_data_2$value,
       getunits(LUC_EMISSIONS()))
core
reset(core)
run(core)
out3 <- fetchvars(core, isolated_data_2$year, vars = c(LUC_EMISSIONS(),
                                            CONCENTRATIONS_CO2()))
head(out3)
out3
#plotting emissions default vs 2007 emissions
out1[["scenario"]] <- "Unmodified"
out2[["scenario"]] <- "2024 data"
out3[["scenario"]] <- "2007 data"
comparison_plot <- rbind(out1, out2, out3)
comparison_plot
#is it not just group = scenario to differentiate?
ggplot(data = comparison_plot, aes(year, value, color = scenario,
                                   linetype = scenario)) + 
  geom_line() + 
  facet_wrap("variable", scales = "free") + ggtitle("LUC Emissions")
ggsave("LUC_emissions_fig.png", width = 8, height = 5)
