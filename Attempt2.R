#Pablo Tomberlin, LUC emissions attempt 2 just extracting
setwd("~/LUC_Emissions")
data <- read.csv("selected_GCP.csv")
library(tidyverse)
#initial plot
#data %>%
  #ggplot(aes(x = year, y = value, group = GCP, color = GCP)) + geom_line() +
  #minimal_theme()

#Isolate one GCP release
isolated_data <- data %>% filter(GCP == 2007) #%>% ggplot(aes(x = year, y = value)) + geom_line()
head(isolated_data)

#basic run, I choose the years
library(hector)
ini_file <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini_file)
run(core)
out1 <- fetchvars(core, isolated_data$year, vars = c(LUC_EMISSIONS(),
                                                     CONCENTRATIONS_CO2()))
head(out1)
ggplot(data = out1, aes(year, value)) + 
  geom_line()

#modified data from 2007 GCP
setvar(core, isolated_data$year, LUC_EMISSIONS(), isolated_data$value, getunits(LUC_EMISSIONS()))
core
reset(core)
run(core)

#test it out
out2 <- fetchvars(core, isolated_data$year, vars = c(LUC_EMISSIONS(),
                                                     CONCENTRATIONS_CO2()))
head(out2)
ggplot(data = out2, aes(year, value)) + 
  geom_line()
#plotting emissions default vs 2007 emissions
out1[["scenario"]] <- "Unmodified"
out2[["scenario"]] <- "2007 data"
comparison_plot <- rbind(out1, out2)
comparison_plot
#is it not just group = scenario to differentiate?
ggplot(data = comparison_plot, aes(year, value, color = scenario)) + 
  geom_line() + 
  facet_wrap("variable", scales = "free") + ggtitle("LUC Emissions")
