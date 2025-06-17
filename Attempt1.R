#Pablo Tomberlin, LUC emissions attempt 1
setwd("~/LUC_Emissions")
data <- read.csv("selected_GCP.csv")
library(tidyverse)
#initial plot
data %>%
  ggplot(aes(x = year, y = value, group = GCP, color = GCP)) + geom_line() #+
  #minimal_theme()
head(data)

#Isolate one GCP release
data %>% filter(GCP == 2007) %>% ggplot(aes(x = year, y = value)) + geom_line()

#basic run, setting value of luc_emissions to data value
library(hector)
ini_file <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini_file)
core
run(core)
out1 <- fetchvars(core, 1959:2006, vars = c(LUC_EMISSIONS(),
                                            CONCENTRATIONS_CO2()))
head(out1)
reset(core)
#failed attempt: setvar(core, 1959:2006, LUC_EMISSIONS(), data[], "(unitless)")
values = c(1.4,
           1.39,
           1.46,
           1.46,
           1.47,
           1.49,
           1.5,
           1.54,
           1.55,
           1.48,
           1.48,
           1.44,
           1.29,
           1.26,
           1.25,
           1.25,
           1.25,
           1.31,
           1.32,
           1.31,
           1.28,
           1.24,
           1.26,
           1.46,
           1.51,
           1.56,
           1.58,
           1.6,
           1.61,
           1.64,
           1.65,
           1.64,
           1.71,
           1.61,
           1.59,
           1.58,
           1.56,
           1.53,
           1.49,
           1.49,
           1.45,
           1.41,
           1.39,
           1.52,
           1.51,
           1.53,
           1.47,
           1.47)
setvar(core, 1959:2006, var = LUC_EMISSIONS(), values = values, unit = getunits(LUC_EMISSIONS()))
#why is data$value still 779 when I filtered for 2007?
#setvar(core, 1959:2006, var = LUC_EMISSIONS(), values = data$value, unit = getunits(LUC_EMISSIONS()))
core
run(core)

#verification
test_emissions <- fetchvars(core, 1959:2006, vars = LUC_EMISSIONS())
head(test_emissions)

#outputs: predicted emissions or co2 concentration?
#how do I predict the future
out2 <- fetchvars(core, 1959:2006, vars = c(LUC_EMISSIONS(),
                                            CONCENTRATIONS_CO2()))
head(out2)

#out %>% ggplot(aes(x = year, y = value)) + geom_line()

#plot
yseven_plot <- rbind(out1, out2)
ggplot(data = yseven_plot, aes(year, value, color = scenario)) + geom_point() +
  facet_wrap("variable", scales = "free")
