#Fossil Fuel Visualization
data <- read.csv("Fossil_Fuels.csv")
head(data)
library(tidyverse)

all_gcp_years <- unique(data$GCP)
results <- list()
for(gcp_year in all_gcp_years){
  isolated_data <- data %>% filter(GCP == gcp_year)
  message("running ", gcp_year)
  message("I see ", nrow(isolated_data), " rows of data")
  results[[as.character(gcp_year)]] <- isolated_data$value
  message(head(results[[as.character(gcp_year)]]))
}
#results_df <- bind_rows(results, .id = "GCP")
gcp_years <- c("2014", "2019", "2024")
results_df <- lapply(names(results), function(gcp_data){
  gcp_values <- results[[gcp_data]]
  df_build <- as.data.frame(value = gcp_values)
  df_build$gcp <- gcp_data
  return(df_build)
  
}
)
  
head(results_df)

ggplot() +
  geom_line(data = results_df,
            aes(year, value, color = GCP)) +
  facet_wrap("variable", scales = "free") +
  ggtitle("Fossil Fuel Emissions") +
  xlab("Year") +
  ylab("Value (Pg C/yr)") +
  theme(text = element_text(size = 12, family = "mono", face = "bold"))
