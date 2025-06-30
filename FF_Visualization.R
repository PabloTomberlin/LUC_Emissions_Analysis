#Fossil Fuel Visualization
data <- read.csv("Fossil_Fuels.csv")
head(data)
library(tidyverse)
#data$GCP <- as.character(GCP)

#It is already in a data frame so I thought I could just plot it 
ggplot() +
  geom_line(data = data,
            aes(year, value, color = GCP)) +
  facet_wrap("GCP", scales = "fixed") +
  scale_color_gradient(low = "blue", high = "red") +
  ggtitle("Fossil Fuel Emissions") +
  xlab("Year") +
  ylab("Value (Pg C/yr)") +
  theme(text = element_text(size = 12, family = "mono", face = "bold"))
  

#Claudia suggested making it a matrix then data frame but I don't think it plots in ggplot
GCP <- unique(data$GCP)
(nc <- length(unique(data$GCP)))
(all_years <- sort(unique(data$year)))
results_df <- matrix(NA, length(all_years), nc + 1)
results_df[, 1] <- all_years
for(i in 1:nc){
matching_years<-data$year[data$GCP == GCP[i]]
results_df[match(matching_years,results_df[,1]), 1+i] <- data$value[data$GCP == GCP[i]]
}
dimnames(results_df) <- list(NULL, c("Year", GCP))
results_df <- as.data.frame(results_df)
results_df

matplot(results_df$Year, as.matrix(results_df[, -1]),xlab="year", ylab="FF emissions",type="l",lwd=2,las=1,lty=1,col=c(1:3))

graphics.off()


#I tried a for loop
all_gcp_years <- unique(data$GCP)
results <- list()
for(gcp_year in all_gcp_years){
  isolated_data <- data %>% filter(GCP == gcp_year)
  message("running ", gcp_year)
  message("I see ", nrow(isolated_data), " rows of data")
  results[[as.character(gcp_year)]] <- cbind(isolated_data$yearisolated_data$value)
  message(head(results[[as.character(gcp_year)]]))
}

results_df <- results[[2]]
head(results_df)

#maybe fill with NA?
results_df <- as.data.frame(do.call(bind_rows(results, .id = "GCP"), results))
head(results_df)

ggplot() +
  geom_line(data = results_df,
            aes(year, value, color = GCP))


#make results a list of data frames
results
results_df <- data.frame(results)
results_df <- bind_rows(results, .id = "GCP")
head(results_df)

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
