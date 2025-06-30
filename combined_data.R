#PT, combined data
data_1 <- read.csv("selected_GCP.csv")
data_2 <- read.csv("Historical_data_increase.csv")
complete_csv <- rbind(data_1, data_2)

head(data_2)
ggplot() +
  geom_line(data = complete_csv,
            aes(year, value, color = GCP)) +
  facet_wrap("GCP", scales = "fixed")
