install.packages("zoo")
library(zoo)
library(ggplot2)
dta <- read.csv("H:/Stibnite_temp_graph_dta.csv")
dta$Date <- as.Date(dta$Date, format = "%m/%d/%y")
dta_dadm <- transform(dta, dadm7 = rollmeanr(Daily_max, 7, fill=NA))

ggplot(dta_dadm, aes(y=dadm7, x=Date, group = Site, colour = Site)) +
  geom_line(size = 1.1) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  labs(y = "7DADM (°C)", x = "2022") +
  theme_bw(base_size = 18)
