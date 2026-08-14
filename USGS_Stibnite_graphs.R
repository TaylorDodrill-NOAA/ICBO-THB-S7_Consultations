library(lubridate)
library(dplyr)
library(zoo)
library(ggplot2)
library(RColorBrewer)
install.packages("ggpubr")
library(ggpubr)


#read in, format and bind all data togehter
mergeddta<- do.call(rbind, lapply(list.files(path = "H:/temp_dta_Stibnite"), read.csv))
ypp <- read.csv("H:/Stibnite_temp_graph_dta.csv")
colnames(ypp) <- c("site", "date", "daily_max")
ypp$date <- as.Date(ypp$date, format = "%m/%d/%y")

max <- select(mergeddta, c("site", "date", "daily_max"))
max$date <- as.Date(max$date, format = "%m/%d/%Y")
dta <- rbind(ypp, max)

# calculate 7DADM
dadm <- dta %>%
  group_by(site) %>%
  mutate(dadm7 = rollmeanr(daily_max, 7, fill=NA))

#make a subset with only June-September
dadm$date <- as.POSIXlt(dadm$date)
dadm_sub_long <- dadm %>%
  filter(month(date) %in% 6:9)

#make subsets with June-September for each site
dadm_sub_long$site <- as.factor(dadm_sub_long$site)
#dadm_sub_long$date <- as.Date(dadm_sub_long$date, format = "%Y-%m-%d")
wyear <- dadm_sub_long %>%
  mutate(year = year(date), doy = strftime(date, format = "%j"), month = month(date))

meadow <- wyear %>%
  filter(site=="13310850")
EFSFSR_0800 <- wyear %>%
  filter(site=="13310800")
EFSFSR_1000 <- wyear %>%
  filter(site=="13311000")
EFSFSR_1250 <- wyear %>%
  filter(site=="13311250")
sugar <- wyear %>%
  filter(site=="13311450")


#make a subset with only June-September of 2022
dadm_sub_2022 <- dadm %>%
  filter(year(date) %in% 2022 & month(date) %in% 6:9)

#subset with June-Sept USGS and stk sites
dadm_sub_2022$site <- as.factor(dadm_sub_2022$site)
stk <- dadm_sub_2022 %>%
  filter(site %in% c("13310800", "13310850", "13311000", "13311250", "13311450", "15-2-stk", "16B_stake", "6-1-stk"))

#subset with June-Sept USGS and epox sites
epox <- dadm_sub_2022 %>%
  filter(site %in% c("13310800", "13310850", "13311000", "13311250", "13311450", "15-1-epox", "16A_epox", "6-2-epox"))

# graph for 2022 with all sites
dadm_sub_2022$date <- as.Date(dadm_sub_2022$date, format = "%Y-%m-%d")
ggplot(dadm_sub_2022, aes(y=dadm7, x=date, group = site, colour = site)) +
  geom_line(size = 1.1) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  labs(y = "7DADM (°C)", x = "2022") +
  theme_bw(base_size = 18)

#graph for 2022 with USGS and stk sites
ggplot(stk, aes(y=dadm7, x=date, group = site, colour = site)) +
  geom_line(size = 1.1) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  scale_y_continuous(limits = c(0,16), breaks=seq(0,16,by=4))+
  labs(y = "7DADM (°C)", x = "2022") +
  theme_bw(base_size = 18)

#graph for 2022 with USGS and epox sites
ggplot(epox, aes(y=dadm7, x=date, group = site, colour = site)) +
  geom_line(size = 1.1) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  scale_y_continuous(limits = c(0,16), breaks=seq(0,16,by=4))+
  labs(y = "7DADM (°C)", x = "2022") +
  theme_bw(base_size = 18)


#graph for long-term baseline for Meadow Creek site
# meadow$date <- as.Date(meadow$date, format = "%Y-%m-%d")
# ggplot(meadow, aes(y=dadm7, x=doy, group = year, colour = year)) +
#   geom_line(size = 1.1) +
#   #scale_x_date(date_breaks = "1 months", date_labels = "%b") +
#   labs(y = "7DADM (°C)", x = "2022") +
#   theme_bw(base_size = 18)
# 

nb.cols <- 14
mycolors <- colorRampPalette(brewer.pal(8, "Blues"))(nb.cols)


p1 <- ggplot(meadow, aes(x = as.Date("2019-12-31") + yday(date), 
           # x = ymd(paste(2020, month(date), day(date))),  # alternative
           y = dadm7, colour = factor(year(date)))) + 
  geom_line(size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(limits = c(0,18), breaks=seq(0,18,by=2))+
  scale_color_manual(values=mycolors)+
  labs(title = "Meadow Creek: 13310850", y = "7DADM (°C)", x = "Month", color = "Year") +
  theme_bw(base_size = 18)

#East Fork South Fork Salmon River: 13310800
p2 <- ggplot(EFSFSR_0800, aes(x = as.Date("2019-12-31") + yday(date), 
                         # x = ymd(paste(2020, month(date), day(date))),  # alternative
                         y = dadm7, colour = factor(year(date)))) + 
  geom_line(size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(limits = c(0,18), breaks=seq(0,18,by=2))+
  scale_color_manual(values=mycolors)+
  labs(title = "East Fork South Fork Salmon River: 13310800", y = "7DADM (°C)", x = "Month", color = "Year") +
  theme_bw(base_size = 18)

#East Fork South Fork Salmon River: 13311000
p3 <- ggplot(EFSFSR_1000, aes(x = as.Date("2019-12-31") + yday(date), 
                              # x = ymd(paste(2020, month(date), day(date))),  # alternative
                              y = dadm7, colour = factor(year(date)))) + 
  geom_line(size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(limits = c(0,18), breaks=seq(0,18,by=2))+
  scale_color_manual(values=mycolors)+
  labs(title = "East Fork South Fork Salmon River: 13311000", y = "7DADM (°C)", x = "Month", color = "Year") +
  theme_bw(base_size = 18)

#East Fork South Fork Salmon River: 13311250
p4 <- ggplot(EFSFSR_1250, aes(x = as.Date("2019-12-31") + yday(date), 
                              # x = ymd(paste(2020, month(date), day(date))),  # alternative
                              y = dadm7, colour = factor(year(date)))) + 
  geom_line(size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(limits = c(0,18), breaks=seq(0,18,by=2))+
  scale_color_manual(values=mycolors)+
  labs(title = "East Fork South Fork Salmon River: 13311250", y = "7DADM (°C)", x = "Month", color = "Year") +
  theme_bw(base_size = 18)

#Sugar Creek: 13311450
p5 <- ggplot(sugar, aes(x = as.Date("2019-12-31") + yday(date), 
                              # x = ymd(paste(2020, month(date), day(date))),  # alternative
                              y = dadm7, colour = factor(year(date)))) + 
  geom_line(size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(limits = c(0,18), breaks=seq(0,18,by=2))+
  scale_color_manual(values=mycolors)+
  labs(title = "Sugar Creek: 13311450", y = "7DADM (°C)", x = "Month", color = "Year") +
  theme_bw(base_size = 18)

ggarrange(p1, p2, p3, p4, p5, ncol = 1, nrow=5, common.legend = TRUE, legend = "right")
