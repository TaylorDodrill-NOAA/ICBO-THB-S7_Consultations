install.packages("tidyverse")
library(tidyverse)
library(dplyr)

king <- read.csv("C:/Users/Taylor.Dodrill/Downloads/pestsamp_king.csv")
twin <- read.csv("C:/Users/Taylor.Dodrill/Downloads/pestsamp_twin.csv")
rexb <- read.csv("C:/Users/Taylor.Dodrill/Downloads/pestsamp_rexb.csv")

king$CONSTIT <- as.factor(king$CONSTIT)
kingsub <- king[king$CONSTIT == c("Carbaryl", "Diazinon"), ]
dates_split <- strsplit(kingsub$DATETIME, split = " ")
kingsub[c("date", "time")] <- do.call(rbind, strsplit(kingsub$DATETIME, " "))
kingsub$date <- as.Date(kingsub$date, format = "%m/%d/%Y")


kingplot <- ggplot(kingsub, aes(y=CONCENTRATION, x=date, group = CONSTIT, colour = CONSTIT)) +
  geom_point() +
 # scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  labs(y = "concentration", x = "date") +
  theme_bw(base_size = 18)

twin$CONSTIT <- as.factor(twin$CONSTIT)
twinsub <- twin[twin$CONSTIT == c("Carbaryl", "Diazinon"), ]
dates_split <- strsplit(twinsub$DATETIME, split = " ")
twinsub[c("date", "time")] <- do.call(rbind, strsplit(twinsub$DATETIME, " "))
twinsub$date <- as.Date(twinsub$date, format = "%m/%d/%Y")

twinplot <- ggplot(twinsub, aes(y=CONCENTRATION, x=date, group = CONSTIT, colour = CONSTIT)) +
  geom_point() +
  # scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  labs(y = "concentration", x = "date") +
  theme_bw(base_size = 18)

rexb$CONSTIT <- as.factor(rexb$CONSTIT)
rexbsub <- rexb[rexb$CONSTIT == c("Carbaryl", "Diazinon"), ]
rexbsubc <- rexb[rexb$CONSTIT == c("Carbaryl"), ]
rexbsubd <- rexb[rexb$CONSTIT == c("Diazinon"), ]
dim(rexbsubc)
dim(rexbsubd)


dates_split <- strsplit(rexbsub$DATETIME, split = " ")
rexbsub[c("date", "time")] <- do.call(rbind, strsplit(rexbsub$DATETIME, " "))
rexbsub$date <- as.Date(rexbsub$date, format = "%m/%d/%Y")

rexbplot <- ggplot(rexbsub, aes(y=CONCENTRATION, x=date, group = CONSTIT, colour = CONSTIT)) +
  geom_point() +
  # scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  labs(y = "concentration", x = "date") +
  theme_bw(base_size = 18)


kingsubc <- kingsub[kingsub$CONSTIT == c("Carbaryl"), ]
kingsubd <- kingsub[kingsub$CONSTIT == c("Diazinon"), ]
dim(kingsubc)
dim(kingsubd)

twinsubc <- twinsub[twinsub$CONSTIT == c("Carbaryl"), ]
twinsubd <- twinsub[twinsub$CONSTIT == c("Diazinon"), ]
dim(twinsubc)
dim(twinsubd)

rexbsubc <- rexbsub[rexbsub$CONSTIT == c("Carbaryl"), ]
rexbsubd <- rexbsub[rexbsub$CONSTIT == c("Diazinon"), ]
dim(rexbsubc)
dim(rexbsubd)

#next, find a way to extract only the rows with shared dates
#make a carbaryl df and a diaz df then merge them on shared dates, getting rid of those that don't match?

kingshared <- merge(kingsubc, kingsubd, by.x = "date", by.y = "date")

twinshared <- merge(twinsubc, twinsubd, by.x = "date", by.y = "date")

rexbshared <- merge(rexbsubc, rexbsubd, by.x = "date", by.y = "date")

