fish <- read.csv("fish_ssd.csv")
install.packages("ssdtools")
install.packages('tidyverse')
library(ssdtools)
library(tidyverse)
library(tibble)
library(ggplot2)

fish <- fish[1:12, ]
my_tibble <- as_tibble(fish)

fits <- ssd_fit_dists(my_tibble)
autoplot(fits)

withr::with_seed(99, {
  hc5 <- ssd_hc(fits, ci = TRUE)
})
print(hc5)

cyanide_pred <- predict(fits, ci = TRUE)


theme_set(theme_bw())

ssd_plot(my_tibble, cyanide_pred,
         shape = "group", color = "group", label = "species_sci_name",
         xlab = "Concentration (mg/L)", ribbon = TRUE
) +
  expand_limits(x = 3000) +
  scale_colour_ssd()
