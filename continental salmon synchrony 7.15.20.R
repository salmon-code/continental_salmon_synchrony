#for tomorrow, seems like you should filter out salmon that are not in at least 
#~10% of trawls 


library(tidyverse)
library(MARSS)
setwd("/Users/StuartMunsch/Google Drive/NE Salmon Synchrony/")


#data prep =======================================================
#data prep =======================================================
#data prep =======================================================

ps <- read.csv("data/correighs_data.csv")
vi <- read.csv("data/jackies_data.csv")
ow <- read.csv("data/brians_data.csv")
ak <- read.csv("data/jamals_data.csv")

#for every dataset, create a tidy dataframe with
  #year, salmon species, abundance metric standardized by sampling intensity

ps_1 <- ps %>% select(Year, all.CK.., all.coho.., Chum., Pink., Sockeye., flow)
names(ps_1) <- c("year", "chinook_c", "coho_c", "chum_c", "pink_c", "sockeye_c", "flow")
mean_flow <- mean(na.omit(ps_1$flow))
ps_1$flow_2 <- ifelse(is.na(ps_1$flow) == TRUE, mean_flow, ps_1$flow)
ps_1$chinook <- ps_1$chinook_c / ps_1$flow_2
ps_1$chum <- ps_1$chum_c / ps_1$flow_2
ps_1$coho <- ps_1$coho_c / ps_1$flow_2
ps_1$pink <- ps_1$pink_c / ps_1$flow_2
ps_1$sockeye <- ps_1$sockeye_c / ps_1$flow_2
ps_1_long <- ps_1 %>% select("year", "chinook", "coho", "chum", "pink", "sockeye") %>% pivot_longer(names_to = "species", values_to = "count", cols = c("chinook", "coho", "chum", "pink", "sockeye"))
ps_out <- ps_1_long %>% filter(count > -1) %>% group_by(year, species) %>% summarise(log_mean_count = mean(log(count + 1)))
ps_out$region <- "Puget Sound"

#only going to use surface tows
vi_1 <- vi %>% filter(HEADROPE_DEPTH == 0) %>% select(TRIP_YEAR, CHINOOK, CHUM, COHO, PINK, SOCKEYE, SWEPT_VOLUME)
vi_1$year <- vi_1$TRIP_YEAR 
vi_1$chinook <- vi_1$CHINOOK / vi_1$SWEPT_VOLUME
vi_1$chum <- vi_1$CHUM / vi_1$SWEPT_VOLUME
vi_1$coho <- vi_1$COHO / vi_1$SWEPT_VOLUME
vi_1$pink <- vi_1$PINK / vi_1$SWEPT_VOLUME
vi_1$sockeye <- vi_1$SOCKEYE / vi_1$SWEPT_VOLUME
vi_1_long <- vi_1 %>% select("year", "chinook", "coho", "chum", "pink", "sockeye") %>% pivot_longer(names_to = "species", values_to = "count", cols = c("chinook", "coho", "chum", "pink", "sockeye"))
vi_out <- vi_1_long %>% filter(count > -1) %>% group_by(year, species) %>% summarise(log_mean_count = mean(log(count + 1)))
vi_out$region <- "Vancouver Island"

ow$chinook <- ow$Chinook.mixed.age.juvenile.CPUE....km.trawled. + ow$Chinook.subyearling.CPUE....km.trawled. + ow$Chinook.yearling.CPUE....km.trawled.
ow_1 <- ow %>% select(Year, chinook, Chum.juvenile.CPUE....km.trawled., Coho.yearling.CPUE....km.trawled., Sockeye.juvenile.CPUE....km.trawled. )
names(ow_1) <- c("year", "chinook", "chum", "coho", "sockeye")
ow_1_long <- ow_1 %>% select("year", "chinook", "coho", "chum",  "sockeye") %>% pivot_longer(names_to = "species", values_to = "count", cols = c("chinook", "coho", "chum", "sockeye"))
ow_out <- ow_1_long %>% filter(count > -1) %>% group_by(year, species) %>% summarise(log_mean_count = mean(log(count + 1)))
ow_out$region <- "Oregon & Washington"

#not inclduing west gulf of alaska because there are only a few years of data
ak$chinook <- (ak$Chinook.Salmon.IM + ak$Chinook.Salmon.J) / ak$Effort_area_km2
ak$coho <- (ak$Coho.Salmon.IM + ak$Coho.Salmon.J) / ak$Effort_area_km2 
ak$chum <- (ak$Chum.Salmon.IM + ak$Chum.Salmon.J) / ak$Effort_area_km2
ak$pink <- (ak$Pink.Salmon.IM + ak$Pink.Salmon.J) / ak$Effort_area_km2
ak$sockeye <- (ak$Sockeye.Salmon.IM + ak$Sockeye.Salmon.J) / ak$Effort_area_km2
ak$year <- ak$SampleYear
ak_1 <- ak %>% select(year, chinook, coho, chum, pink, sockeye, region)
ak_1_long <- ak_1 %>% select("year", "region", "chinook", "coho", "chum", "pink", "sockeye") %>% pivot_longer(names_to = "species", values_to = "count", cols = c("chinook", "coho", "chum", "pink", "sockeye"))
ak_out <- ak_1_long %>% filter(count > -1 & region != "West Gulf of Alaska") %>% group_by(year, species, region) %>% summarise(log_mean_count = mean(log(count + 1)))

all_out_1 <- bind_rows(ps_out, vi_out, ow_out, ak_out)
all_out <- all_out_1 %>% group_by(region, species) %>% mutate(log_mean_count_z = (log_mean_count - mean(log_mean_count)) / sd(log_mean_count))

ggplot(aes(x = year, y = log_mean_count_z, col = species), data = all_out) +
  geom_line() + 
  facet_wrap(~region) +
  geom_vline(aes(xintercept = 2014)) +
  geom_vline(aes(xintercept = 2015))

