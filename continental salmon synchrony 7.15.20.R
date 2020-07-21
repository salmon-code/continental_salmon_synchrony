#loadings looked reversed for some reason...
#figure it out Monday
#next step: create an abundance and index for every year. keep it simple and just do a smoother on time. will need to be a hurdle model for fish
#write one function for fish and one for temp

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
sum(na.omit(ps_1$chinook > 0)) / length(na.omit(ps_1$chinook)); sum(na.omit(ps_1$chum > 0)) / length(na.omit(ps_1$chum)); sum(na.omit(ps_1$coho > 0)) / length(na.omit(ps_1$coho)); sum(na.omit(ps_1$pink > 0)) / length(na.omit(ps_1$pink)); sum(na.omit(ps_1$sockeye > 0)) / length(na.omit(ps_1$sockeye)) 
#chinook, chum, pink >10% (threshold for pink = 5% because only present every other year)
ps_1_long <- ps_1 %>% select("year", "chinook", "chum", "pink") %>% pivot_longer(names_to = "species", values_to = "count", cols = c("chinook", "chum", "pink"))
ps_1_long$region <- "Puget Sound"

#only going to use surface tows
vi_1 <- vi %>% filter(HEADROPE_DEPTH == 0) %>% select(TRIP_YEAR, CHINOOK, CHUM, COHO, PINK, SOCKEYE, SWEPT_VOLUME)
vi_1$year <- vi_1$TRIP_YEAR 
vi_1$chinook <- vi_1$CHINOOK / vi_1$SWEPT_VOLUME
vi_1$chum <- vi_1$CHUM / vi_1$SWEPT_VOLUME
vi_1$coho <- vi_1$COHO / vi_1$SWEPT_VOLUME
vi_1$pink <- vi_1$PINK / vi_1$SWEPT_VOLUME
vi_1$sockeye <- vi_1$SOCKEYE / vi_1$SWEPT_VOLUME
#all species regularly present
sum(na.omit(vi_1$chinook > 0)) / length(na.omit(vi_1$chinook)); sum(na.omit(vi_1$chum > 0)) / length(na.omit(vi_1$chum)); sum(na.omit(vi_1$coho > 0)) / length(na.omit(vi_1$coho)); sum(na.omit(vi_1$pink > 0)) / length(na.omit(vi_1$pink)); sum(na.omit(vi_1$sockeye > 0)) / length(na.omit(vi_1$sockeye)) 
vi_1_long <- vi_1 %>% select("year", "chinook", "coho", "chum", "pink", "sockeye") %>% pivot_longer(names_to = "species", values_to = "count", cols = c("chinook", "coho", "chum", "pink", "sockeye"))
vi_1_long$region <- "Vancouver Island"

ow$chinook <- ow$Chinook.mixed.age.juvenile.CPUE....km.trawled. + ow$Chinook.subyearling.CPUE....km.trawled. + ow$Chinook.yearling.CPUE....km.trawled.
ow_1 <- ow %>% select(Year, chinook, Chum.juvenile.CPUE....km.trawled., Coho.yearling.CPUE....km.trawled., Sockeye.juvenile.CPUE....km.trawled. )
names(ow_1) <- c("year", "chinook", "chum", "coho", "sockeye")
#all species regularly present
sum(na.omit(ow_1$chinook > 0)) / length(na.omit(ow_1$chinook)); sum(na.omit(ow_1$chum > 0)) / length(na.omit(ow_1$chum)); sum(na.omit(ow_1$coho > 0)) / length(na.omit(ow_1$coho)); sum(na.omit(ow_1$pink > 0)) / length(na.omit(ow_1$pink)); sum(na.omit(ow_1$sockeye > 0)) / length(na.omit(ow_1$sockeye)) 
ow_1_long <- ow_1 %>% select("year", "chinook", "coho", "chum",  "sockeye") %>% pivot_longer(names_to = "species", values_to = "count", cols = c("chinook", "coho", "chum", "sockeye"))
ow_1_long$region <- "Oregon & Washington"

#not inclduing west gulf of alaska because there are only a few years of data
ak$chinook <- (ak$Chinook.Salmon.IM + ak$Chinook.Salmon.J) / ak$Effort_area_km2
ak$coho <- (ak$Coho.Salmon.IM + ak$Coho.Salmon.J) / ak$Effort_area_km2 
ak$chum <- (ak$Chum.Salmon.IM + ak$Chum.Salmon.J) / ak$Effort_area_km2
ak$pink <- (ak$Pink.Salmon.IM + ak$Pink.Salmon.J) / ak$Effort_area_km2
ak$sockeye <- (ak$Sockeye.Salmon.IM + ak$Sockeye.Salmon.J) / ak$Effort_area_km2
ak$year <- ak$SampleYear
ak$region <- ak$Region
ak_1 <- ak %>% select(year, chinook, coho, chum, pink, sockeye, region)
ak_1_long <- ak_1 %>% select("year", "region", "chinook", "coho", "chum", "pink", "sockeye") %>% pivot_longer(names_to = "species", values_to = "count", cols = c("chinook", "coho", "chum", "pink", "sockeye"))
#all species regularly present

all_long <- bind_rows(ps_1_long, vi_1_long, ow_1_long, ak_1_long)


#function is a zero inflated GAM that generates abundance indexes for years

zero_inflated_gam <- function(the_data, the_species, the_year_begin, the_year_end){
  presence_absence <- inla(count ~ -1 + f(day_of_year, model = "rw2") + as.factor(year), family = "binomial")
  abundance_when_present <- inla(log(count) ~ -1  )
}

#indexes

U <- 1 #explained in rw1 portion of zuur's first of two books
hyper.prec <- list("theta" = list(
  prior = "pc.prec", 
  param = c(U, 0.1)))
sac125_smooth <- inla(presence ~  
                        m_cpue_SAC_LN.s +
                        f(round(sac_mavg_flow.s, 1), model = "rw2", hyper = hyper.prec) + 
                        f(station,model = "iid") + 
                        f(day.of.water.year, model = "rw2", hyper = hyper.prec) + 
                        f(watertempR, model = "rw2", hyper = hyper.prec) + 
                        f(wateryear, model = "iid"), 
                      family="binomial",
                      control.predictor = list(link = 1, compute = TRUE, quantiles = c(0.025, 0.975)),
                      data = sac_for_flow_predictions)
plot(sac125_smooth$summary.random$`round(sac_mavg_flow.s, 1)`$mean)


#Zuur page 309. creating a truncated negative binomial distribution
trunc_nb <- list(hyper = list(theta = list(initial = -10, fixed = TRUE)))

sac125_smooth_catch <- inla(catch ~  
                              m_cpue_SAC_LN.s +
                              f(round(sac_mavg_flow.s, 1), model = "rw2", hyper = hyper.prec) + 
                              f(station,model = "iid") + 
                              f(day.of.water.year, model = "rw2", hyper = hyper.prec) + 
                              f(watertempR, model = "rw2", hyper = hyper.prec) + 
                              f(wateryear, model = "iid"), 
                            family="zeroinflatednbinomial0",
                            control.predictor = list(link = 1, compute = TRUE, quantiles = c(0.025, 0.975)), 
                            control.family = trunc_nb,
                            data = sac_for_flow_predictions_catch)




ggplot(aes(x = year, y = log_mean_count_z, col = species), data = all_out) +
  geom_line() + 
  geom_point() +
  facet_wrap(~region) +
  geom_vline(aes(xintercept = 2014)) +
  geom_vline(aes(xintercept = 2015))
ggsave("salmon synchrony raw.jpeg", width = 10, height = 4)

ow_temp <- ow %>% group_by(Year) %>% summarise(mean_temp = mean(na.omit(X5m_Temperature_oC))) %>% mutate(mean_temp_z = (mean_temp - mean(mean_temp)) / sd(mean_temp))
names(ow_temp) <- c("year", "mean_temp", "mean_temp_z")
ow_temp$region <- "Oregon & Washington"

vi_temp <- vi %>% group_by(TRIP_YEAR) %>% summarise(mean_temp = mean(na.omit(Temp5m))) %>% mutate(mean_temp_z = (mean_temp - mean(mean_temp)) / sd(mean_temp))
names(vi_temp) <- c("year", "mean_temp", "mean_temp_z")
vi_temp$region <- "Vancouver Island"

ak_temp <- ak %>% group_by(region, year) %>% summarise(mean_temp = mean(na.omit(Temp_at_5m))) %>% mutate(mean_temp_z = (mean_temp - mean(mean_temp)) / sd(mean_temp))



#fit DFA to salmon data =======================================================
#fit DFA to salmon data =======================================================
#fit DFA to salmon data =======================================================
ega_out_wide <- all_out %>% select(-log_mean_count) %>% filter(region == "East Gulf of Alaska") %>% pivot_wider(names_from = species, values_from = log_mean_count_z)
names(ega_out_wide) <- c("year", "region", "chinook_ega", "chum_ega", "coho_ega", "pink_ega", "sockeye_ega")

nbs_out_wide <- all_out %>% select(-log_mean_count) %>% filter(region == "North Bering Sea") %>% pivot_wider(names_from = species, values_from = log_mean_count_z)
names(nbs_out_wide) <- c("year", "region", "chinook_nbs", "chum_nbs", "coho_nbs", "pink_nbs", "sockeye_nbs")

ow_out_wide <- all_out %>% select(-log_mean_count) %>% filter(region == "Oregon & Washington") %>% pivot_wider(names_from = species, values_from = log_mean_count_z)
names(ow_out_wide) <- c("year", "region", "chinook_ow", "chum_ow", "coho_ow", "sockeye_ow")

ps_out_wide <- all_out %>% select(-log_mean_count) %>% filter(region == "Puget Sound") %>% pivot_wider(names_from = species, values_from = log_mean_count_z)
names(ps_out_wide) <- c("year", "region", "chinook_ps", "chum_ps", "pink_ps")

sbs_out_wide <- all_out %>% select(-log_mean_count) %>% filter(region == "Southeast Bering Sea") %>% pivot_wider(names_from = species, values_from = log_mean_count_z)
names(sbs_out_wide) <- c("year", "region", "chinook_sbs", "chum_sbs", "coho_sbs", "pink_sbs", "sockeye_sbs")

vi_out_wide <- all_out %>% select(-log_mean_count) %>% filter(region == "Vancouver Island") %>% pivot_wider(names_from = species, values_from = log_mean_count_z)
names(vi_out_wide) <- c("year", "region", "chinook_vi", "chum_vi", "coho_vi", "pink_vi", "sockeye_vi")

all_out_wide_1 <- left_join(nbs_out_wide %>% ungroup() %>% select(-"region"), ega_out_wide %>% ungroup() %>% select(-"region") %>% ungroup())
all_out_wide_2 <- left_join(ow_out_wide %>% ungroup() %>% select(-"region") %>% ungroup(), all_out_wide_1)
all_out_wide_3 <- left_join(all_out_wide_2, ps_out_wide %>% ungroup() %>% select(-"region") %>% ungroup())
all_out_wide_4 <- left_join(all_out_wide_3, sbs_out_wide %>% ungroup() %>% select(-"region") %>% ungroup())
all_out_wide_5 <- left_join(all_out_wide_4, vi_out_wide %>% ungroup() %>% select(-"region") %>% ungroup())
all_out_wide <- all_out_wide_5[,c("year",
                                  "chinook_nbs", "chum_nbs", "coho_nbs", "pink_nbs", "sockeye_nbs",
                                  "chinook_sbs", "chum_sbs", "coho_sbs", "pink_sbs", "sockeye_sbs",
                                  "chinook_ega", "chum_ega", "coho_ega", "pink_ega", "sockeye_ega",
                                  "chinook_vi", "chum_vi", "coho_vi", "pink_vi", "sockeye_vi",
                                  "chinook_ps", "chum_ps", "pink_ps",
                                  "chinook_ow", "chum_ow", "coho_ow", "sockeye_ow")]
## transpose data so time goes across columns
dat_salmon <- t(all_out_wide %>% select(-"year"))
## get number of time series
N_ts <- dim(dat_salmon)[1]
## get length of time series
TT <- dim(dat_salmon)[2]

## 'ZZ' is loadings matrix
Z_vals <- list("z1.1", 0, 0, 
               "z2.1", "z2.2", 0, 
               "z3.1", "z3.2", "z3.3", 
               "z4.1", "z4.2", "z4.3", 
               "z5.1", "z5.2", "z5.3",
               "z6.1", "z6.2", "z6.3",
               "z7.1", "z7.2", "z7.3",
               "z8.1", "z8.2", "z8.3",
               "z9.1", "z9.2", "z9.3",
               "z10.1", "z10.2", "z10.3",
               "z11.1", "z11.2", "z11.3",
               "z12.1", "z12.2", "z12.3",
               "z13.1", "z13.2", "z13.3",
               "z14.1", "z14.2", "z14.3",
               "z15.1", "z15.2", "z15.3",
               "z16.1", "z16.2", "z16.3",
               "z17.1", "z17.2", "z17.3",
               "z18.1", "z18.2", "z18.3",
               "z19.1", "z19.2", "z19.3",
               "z20.1", "z20.2", "z20.3",
               "z21.1", "z21.2", "z21.3",
               "z22.1", "z22.2", "z22.3",
               "z23.1", "z23.2", "z23.3",
               "z24.1", "z24.2", "z24.3",
               "z25.1", "z25.2", "z25.3",
               "z26.1", "z26.2", "z26.3",
               "z27.1", "z27.2", "z27.3")
ZZ <- matrix(Z_vals, nrow = N_ts, ncol = 3, byrow = TRUE)

Z_vals <- list("z1.1", 0, 
               "z2.1", "z2.2", 
               "z3.1", "z3.2", 
               "z4.1", "z4.2", 
               "z5.1", "z5.2", 
               "z6.1", "z6.2", 
               "z7.1", "z7.2", 
               "z8.1", "z8.2", 
               "z9.1", "z9.2", 
               "z10.1", "z10.2", 
               "z11.1", "z11.2", 
               "z12.1", "z12.2", 
               "z13.1", "z13.2", 
               "z14.1", "z14.2", 
               "z15.1", "z15.2",
               "z16.1", "z16.2", 
               "z17.1", "z17.2", 
               "z18.1", "z18.2", 
               "z19.1", "z19.2", 
               "z20.1", "z20.2", 
               "z21.1", "z21.2", 
               "z22.1", "z22.2", 
               "z23.1", "z23.2", 
               "z24.1", "z24.2", 
               "z25.1", "z25.2", 
               "z26.1", "z26.2", 
               "z27.1", "z27.2")
ZZ <- matrix(Z_vals, nrow = N_ts, ncol = 2, byrow = TRUE)


Z_vals <- list(0,  
               "z2.1",  
               "z3.1",  
               "z4.1",  
               "z5.1", 
               "z6.1", 
               "z7.1", 
               "z8.1",  
               "z9.1",  
               "z10.1",
               "z11.1", 
               "z12.1", 
               "z13.1", 
               "z14.1",
               "z15.1",
               "z16.1",  
               "z17.1", 
               "z18.1",
               "z19.1", 
               "z20.1", 
               "z21.1",  
               "z22.1", 
               "z23.1", 
               "z24.1", 
               "z25.1", 
               "z26.1",
               "z27.1")
ZZ <- matrix(Z_vals, nrow = N_ts, ncol = 1, byrow = TRUE)

## 'aa' is the offset/scaling
aa <- "zero"
## 'DD' and 'd' are for covariates
DD <- "zero"  # matrix(0,mm,1)
dd <- "zero"  # matrix(0,1,wk_last)
## 'RR' is var-cov matrix for obs errors
#RR <- "diagonal and unequal"

#grouping by region

R_vals <- list("nbs_v",	"nbs_c",	"nbs_c",	"nbs_c",	"nbs_c",	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
               "nbs_c",	"nbs_v",	"nbs_c",	"nbs_c",	"nbs_c",	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
               "nbs_c",	"nbs_c",	"nbs_v",	"nbs_c",	"nbs_c",	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
               "nbs_c",	"nbs_c",	"nbs_c",	"nbs_v",	"nbs_c",	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
               "nbs_c",	"nbs_c",	"nbs_c",	"nbs_c",	"nbs_v",	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
               0,	0,	0,	0,	0,	"sbs_v",	"sbs_c",	"sbs_c",	"sbs_c",	"sbs_c",	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
               0,	0,	0,	0,	0,	"sbs_c",	"sbs_v",	"sbs_c",	"sbs_c",	"sbs_c",	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
               0,	0,	0,	0,	0,	"sbs_c",	"sbs_c",	"sbs_v",	"sbs_c",	"sbs_c",	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
               0,	0,	0,	0,	0,	"sbs_c",	"sbs_c",	"sbs_c",	"sbs_v",	"sbs_c",	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
               0,	0,	0,	0,	0,	"sbs_c",	"sbs_c",	"sbs_c",	"sbs_c",	"sbs_v",	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
               0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	"ega_v",	"ega_c",	"ega_c",	"ega_c",	"ega_c",	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
               0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	"ega_c",	"ega_v",	"ega_c",	"ega_c",	"ega_c",	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
               0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	"ega_c",	"ega_c",	"ega_v",	"ega_c",	"ega_c",	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
               0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	"ega_c",	"ega_c",	"ega_c",	"ega_v",	"ega_c",	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
               0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	"ega_c",	"ega_c",	"ega_c",	"ega_c",	"ega_v",	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
               0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	"vi_v",	"vi_c",	"vi_c",	"vi_c",	"vi_c",	0,	0,	0,	0,	0,	0,	0,
               0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	"vi_c",	"vi_v",	"vi_c",	"vi_c",	"vi_c",	0,	0,	0,	0,	0,	0,	0,
               0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	"vi_c",	"vi_c",	"vi_v",	"vi_c",	"vi_c",	0,	0,	0,	0,	0,	0,	0,
               0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	"vi_c",	"vi_c",	"vi_c",	"vi_v",	"vi_c",	0,	0,	0,	0,	0,	0,	0,
               0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	"vi_c",	"vi_c",	"vi_c",	"vi_c",	"vi_v",	0,	0,	0,	0,	0,	0,	0,
               0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	"ps_v",	"ps_c",	"ps_c",	0,	0,	0,	0,
               0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	"ps_c",	"ps_v",	"ps_c",	0,	0,	0,	0,
               0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	"ps_c",	"ps_c",	"ps_v",	0,	0,	0,	0,
               0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	"ow_v",	"ow_c",	"ow_c",	"ow_c",
               0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	"ow_c",	"ow_v",	"ow_c",	"ow_c",
               0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	"ow_c",	"ow_c",	"ow_v",	"ow_c",
               0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	"ow_c",	"ow_c",	"ow_c",	"ow_v")

                  
RR <- matrix(R_vals, nrow = N_ts, ncol = N_ts, byrow = TRUE)


## number of processes
mm <- 1
## 'BB' is identity: 1's along the diagonal & 0's elsewhere
BB <- "identity"  # diag(mm)
## 'uu' is a column vector of 0's
uu <- "zero"  # matrix(0,mm,1)
## 'CC' and 'cc' are for covariates
CC <- "zero"  # matrix(0,mm,1)
cc <- "zero"  # matrix(0,1,wk_last)
## 'QQ' is identity
#QQ <- "identity"  # diag(mm)
#QQ <- matrix(list("q1", 0, 0, 
#             0, "q2", 0,
#             0, 0, "q3"), 
#             nrow = 3)

#QQ <- matrix(list("q1", 0, 
#                  0, "q2"), 
#             nrow = 2)

QQ <- matrix("q1")

## list with specifications for model vectors/matrices
mod_list <- list(Z = ZZ, A = aa, D = DD, d = dd, R = RR, B = BB, 
                 U = uu, C = CC, c = cc, Q = QQ)
## list with model inits
init_list <- list(x0 = matrix(rep(0, mm), mm, 1))
## list with model control parameters
con_list <- list(maxit = 10000, allow.degen = TRUE)

## fit MARSS
dfa_3 <- MARSS(y = dat_salmon, model = mod_list, inits = init_list, 
               control = con_list)

dfa_1$AICc #3 trends 
dfa_2$AICc #2 trends
dfa_3$AICc #1 trends = fits best

## get the estimated ZZ
Z_est <- coef(dfa_2, type = "matrix")$Z
## get the inverse of the rotation matrix
H_inv <- varimax(Z_est)$rotmat

## rotate factor loadings
Z_rot = Z_est %*% H_inv
## rotate processes
#proc_rot = solve(H_inv) %*% dfa_2$states
proc_rot = dfa_3$states

mm <- 1

yr_frst <- 1998
ylbl <- rownames(dat_salmon)
w_ts <- seq(dim(dat_salmon)[2])
layout(matrix(c(1, 2, 3, 4, 5, 6), mm, 2), widths = c(2, 1))
## par(mfcol=c(mm,2), mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
par(mai = c(0.5, 0.5, 0.5, 0.1), omi = c(0, 0, 0, 0))
## plot the processes
for (i in 1:mm) {
  ylm <- c(-1, 1) * max(abs(proc_rot[i, ]))
  ## set up plot area
  plot(w_ts, proc_rot[i, ], type = "n", bty = "L", ylim = ylm, 
       xlab = "", ylab = "", xaxt = "n")
  ## draw zero-line
  abline(h = 0, col = "gray")
  ## plot trend line
  lines(w_ts, proc_rot[i, ], lwd = 2)
  lines(w_ts, proc_rot[i, ], lwd = 2)
  ## add panel labels
  mtext(paste("State", i), side = 3, line = 0.5)
  axis(1, (0:dim(dat_salmon)[2]) + 1, yr_frst + 0:dim(dat_salmon)[2])
}
## plot the loadings

clr_0 <- viridis::plasma(6, alpha=0.7, end=0.8)
clr <- c(
  rep(clr_0[1], 5),
  rep(clr_0[2], 5),
  rep(clr_0[3], 5),
  rep(clr_0[4], 5),
  rep(clr_0[5], 3),
  rep(clr_0[6], 4))
minZ <- 0
ylm <- c(-1, 1) * max(abs(Z_rot))
for (i in 1:mm) {
  plot(c(1:N_ts)[abs(Z_rot[, i]) > minZ], as.vector(Z_rot[abs(Z_rot[, 
                                                                    i]) > minZ, i]), type = "h", lwd = 2, xlab = "", ylab = "", 
       xaxt = "n", ylim = ylm, xlim = c(0.5, N_ts + 0.5), col = clr)
  for (j in 1:N_ts) {
    if (Z_rot[j, i] > minZ) {
      text(j, -0.03, ylbl[j], srt = 90, adj = 1, cex = 1.2, 
           col = clr[j])
    }
    if (Z_rot[j, i] < -minZ) {
      text(j, 0.03, ylbl[j], srt = 90, adj = 0, cex = 1.2, 
           col = clr[j])
    }
    abline(h = 0, lwd = 1.5, col = "gray")
  }
  mtext(paste("Factor loadings on state", i), side = 3, line = 0.5)
}


plot(proc_rot[1,], type = "l", col = "blue")
lines(proc_rot[2,], type = "l", col = "red")

