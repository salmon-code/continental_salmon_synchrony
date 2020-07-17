#if they all have the same variance (bc z score, then do you need to estimate unique Rs?)
#actually you might. think the issue is that you need to group the obs by region

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
#all species regularly present
sum(na.omit(vi_1$chinook > 0)) / length(na.omit(vi_1$chinook)); sum(na.omit(vi_1$chum > 0)) / length(na.omit(vi_1$chum)); sum(na.omit(vi_1$coho > 0)) / length(na.omit(vi_1$coho)); sum(na.omit(vi_1$pink > 0)) / length(na.omit(vi_1$pink)); sum(na.omit(vi_1$sockeye > 0)) / length(na.omit(vi_1$sockeye)) 
vi_1_long <- vi_1 %>% select("year", "chinook", "coho", "chum", "pink", "sockeye") %>% pivot_longer(names_to = "species", values_to = "count", cols = c("chinook", "coho", "chum", "pink", "sockeye"))
vi_out <- vi_1_long %>% filter(count > -1) %>% group_by(year, species) %>% summarise(log_mean_count = mean(log(count + 1)))
vi_out$region <- "Vancouver Island"

ow$chinook <- ow$Chinook.mixed.age.juvenile.CPUE....km.trawled. + ow$Chinook.subyearling.CPUE....km.trawled. + ow$Chinook.yearling.CPUE....km.trawled.
ow_1 <- ow %>% select(Year, chinook, Chum.juvenile.CPUE....km.trawled., Coho.yearling.CPUE....km.trawled., Sockeye.juvenile.CPUE....km.trawled. )
names(ow_1) <- c("year", "chinook", "chum", "coho", "sockeye")
#all species regularly present
sum(na.omit(ow_1$chinook > 0)) / length(na.omit(ow_1$chinook)); sum(na.omit(ow_1$chum > 0)) / length(na.omit(ow_1$chum)); sum(na.omit(ow_1$coho > 0)) / length(na.omit(ow_1$coho)); sum(na.omit(ow_1$pink > 0)) / length(na.omit(ow_1$pink)); sum(na.omit(ow_1$sockeye > 0)) / length(na.omit(ow_1$sockeye)) 
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
#all species regularly present
ak_1_long %>% group_by(region, species) %>% summarise(sum(na.omit(count) > 0) / length(na.omit(count)))
ak_out <- ak_1_long %>% filter(count > -1 & region != "West Gulf of Alaska") %>% group_by(year, species, region) %>% summarise(log_mean_count = mean(log(count + 1)))
all_out_1 <- bind_rows(ps_out, vi_out, ow_out, ak_out)
#get rid of odd year pinks from ps and vi because MARSS cannot handle their error terms
all_out_1[all_out_1$species == "pink" &
  (all_out_1$region == "Puget Sound" | all_out_1$region == "Vancouver Island") &
    all_out_1$year %% 2 != 0,]$log_mean_count <- NA
all_out <- all_out_1 %>% group_by(region, species) %>% mutate(log_mean_count_z = (log_mean_count - mean(na.omit(log_mean_count))) / sd(na.omit(log_mean_count)))


ggplot(aes(x = year, y = log_mean_count_z, col = species), data = all_out) +
  geom_line() + 
  geom_point() +
  facet_wrap(~region) +
  geom_vline(aes(xintercept = 2014)) +
  geom_vline(aes(xintercept = 2015))

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
all_out_wide <- left_join(all_out_wide_4, vi_out_wide %>% ungroup() %>% select(-"region") %>% ungroup())

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

## 'aa' is the offset/scaling
aa <- "zero"
## 'DD' and 'd' are for covariates
DD <- "zero"  # matrix(0,mm,1)
dd <- "zero"  # matrix(0,1,wk_last)
## 'RR' is var-cov matrix for obs errors
#RR <- "diagonal and unequal"

#grouping by region
#see above. you need to have a list of values, then you put them in a matrix
R_vals <- list("ow_v",	"ow_c",	"ow_c",	"ow_c", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #23 zeros
                   "ow_c",	"ow_v",	"ow_c",	"ow_c", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #23 zeros
                   "ow_c",	"ow_c",	"ow_v",	"ow_c", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #23 zeros
                   "ow_c",	"ow_c",	"ow_c",	"ow_v", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #23 zeros
                   0, 0, 0, 0, "nbs_v",	"nbs_c",	"nbs_c",	"nbs_c",	"nbs_c", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #4, 18 zeros
                   0, 0, 0, 0, "nbs_c",	"nbs_v",	"nbs_c",	"nbs_c",	"nbs_c", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #4, 18 zeros
                   0, 0, 0, 0, "nbs_c",	"nbs_c",	"nbs_v",	"nbs_c",	"nbs_c", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #4, 18 zeros
                   0, 0, 0, 0, "nbs_c",	"nbs_c",	"nbs_c",	"nbs_v",	"nbs_c", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #4, 18 zeros
                   0, 0, 0, 0, "nbs_c",	"nbs_c",	"nbs_c",	"nbs_c",	"nbs_v", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #4, 18 zeros
                   0, 0, 0, 0, 0, 0, 0, 0, 0, "ega_v",	"ega_c",	"ega_c",	"ega_c",	"ega_c", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #9, 13 zeros
                   0, 0, 0, 0, 0, 0, 0, 0, 0, "ega_c",	"ega_v",	"ega_c",	"ega_c",	"ega_c", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, "ega_c",	"ega_c",	"ega_v",	"ega_c",	"ega_c", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, "ega_c",	"ega_c",	"ega_c",	"ega_v",	"ega_c", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, "ega_c",	"ega_c",	"ega_c",	"ega_c",	"ega_v", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "ps_v",	"ps_c", "ps_c", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #14, 10
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "ps_c",	"ps_v", "ps_c", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "ps_c",	"ps_c", "ps_v", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "sbs_v",	"sbs_c",	"sbs_c",	"sbs_c",	"sbs_c", 0, 0, 0, 0, 0, #17, 5
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  "sbs_c",	"sbs_v",	"sbs_c",	"sbs_c",	"sbs_c", 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "sbs_c",	"sbs_c",	"sbs_v",	"sbs_c",	"sbs_c", 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "sbs_c",	"sbs_c",	"sbs_c",	"sbs_v",	"sbs_c", 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "sbs_c",	"sbs_c",	"sbs_c",	"sbs_c",	"sbs_v", 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "vi_v",	"vi_c",	"vi_c",	"vi_c",	"vi_c", #22, 0
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "vi_c",	"vi_v",	"vi_c",	"vi_c",	"vi_c",
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "vi_c",	"vi_c",	"vi_v",	"vi_c",	"vi_c",
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "vi_c",	"vi_c",	"vi_c",	"vi_v",	"vi_c",
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "vi_c",	"vi_c",	"vi_c",	"vi_c",	"vi_v")
                  
RR <- matrix(R_vals, nrow = N_ts, ncol = N_ts, byrow = TRUE)

#zeros need to not have quotes...

## number of processes
mm <- 3
## 'BB' is identity: 1's along the diagonal & 0's elsewhere
BB <- "identity"  # diag(mm)
## 'uu' is a column vector of 0's
uu <- "zero"  # matrix(0,mm,1)
## 'CC' and 'cc' are for covariates
CC <- "zero"  # matrix(0,mm,1)
cc <- "zero"  # matrix(0,1,wk_last)
## 'QQ' is identity
QQ <- "identity"  # diag(mm)
#QQ <- r_vcv[, -1] %>% as.matrix()

## list with specifications for model vectors/matrices
mod_list <- list(Z = ZZ, A = aa, D = DD, d = dd, R = RR, B = BB, 
                 U = uu, C = CC, c = cc, Q = QQ)
## list with model inits
init_list <- list(x0 = matrix(rep(0, mm), mm, 1))
## list with model control parameters
con_list <- list(maxit = 10000, allow.degen = TRUE)

## fit MARSS
dfa_1 <- MARSS(y = dat_salmon, model = mod_list, inits = init_list, 
               control = con_list)

isSymmetric.matrix(RR)
#next steps are going to be to think about covariance matrix for fish from the same regions  

diag(c("a"))

isSymmetric.matrix(matrix(c("a","c","c","b"), 2 ,2))
