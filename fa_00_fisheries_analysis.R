# RUN FUNCTIONS HERE

# Call the function to install and load packages
install_load_packages(required_packages)

# Set path to replication package
if(Sys.info()[["user"]]=="mpk9bp"){
  setwd("/Users/mpk9bp/Documents/Fisheries")
  }

# Set path to location for figures
figs <- "Figures"

# Run indicator
# Set equal to 1 for first time running
# Script will save models and data as new variables are added and will not need to be re-run
#run = 1
run = 0
set.seed(1234)

#if this is the first time, load and clean and merge the data.  If it's not the first time the cleaned and merged data will load.
if(run == 1){
  #Section 1: Load and Clean Data ----------------------------------------------------
  point1_all <- read_dta("ians_files/Data/analysis_data.dta") #this takes about 13 minutes
  
  # Select relevant variables
  point1_all <- point1_all %>% dplyr::select(year,lat_id,lon_id,is_inside_eez, npp_standard,sst_modis,sss,Depth,total_fishing,ever_fished,always_fished,tf_asinh,ProvCode)
  
  # Select relevant years
  point1_all <- point1_all %>% subset(year == 2017 | year == 2018 | year == 2019)
  
  # Cleanup
  gc(full=TRUE)
  
  # Format LH Indicator
  point1_all$lh <-as.factor(point1_all$ProvCode)
  
  # Format EEZ Indicator
  point1_all$eez <-as.factor(point1_all$is_inside_eez)
  
  # Generate asinh fishing effort 
  point1_all$asinh_total_fishing <- asinh(point1_all$total_fishing)
  
  # Generate log10 fishing effort (Do I need this for all the data or just 2018?)
  point1_all$log10_total_fishing <- log10(point1_all$total_fishing)
  
  point1 <- point1_all %>% subset(year == 2018)
  
  # Drop cells where predictors are missing
  point1_all <- point1_all %>% subset(!(is.na(point1_all$npp_standard)) &
                                        !(is.na(point1_all$Depth)) &
                                        !(is.na(point1_all$sst_modis)) &
                                        !(is.na(point1_all$sss)))
  
  # Drop if not in a Longhurst region
  point1_all <- point1_all %>% subset(ProvCode != "NA")
  
  # Cleanup
  gc(full=TRUE)
  
  # Keep relevant years as separate datasets
  point1_2017 <- point1_all %>% subset(year == 2017)
  point1 <- point1_all %>% subset(year == 2018)
  point1_2019 <- point1_all %>% subset(year == 2019)
  
  # Drop dataframes
  rm(ls="point1_all")
  
  # Cleanup
  gc(full=TRUE)
}else{
  # Load merged and cleaned data
  # (do this for '17 and '19 and '23 in fa_14_explore_new_data.R)
  point1 <- read_dta("new_data/analysis_data_2018_new_gfw_data_v6.dta")
  # point1 <- read_dta("new_data/analysis_data_2018_viirs_v7.dta")
}



point1$fishing_hours_new <- point1$fishing_hours_new_data
point1$fishing_hours_new_data <- NULL

point1$asinh_total_fishing_new <- point1$asinh_total_fishing_new_data
point1$asinh_total_fishing_new_data <- NULL

point1$EEZ_J_highseas <- as_factor(point1$EEZ_J_highseas) # Use as_factor rather than as.factor so that it takes the labels from the original haven_labelled variable.
head(point1$EEZ_J_highseas)
gc(full = TRUE)

colnames(point1)

point1 <- point1 %>% select(-fitted21, -fitted25, -fitted50, -fitted25_same, -fitted50_same,
                            -PredictedFished_1, -PredictedFished_1_25percent, -PredictedFished_1_50percent, -PredictedFished_1_75percent,
                            -FalseZeros_mod1_25percent, -FalseZeros_mod1_50percent, -FalseZeros_mod1_75percent,
                            -asinh_total_fishing_25_percent, -asinh_total_fishing_50_percent, -asinh_total_fishing_75_percent,
                            -asinh_total_fishing_0_percent, -asinh_total_fishing_highseas, -asinh_total_fishing_eezs, 
                            -residual21, -residual25_extrap, -residual50_extrap, -residual25_same, -residual50_same, -lon_id_round, -lat_id_round,
                            -old_new_difference)

colnames(point1)
point1 <- point1 %>% select(-tf_ihs, -log10_total_fishing)
gc(full = TRUE)


summary(point1$port_distance)

distance_q <- as.numeric(quantile(point1$port_distance, probs = c(.01,.995), na.rm = TRUE))
distance_q

plot_map(
  data = point1,
  column = "port_distance",
  title = "port_distance",
  file_name = "port_distance",
  lims = c(0, distance_q[2])
)

distance_q <- as.numeric(quantile(point1$distance, probs = c(.01,.995), na.rm = TRUE))
distance_q
plot_map(
  data = point1,
  column = "distance",
  title = "distance",
  file_name = "distance",
  lims = c(0, distance_q[2])
)


point1 <- point1 %>% select(-total_fishing_old, -ever_fished_old, -always_fished_old, -asinh_total_fishing_old, -lat_rad,
                            -lon_rad, -closest_lat, -closest_lon, -lh_eezj, -fished_old, -distance)
colnames(point1)

sample(point1$ProvCode, 20)
sample(point1$lh, 20)

point1$ProvCode <- as_factor(point1$ProvCode) # Use as_factor rather than as.factor so that it takes the labels from the original haven_labelled variable.
point1$lh <- as_factor(point1$lh) # Use as_factor rather than as.factor so that it takes the labels from the original haven_labelled variable.

sample(point1$ProvCode, 10)
sample(point1$lh, 10)
point1 <- point1 %>% select(-lh)
colnames(point1)

write_dta(point1, "new_data/analysis_data_2018_cleaned_v7.dta") 

# # Rename column where names is "ihs" to "asinh
# names(point1)[names(point1) == "ihs_total_fishing"] <- "asinh_total_fishing"
# 
# #Add highseas to eez by jurisdiction (replace NAs with "high_seas")
# point1$EEZ_J_highseas <- point1$EEZ_Jurisdiction
# 
# point1 <- point1 %>%
#   mutate(EEZ_J_highseas = ifelse(EEZ_J_highseas == "", "high_seas", EEZ_J_highseas))



#format EEZ Jurisdiction
# point1$EEZ_J_highseas <- as.factor(point1$EEZ_J_highseas)
# # Format factors as factors
# point1$lh <-as.factor(point1$lh)
# point1$ProvCode <-as.factor(point1$ProvCode)
# point1$lh_eezj <- as.factor(point1$lh_eezj)
# point1$EEZ_Jurisdiction <- as.factor(point1$EEZ_Jurisdiction)

# Generate binary `fished` indicator
point1 <- point1 %>% mutate(fished = case_when(total_fishing > 0 ~ 1,
                                               total_fishing == 0 ~ 0))




point1_2017 <- read_dta("new_data/analysis_data_2017_kw_hours.dta") 


point1_2017$EEZ_J_highseas <- as_factor(point1_2017$EEZ_J_highseas) # Use as_factor rather than as.factor so that it takes the labels from the original haven_labelled variable.

point1_2017 <- point1_2017 %>% select(-total_fishing, -ever_fished, -always_fished, -tf_ihs, -lh,
                            -lh_eez, -asinh_total_fishing, -lat_rad, -lon_rad, -closest_lat, -closest_lon, -distance,
                            -lh_eezj, -lon_id_round, -lat_id_round, -old_new_difference)
colnames(point1_2017)

point1_2017 <- point1_2017 %>% mutate(fished_new = case_when(kw_hours > 0 ~ 1,
                                                         kw_hours == 0 ~ 0))

sum(point1_2017$fished_new)/nrow(point1_2017) #over 70% of the 2017 data is zero

point1_2017 <- point1_2017 %>%
  mutate(
    cell_area = 11.12 * 11.12 * abs(cos(lat_id * pi / 180))
  )

summary(point1_2017$cell_area)

point1_2017 <- point1_2017 %>%
  mutate(
    kw_hours_norm = kw_hours / cell_area
  )

point1_2017 <- point1_2017 %>%
  mutate(
    asinh_kw_hours_norm = asinh(kw_hours_norm)
  )

write_dta(point1_2017, "new_data/analysis_data_2017_cleaned_v7.dta") 

gc(full = TRUE)


# RUN MAPS HERE

# Section 2: Extensive Margin Analysis ----------------------------------------------------

# Create random subsample (10%)
sample <- sample(nrow(point1), size=ceiling(.1*nrow(point1)))
point1_random <- point1[sample, ]
point1_extra <- setdiff(point1,point1_random)

point1_fished <- point1[point1$asinh_total_fishing != 0, ]
# Create random subsample for point1_fished (10%)
sample_fished <- sample(nrow(point1_fished), size=ceiling(.1*nrow(point1_fished)))
point1_fished_random <- point1_fished[sample_fished, ]
point1_fished_extra <- setdiff(point1_fished,point1_fished_random)

gc(full = TRUE)

# RUN MODELS HERE

## Model Statistics ----

# Get summary stats

summary_21 <- summary(mod21)
summary_22 <- summary(mod22)
summary_23 <- summary(mod23)
summary_24 <- summary(mod24)

summary_1 <- summary(logitmod1)
summary_2 <- summary(logitmod2)
summary_3 <- summary(logitmod3)
summary_4 <- summary(logitmod4)


summary_21$r.sq # asinh complete
summary_22$r.sq # asinh fished
summary_23$r.sq # log10 complete
summary_24$r.sq # log10 fished

summary_1$r.sq # Logit predictors
summary_2$r.sq # Logit longhurst regions
summary_3$r.sq # Logit predictors no EEZ
summary_4$r.sq # Logit longhurst regions no EEZ

#get the pseudo R^2 (can also get this from the summary)
deviance_total <- mod19$deviance
deviance_null <- mod19$null.deviance
pseudo_r_squared_19 <- 1 - deviance_total / deviance_null
print(pseudo_r_squared_19)

deviance_total <- mod21$deviance
deviance_null <- mod21$null.deviance
pseudo_r_squared_21 <- 1 - deviance_total / deviance_null
print(pseudo_r_squared_21)

sum(mod21$edf)
sum(mod23$edf)
sum(mod24$edf)



# Make a table
rows <- c("Model Number", "Adj. R2", "Sea Salt Salinity", "Distance to Port")
stats9 <- c(9, round(summary_9$r.sq,3), "Yes", "No")
stats10 <- c(10, round(summary_10$r.sq,3), "No", "No")
stats11 <- c(11, round(summary_11$r.sq,3), "Yes", "Yes")
stats12 <- c(12, round(summary_12$r.sq,3), "No", "Yes")

tab_sss_port <- cbind(rows,stats9,stats10,stats11,stats12)
print(xtable(tab_sss_port), file = paste0("Tables/tab_sss_port.tex"), compress = FALSE)

install.packages("pander")
library(pander)
pander(tab_sss_port)

point1_2017_test <- point1_2017 %>% subset(lh_eez %in% unique(point1$lh_eez))
point1_2019_test <- point1_2019 %>% subset(lh_eez %in% unique(point1$lh_eez))

pred_9_2017 <- predict.bam(mod9,type="response",newdata=point1_2017_test)
pred_10_2017 <- predict.bam(mod10,type="response",newdata=point1_2017_test)
pred_11_2017 <- predict.bam(mod11,type="response",newdata=point1_2017_test)
pred_12_2017 <- predict.bam(mod12,type="response",newdata=point1_2017_test)

pred_9_2019 <- predict.bam(mod9,type="response",newdata=point1_2019_test)
pred_10_2019 <- predict.bam(mod10,type="response",newdata=point1_2019_test)
pred_11_2019 <- predict.bam(mod11,type="response",newdata=point1_2019_test)
pred_12_2019 <- predict.bam(mod12,type="response",newdata=point1_2019)

point1_extra <- point1_extra %>% subset(lh_eez %in% unique(point1_random$lh_eez))

# pred_9_rss <- predict.bam(mod9_rss,type="response",newdata=point1_extra)
# pred_10_rss <- predict.bam(mod10_rss,type="response",newdata=point1_extra)
pred_11_rss <- predict.bam(mod11_rss,type="response",newdata=point1_extra)
# pred_12_rss <- predict.bam(mod12_rss,type="response",newdata=point1_extra)
pred_13_rss <- predict.bam(mod13_rss,type="response",newdata=point1_extra)
pred_15_rss <- predict.bam(mod15_rss,type="response",newdata=point1_extra)
pred_16_rss <- predict.bam(mod16_rss,type="response",newdata=point1_extra)
#pred_18_rss <- predict.bam(mod18_rss,type="response",newdata=point1_extra)

# # Predict Marginal Effect at x and x + 10%
# 
# sss_q <- as.numeric(quantile(point1$sss, probs = c(.01,.99)))
# depth_q <- as.numeric(quantile(point1$Depth, probs = c(.01,.99)))
# npp_q <- as.numeric(quantile(point1$npp_standard, probs = c(.01,.99)))
# sst_q <- as.numeric(quantile(point1$sst_modis, probs = c(.01,.99)))
# 
# npp_censor <- point1 %>% subset( npp_standard > npp_q[1] & npp_standard < npp_q[2])
# depth_censor <- point1 %>% subset( Depth > depth_q[1] & Depth < depth_q[2])
# sss_censor <- point1 %>% subset( sss > sss_q[1] & sss < sss_q[2])
# sst_censor <- point1 %>% subset( sst_modis > sst_q[1] & sst_modis < sst_q[2])
# 
# # Depth
# pred_depth0 <- predict.gam(mod9, type = "link", newdata = tibble(asinh_total_fishing = NA,
#                                                                  lh_eez="ALSK0",
#                                                                  Depth = mean(point1$Depth),
#                                                                  sst_modis = mean(point1$sst_modis),
#                                                                  npp_standard = mean(point1$npp_standard),
#                                                                  sss = mean(point1$sss)))
# # Plus 10%
# pred_depth_up1 <- predict.gam(mod9, type = "link", newdata = tibble(asinh_total_fishing = NA,
#                                                                     lh_eez="ALSK0",
#                                                                     Depth = 1.01*mean(point1$Depth),
#                                                                     sst_modis = mean(point1$sst_modis),
#                                                                     npp_standard = mean(point1$npp_standard),
#                                                                     sss = mean(point1$sss)))
# # Minus 10%
# pred_depth_down1 <- predict.gam(mod9, type = "link", newdata = tibble(asinh_total_fishing = NA,
#                                                                       lh_eez="ALSK0",
#                                                                       Depth = .99*mean(point1$Depth),
#                                                                       sst_modis = mean(point1$sst_modis),
#                                                                       npp_standard = mean(point1$npp_standard),
#                                                                       sss = mean(point1$sss)))
# 
# # Change in asinh fishing
# mod9_depth_ME_up <- pred_depth_up1 - pred_depth0 # decrease
# (exp(mod9_depth_ME_up)-1) * 100
# mod9_depth_ME_down <- pred_depth_down1 - pred_depth0 # increase
# (exp(mod9_depth_ME_down)-1) * 100
# 
# ## NPP
# pred_npp0 <- predict.gam(mod9, type = "link",  newdata = tibble(asinh_total_fishing = NA,
#                                                                 lh_eez="ALSK0",
#                                                                 Depth = mean(point1$Depth),
#                                                                 sst_modis = mean(point1$sst_modis),
#                                                                 npp_standard = mean(point1$npp_standard),
#                                                                 sss = mean(point1$sss)))
# pred_npp_up1 <- predict.gam(mod9, type = "link",  newdata = tibble(asinh_total_fishing = NA,
#                                                                    lh_eez="ALSK0",Depth = mean(point1$Depth),
#                                                                    sst_modis = mean(point1$sst_modis),
#                                                                    npp_standard = 1.01*mean(point1$npp_standard),
#                                                                    sss = mean(point1$sss)))
# pred_npp_down1 <- predict.gam(mod9, type = "link",  newdata = tibble(asinh_total_fishing = NA,
#                                                                      lh_eez="ALSK0",Depth = mean(point1$Depth),
#                                                                      sst_modis = mean(point1$sst_modis),
#                                                                      npp_standard = .99*mean(point1$npp_standard),
#                                                                      sss = mean(point1$sss)))
# 
# 
# # Change in asinh fishing
# mod9_npp_ME_up <- pred_npp_up1 - pred_npp0 # decrease
# (1-exp(mod9_npp_ME_up)) * 100
# mod9_npp_ME_down <- pred_npp_down1 - pred_npp0 # increase
# (1-exp(mod9_npp_ME_down)) * 100
# 
# 
# ## SST
# pred_sst0 <- predict.gam(mod9, type = "link",  newdata = tibble(asinh_total_fishing = NA,
#                                                                 lh_eez="ALSK0",
#                                                                 Depth = mean(point1$Depth),
#                                                                 sst_modis = mean(point1$sst_modis),
#                                                                 npp_standard = mean(point1$npp_standard),
#                                                                 sss = mean(point1$sss)))
# pred_sst_up1 <- predict.gam(mod9, type = "link",  newdata = tibble(asinh_total_fishing = NA,
#                                                                    lh_eez="ALSK0",Depth = mean(point1$Depth),
#                                                                    sst_modis = 1.01*mean(point1$sst_modis),
#                                                                    npp_standard = mean(point1$npp_standard),
#                                                                    sss = mean(point1$sss)))
# pred_sst_down1 <- predict.gam(mod9, type = "link",  newdata = tibble(asinh_total_fishing = NA,
#                                                                      lh_eez="ALSK0",Depth = mean(point1$Depth),
#                                                                      sst_modis = .90*mean(point1$sst_modis),
#                                                                      npp_standard = mean(point1$npp_standard),
#                                                                      sss = mean(point1$sss)))
# 
# # Change in asinh fishing
# mod9_sst_ME_up <- pred_sst_up1 - pred_sst0 # decrease
# (1-exp(mod9_sst_ME_up)) * 100
# mod9_sst_ME_down <- pred_sst_down1 - pred_sst0 # increase
# (1-exp(mod9_sst_ME_down)) * 100
# 
# ## SSS
# 
# pred_sss0 <- predict.gam(mod9, type = "link",  newdata = tibble(asinh_total_fishing = NA,
#                                                                 lh_eez="ALSK0",
#                                                                 Depth = mean(point1$Depth),
#                                                                 sst_modis = mean(point1$sst_modis),
#                                                                 npp_standard = mean(point1$npp_standard),
#                                                                 sss = mean(point1$sss)))
# pred_sss_up1 <- predict.gam(mod9, type = "link",  newdata = tibble(asinh_total_fishing = NA,
#                                                                    lh_eez="ALSK0",Depth = mean(point1$Depth),
#                                                                    sst_modis = mean(point1$sst_modis),
#                                                                    npp_standard = mean(point1$npp_standard),
#                                                                    sss = 1.01*mean(point1$sss)))
# pred_sss_down1 <- predict.gam(mod9, type = "link",  newdata = tibble(asinh_total_fishing = NA,
#                                                                      lh_eez="ALSK0",Depth = mean(point1$Depth),
#                                                                      sst_modis = mean(point1$sst_modis),
#                                                                      npp_standard = mean(point1$npp_standard),
#                                                                      sss = .99*mean(point1$sss)))
# 
# # Change in asinh fishing
# mod9_sss_ME_up <- pred_sss_up1 - pred_sss0 # decrease
# (1-exp(mod9_sss_ME_up)) * 100
# mod9_sss_ME_down <- pred_sss_down1 - pred_sss0 # increase
# (1-exp(mod9_sss_ME_down)) * 100
# 
# adj_r2_9 <- 1- (sum(mod9$residuals^2)/(summary_9$n - summary_9$np)) / (sum((mod9$y - mean(mod9$y))^2)/(summary_9$n - 1))
# adj_r2_10 <- 1- (sum(mod10$residuals^2)/(summary_10$n - summary_10$np)) / (sum((mod10$y - mean(mod10$y))^2)/(summary_10$n - 1))
# adj_r2_11 <- 1- (sum(mod11$residuals^2)/(summary_11$n - summary_11$np)) / (sum((mod11$y - mean(mod11$y))^2)/(summary_11$n - 1))


stats9 <- c(round(summary_9$r.sq,3),
            # round(mod8$aic,2), # AIC
            #           summary_8$rank, # Rank
            #           round(summary_8$r.sq,2), # Adj r-squared
            #           round(summary_8$dev.expl,2), # Deviance Explained
            #           summary_8$n, # N Obs
            round(cor(pred_9_2017, point1_2017$asinh_total_fishing),2),
            round(cor(pred_9_rss,point1_extra$asinh_total_fishing),2),
            round(cor(pred_9_2019, point1_2019$asinh_total_fishing),2)) # 2019 Corr(y,yhat)

stats10 <- c(round(summary_10$r.sq,3),
             # round(mod8$aic,2), # AIC
             #           summary_8$rank, # Rank
             #           round(summary_8$r.sq,2), # Adj r-squared
             #           round(summary_8$dev.expl,2), # Deviance Explained
             #           summary_8$n, # N Obs
             round(cor(pred_10_2017, point1_2017$asinh_total_fishing),2),
             round(cor(pred_10_rss,point1_extra$asinh_total_fishing),2), 
             round(cor(pred_10_2019, point1_2019$asinh_total_fishing),2)) # 2019 Corr(y,yhat)

stats11 <- c(round(summary_11$r.sq,3),
             # round(mod8$aic,2), # AIC
             #           summary_8$rank, # Rank
             #           round(summary_8$r.sq,2), # Adj r-squared
             #           round(summary_8$dev.expl,2), # Deviance Explained
             #           summary_8$n, # N Obs
             round(cor(pred_11_2017, point1_2017$asinh_total_fishing),2),
             round(cor(pred_11_rss,point1_extra$asinh_total_fishing),2),
             round(cor(pred_11_2019, point1_2019$asinh_total_fishing),2)) # 2019 Corr(y,yhat)


stats12 <- c(round(summary_12$r.sq,3),
             # round(mod8$aic,2), # AIC
             #           summary_8$rank, # Rank
             #           round(summary_8$r.sq,2), # Adj r-squared
             #           round(summary_8$dev.expl,2), # Deviance Explained
             #           summary_8$n, # N Obs
             round(cor(pred_11_2017, point1_2017$asinh_total_fishing),2),
             round(cor(pred_11_rss,point1_extra$asinh_total_fishing),2),
             round(cor(pred_11_2019, point1_2019$asinh_total_fishing),2)) # 2019 Corr(y,yhat)

## Table Stats
rows <- c( "Adj. R2", "Cor(Y_2017,hat Y_2017)", "Cor(Y_2018,hat Y_2018)","Cor(Y_2019,hat Y_2019)")
tab_asinhnorm <- cbind(rows,stats9,stats10,stats11,stats12)
xtable(tab_asinhnorm)
print(xtable(tab_asinhnorm), file = paste0("Tables/tab_asinhnorm_stats.tex"), compress = FALSE)


## Marginal Effects
rows_MEs <- c("Depth", "NPP", "SST", "SSS")
names <- c("Predictor","Increase","Decrease","Change (levels)")
tab_asinhnorm_MEs_trans9 <- rbind(names,cbind(rows_MEs, 
                                            c(paste0(round(100 *(exp(mod9_depth_ME_up)-1),2),"%"),paste0(round( 100 * (exp(mod9_npp_ME_up)-1),2),"%"),paste0(round( 100 * (exp(mod9_sst_ME_up)-1),2),"%"),paste0(round( 100 * (exp(mod9_sss_ME_up)-1),2),"%")),
                                            c(paste0(round(100 *(exp(mod9_depth_ME_down)-1),2),"%"),paste0(round( 100 * (exp(mod9_npp_ME_down)-1),2),"%"),paste0(round( 100 * (exp(mod9_sst_ME_down)-1),2),"%"),paste0(round( 100 * (exp(mod9_sss_ME_down)-1),2),"%")),
                                            c(round(.01*mean(point1$Depth),2), round(.01*mean(point1$npp_standard),2),round(.01*mean(point1$sst_modis),2),round(.01*mean(point1$sss),2 )  )))
xtable(tab_asinhnorm_MEs_trans9)
print(xtable(tab_asinhnorm_MEs_trans9), file = paste0("Tables/tab_asinhnorm_MEs_mod9.tex"), compress = FALSE)



# Figure 3: Distribution of asinh fishing effort for 2018. 
# Distributions of observed and predicted asinh fishing effort. Note, figure illustrates the density of observed and predicted asinh fishing effort in 2018. Expand caption; add units

asinh_fishing_density <- ggplot(point1) +
  geom_density(bw=.1,aes(x = asinh_total_fishing,color = "Observed",fill = 'Observed'),alpha = .7,show.legend=TRUE) +
  geom_density(bw=.1,aes(x = PredictedasinhFishing_21,color = "Predicted (mod 21)",fill = 'Predicted (mod 21)'),alpha = .5) +
  scale_fill_manual("Values", values = c('Observed' = "gray59", 'Predicted (mod 21)' = "gray59"), aesthetics = c("color", "fill")) +
  scale_x_continuous(name = "asinh Fishing Effort, asinh(hrs/100km^2)", 
                     expand = c(0,0),
                     limits = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]))   +
  scale_y_continuous(name = "Density", 
                     expand = c(0,0)) +
  theme_pubr() +
  theme(legend.position = "bottom", 
        legend.title = element_blank())  

ggsave(paste0(figs,"/fig6_fishing_density_v3.png"),asinh_fishing_density,width = 20,height = 10,units ="cm")

# Histogram of asinh(total_fishing)
asinh_total_fishing_hist <-ggplot(point1, aes(x = asinh_total_fishing, y = after_stat(count / sum(count) * 100))) +
  geom_histogram(binwidth = 0.2, fill = "gray59", color = "black") +
  labs(title = "Histogram of asinh(total fishing data)", x = "asinh(hrs/km^2)", y = "Percentage") +
  scale_x_continuous(limits = c(-.3, 6))+
  theme_minimal()+ 
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80", size = 0.5),  
        panel.grid.minor = element_line(color = "gray90", size = 0.3))

ggsave(paste0(figs,"/asinh_total_fishing_hist.png"),asinh_total_fishing_hist,width = 10,height = 10,units ="cm")


#Histogram of asinh(total_fishing) short
asinh_total_fishing_hist_scaled <-ggplot(point1, aes(x = asinh_total_fishing, y = after_stat(count / sum(count) * 100))) +
  geom_histogram(binwidth = 0.2, fill = "gray59", color = "black") +
  labs(title = "Histogram of asinh(total fishing data)", x = "asinh(hrs/km^2)", y = "Percentage") +
  scale_x_continuous(limits = c(-.3, 6))+
  scale_y_continuous(limits = c(-.3, 2), oob = scales::squish)+
  theme_minimal()+ 
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80", size = 0.5),  
        panel.grid.minor = element_line(color = "gray90", size = 0.3))+
  annotate("segment", x = 0, xend = 0, y = 1.8, yend = 2, 
           arrow = arrow(type = "closed", length = unit(0.2, "cm")), color = "red", size = 1) 
#+annotate("text", x = 0, y = 1.5, label = "Higher than 2%", vjust = -0.5, color = "red", size = 5)

ggsave(paste0(figs,"/asinh_total_fishing_hist_scaled_v3.png"),asinh_total_fishing_hist_scaled,width = 10,height = 10,units ="cm")


# Histogram of Model 21: predicted asinh(total_fishing)
asinh_total_fishing_hist <-ggplot(point1, aes(x = CHANGETHIS, y = after_stat(count / sum(count) * 100))) +
  geom_histogram(binwidth = 0.2, fill = "gray59", color = "black") +
  labs(title = "Histogram of asinh(total fishing data)", x = "asinh(hrs/km^2)", y = "Percentage") +
  scale_x_continuous(limits = c(-.3, 6))+
  theme_minimal()+ 
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80", size = 0.5),  
        panel.grid.minor = element_line(color = "gray90", size = 0.3))

ggsave(paste0(figs,"/asinh_total_fishing_hist.png"),asinh_total_fishing_hist,width = 10,height = 10,units ="cm")

# Histogram of Model 21 and fishing data
hist_mod21 <- ggplot() +
  # First histogram (original asinh total fishing)
  geom_histogram(data = point1, aes(x = asinh_total_fishing, y = after_stat(count / sum(count) * 100), fill = "Observed"), 
                 binwidth = 0.2, color = "black", alpha = 0.5) +  
  # Second histogram (model-predicted asinh total fishing)
  geom_histogram(data = point1, aes(x = PredictedasinhFishing_21, y = after_stat(count / sum(count) * 100), 
                                    fill = "Predicted (21)"), 
                 binwidth = 0.2, color = "black", alpha = 0.5) +  
  # Labels and theme
  labs(title = "Histogram of asinh(fishing data)", 
       x = "asinh(hrs/km^2)", 
       y = "Percentage", 
       fill = "Legend") +  
  scale_x_continuous(limits = c(-0.3, 6)) +
  scale_fill_manual(values = c("Observed" = "#A6739B", "Predicted (21)" = "steelblue")) +  
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80", size = 0.5),
        panel.grid.minor = element_line(color = "gray90", size = 0.3))

# Save the figure
ggsave(paste0(figs, "/hist_mod21_v2.png"), hist_mod21, width = 13, height = 10, units = "cm")

# Histogram of Model 21 and fishing data truncated
hist_mod21_trunc <- ggplot() +
  # First histogram (original asinh total fishing)
  geom_histogram(data = point1, aes(x = asinh_total_fishing, y = after_stat(count / sum(count) * 100), fill = "Observed"), 
                 binwidth = 0.2, color = "black", alpha = 0.5) +  
  # Second histogram (model-predicted asinh total fishing)
  geom_histogram(data = point1, aes(x = PredictedasinhFishing_21, y = after_stat(count / sum(count) * 100), 
                                    fill = "Predicted (21)"), 
                 binwidth = 0.2, color = "black", alpha = 0.5) +  
  # Labels and theme
  labs(title = "Histogram of asinh(fishing data)", 
       x = "asinh(hrs/km^2)", 
       y = "Percentage", 
       fill = "Legend") +  
  scale_x_continuous(limits = c(-0.3, 6)) +
  scale_y_continuous(limits = c(-.3, 15), oob = scales::squish)+
  scale_fill_manual(values = c("Observed" = "#A6739B", "Predicted (21)" = "steelblue")) +  
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80", size = 0.5),
        panel.grid.minor = element_line(color = "gray90", size = 0.3)) + 
  annotate("segment", x = 0, xend = 0, y = 13, yend = 15, 
           arrow = arrow(type = "closed", length = unit(0.2, "cm")), color = "red", size = 1) 

# Save the figure
ggsave(paste0(figs, "/hist_mod21_truncated_v3.png"), hist_mod21_trunc, width = 13, height = 10, units = "cm")

# Histogram of Model 22 and fished data
hist_mod22 <- ggplot() +
  # First histogram (masked asinh total fishing)
  geom_histogram(data = point1_fished, aes(x = asinh_total_fishing, y = after_stat(count / sum(count) * 100), 
                                           fill = "Observed (fished)"), 
                 binwidth = 0.2, color = "black", alpha = 0.5) +  
  # Second histogram (model-predicted asinh total fishing)
  geom_histogram(data = point1_fished, aes(x = fitted22, y = after_stat(count / sum(count) * 100), 
                                           fill = "Predicted (22)"), 
                 binwidth = 0.2, color = "black", alpha = 0.5) +  
  # Labels and theme
  labs(title = "Histogram of asinh(masked fishing data)", 
       x = "asinh(hrs/km^2)", 
       y = "Percentage", 
       fill = "Legend") +  
  scale_x_continuous(limits = c(-0.3, 6)) +
  scale_fill_manual(values = c("Observed (fished)" = "#A6739B", "Predicted (22)" = "steelblue")) +  
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80", size = 0.5),
        panel.grid.minor = element_line(color = "gray90", size = 0.3))

# Save the figure
ggsave(paste0(figs, "/hist_mod22.png"), hist_mod22, width = 13, height = 10, units = "cm")

# Histogram of Model 24 and fished data
hist_mod24 <- ggplot() +
  # First histogram (log10 masked fishing data)
  geom_histogram(data = point1_fished, aes(x = log10_total_fishing, y = after_stat(count / sum(count) * 100), 
                                           fill = "Observed (fished)"), 
                 binwidth = 0.2, color = "black", alpha = 0.5) +  
  # Second histogram (model-predicted asinh total fishing)
  geom_histogram(data = point1_fished, aes(x = PredictedLog10Fishing_24, y = after_stat(count / sum(count) * 100), 
                                           fill = "Predicted (24)"), 
                 binwidth = 0.2, color = "black", alpha = 0.5) +  
  # Labels and theme
  labs(title = "Histogram of Log10(masked fishing data)", 
       x = "Log10(hrs/km^2)", 
       y = "Percentage", 
       fill = "Legend") +  
  scale_x_continuous(limits = c(-0.3, 3.5)) +
  scale_fill_manual(values = c("Observed (fished)" = "#A6739B", "Predicted (24)" = "steelblue")) +  
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80", size = 0.5),
        panel.grid.minor = element_line(color = "gray90", size = 0.3))

# Save the figure
ggsave(paste0(figs, "/hist_mod24_v2.png"), hist_mod24, width = 13, height = 10, units = "cm")

# histogram of residuals for model 21 FOR ONLY MASKED DATA
# point1_fished$residuals_21 <- point1_fished$asinh_total_fishing - point1_fished$PredictedasinhFishing_21

hist_residuals_masked_21 <- ggplot(point1_fished, aes(x = residuals_21)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 0.2, fill = "gray59", color = "black", alpha = 0.5) +
  labs(title = "Residuals asinh full (21) on masked data", 
       x = "Residuals asinh(hrs/km²)", 
       y = "Density") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80", size = 0.5),
        panel.grid.minor = element_line(color = "gray90", size = 0.3))

ggsave(paste0(figs, "/hist_residuals_masked_21.png"), hist_residuals_masked_21, width = 12, height = 10, units = "cm")



# histogram of residuals for model 22
# point1_fished$residuals_22 <- point1_fished$asinh_total_fishing - point1_fished$PredictedasinhFishing_22

hist_residuals_22 <- ggplot(point1_fished, aes(x = residuals_22)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 0.2, fill = "gray59", color = "black", alpha = 0.5) +
  labs(title = "Residuals for asinh masked model (22)", 
       x = "Residuals asinh(hrs/km²)", 
       y = "Density") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80", size = 0.5),
        panel.grid.minor = element_line(color = "gray90", size = 0.3))

ggsave(paste0(figs, "/hist_residuals_22.png"), hist_residuals_22, width = 10, height = 10, units = "cm")



# histogram of residuals for model 24
# point1_fished$residuals_24 <- point1_fished$log10_total_fishing - point1_fished$PredictedLog10Fishing_24

hist_residuals_24 <- ggplot(point1_fished, aes(x = residuals_24)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 0.2, fill = "gray59", color = "black", alpha = 0.5) +
  labs(title = "Residuals for log10 model (24)", 
       x = "Residuals log10(hrs/km²)", 
       y = "Density") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80", size = 0.5),
        panel.grid.minor = element_line(color = "gray90", size = 0.3))

ggsave(paste0(figs, "/hist_residuals_24.png"), hist_residuals_24, width = 10, height = 10, units = "cm")







# Create combined histogram
hist_residuals_combined_21thru24 <- ggplot() +
  # Histogram for Model 21
  geom_histogram(data = point1_fished, aes(x = residuals_21, y = after_stat(density), fill = "asinh full model (21) on masked data only"), 
                 binwidth = 0.4, color = "black", alpha = 0.2, position = "identity") +
  
  # Histogram for Model 22
  geom_histogram(data = point1_fished, aes(x = residuals_22, y = after_stat(density), fill = "asinh masked model (22)"), 
                 binwidth = 0.4, color = "black", alpha = 0.2, position = "identity") +
  
  # Histogram for Model 24
  geom_histogram(data = point1_fished, aes(x = residuals_24, y = after_stat(density), fill = "log10 masked model (24)"),
                 binwidth = 0.4, color = "black", alpha = 0.2, position = "identity") +
  
  # Labels and custom legend
  labs(title = "Residuals; predictors and EEZ models", 
       x = "Residuals; asinh(hrs/km²) and log10(hrs/km²)", 
       y = "Density", 
       fill = "Model") +
  
  # Custom colors for each model
  scale_fill_manual(values = c("asinh full model (21) on masked data only" = "black", 
                               "asinh masked model (22)" = "steelblue", 
                               "log10 masked model (24)" = "darkorange")) +
  
  # Minimal theme
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80", size = 0.5),
        panel.grid.minor = element_line(color = "gray90", size = 0.3))

# Save the figure
ggsave(paste0(figs, "/hist_residuals_combined_21thru24.png"), hist_residuals_combined_21thru24, width = 18, height = 10, units = "cm")











rm(hist_combo, hist_mod21, hist_mod21_trunc, hist_mod22, hist_mod22_trunc,
   hist_mod24, logit_mod1_hist, logit_mod2_hist, logit_mod3_hist, logit_mod4_hist)
gc(full = TRUE)



#Histogram of residuals. 
asinh_total_fishing_hist <-ggplot(point1, aes(x = asinh_total_fishing, y = after_stat(count / sum(count) * 100))) +
  geom_histogram(binwidth = 0.2, fill = "gray59", color = "black") +
  labs(title = "Histogram of asinh(total fishing data)", x = "asinh(hrs/km^2)", y = "Percentage") +
  scale_x_continuous(limits = c(-.3, asinh_total_fishing_q[2]))+
  theme_minimal()+ 
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80", size = 0.5),  
        panel.grid.minor = element_line(color = "gray90", size = 0.3))


min_value <- min(point1$asinh_total_fishing[point1$asinh_total_fishing > 0], na.rm = TRUE)
min_value

#The same histogram of total fishing but truncated, not including the zeros
asinh_total_fishing_hist_trunc <-ggplot(point1, aes(x = asinh_total_fishing, y = after_stat(count / sum(count) * 100))) +
  geom_histogram(binwidth = 0.5, fill = "gray59", color = "black") +
  labs(title = "Histogram of asinh(total fishing data) truncated: no zeros", x = "asinh(hrs/km^2)", y = "Percentage") +
  scale_x_continuous(limits = c(0.0098, asinh_total_fishing_q[2]))+
  theme_minimal()+ 
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80", size = 0.5),  
        panel.grid.minor = element_line(color = "gray90", size = 0.3))


ggsave(paste0(figs,"/asinh_total_fishing_hist_trunc.png"),asinh_total_fishing_hist_trunc,width = 20,height = 10,units ="cm")


# #Histogram of residuals
# res_hist <- hist(point1$asinh_total_fishing - point1$PredictedasinhFishing_21, breaks = 100, xlim = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]))
# ggsave(paste0(figs,"/residuals_hist.png"),res_hist,width = 20,height = 10,units ="cm")
# 
# #The same histogram of residuals but truncated, not including the zeros
# res_hist_trunc <- hist(point1$asinh_total_fishing - point1$PredictedasinhFishing_21, breaks = 100, xlim = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]))
# ggsave(paste0(figs,"/residuals_hist_truncated.png"),res_hist_trunc,width = 20,height = 10,units ="cm")
# 


#Histogram of residuals not where the data was zero. 
point1_fished <- point1[point1$asinh_total_fishing != 0, ]

asinh_total_fishing_hist_no_zeros <-ggplot(point1_fished, aes(x = asinh_total_fishing, y = after_stat(count / sum(count) * 100))) +
  geom_histogram(binwidth = 0.5, fill = "gray59", color = "black") +
  labs(title = "Histogram of asinh(total fishing data) only fished data", x = "asinh(hrs/km^2)", y = "Percentage") +
  scale_x_continuous(limits = c(0, asinh_total_fishing_q[2]))+
  theme_minimal()+ 
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80", size = 0.5),  
        panel.grid.minor = element_line(color = "gray90", size = 0.3))

ggsave(paste0(figs,"/asinh_total_fishing_hist_no_zeros.png"),asinh_total_fishing_hist_no_zeros,width = 20,height = 10,units ="cm")


point1_fished$residuals <- point1_fished$asinh_total_fishing - point1_fished$PredictedasinhFishing_21
# Residuals
asinh_total_fishing_residuals_hist_no_zeros <-ggplot(point1_fished, aes(x = residuals, y = after_stat(count / sum(count) * 100))) +
  geom_histogram(binwidth = 0.5, fill = "gray59", color = "black") +
  labs(title = "Histogram of residuals asinh(total fishing data) only fished data", x = "asinh(hrs/km^2)", y = "Percentage") +
  scale_x_continuous(limits = c(0, asinh_total_fishing_q[2]))+
  theme_minimal()+ 
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80", size = 0.5),  
        panel.grid.minor = element_line(color = "gray90", size = 0.3))

ggsave(paste0(figs,"/asinh_total_fishing_residuals_hist_no_zeros.png"),asinh_total_fishing_hist_no_zeros,width = 20,height = 10,units ="cm")


rm(hist_residuals_22, hist_residuals_24, hist_residuals_combined_21thru24, hist_residuals_masked_21, asinh_total_fishing_hist, asinh_total_fishing_hist_no_zeros, asinh_total_fishing_hist_scaled, asinh_total_fishing_hist_trunc, asinh_total_fishing_residuals_hist_no_zeros)
gc(full=TRUE)
















# Predicted fishing effort maps for models 13, 15, 16, 18
custom_palette <- colorRampPalette(c("white", "darkblue", "darkred", "yellow"))
asinh_total_fishing_q <- as.numeric(quantile(point1$asinh_total_fishing, probs = c(.01,.99), na.rm = TRUE))

#### Predicted Fishing Effort model 13 2018-- 4 colors 
PredictedFishingEffortMap_mod13 <- ggplot() +
  xlim(-180,180) +
  ylim(-90,90) +
  geom_tile(data=subset(point1), aes(x=lon_id, y=lat_id, 
                                     color=(PredictedasinhFishing_13))) +
  scale_color_gradientn(colors=custom_palette(100), 
                        name ="Model 13: Predicted asinh of Total Fishing in 2018, asinh(hrs/100km^2)",
                        limits = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]),
                        oob = scales::squish, 
                        guide = guide_colorbar(barwidth = 10)) +  
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  labs(x="Longitude",y="Latitude") +
  theme(panel.background=element_rect(fill="#bfbfbf", colour="#bfbfbf")) +
  theme(legend.title.align = 0.5,
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.box.just = "center", plot.caption = element_text(size = 7, face = "italic"),legend.position = "bottom")

ggsave(paste0(figs,"/PredictedFishingEffort_mod13.png"),PredictedFishingEffortMap_mod13,width = 20,height = 10,units ="cm")

#### Predicted Fishing Effort model 15 2018-- 4 colors 
PredictedFishingEffortMap_mod15 <- ggplot() +
  xlim(-180,180) +
  ylim(-90,90) +
  geom_tile(data=subset(point1), aes(x=lon_id, y=lat_id, 
                                     color=(PredictedasinhFishing_15))) +
  scale_color_gradientn(colors=custom_palette(100), 
                        name ="Model 15: Predicted asinh of Total Fishing in 2018, asinh(hrs/100km^2)",
                        limits = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]),
                        oob = scales::squish, 
                        guide = guide_colorbar(barwidth = 10)) +  
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  labs(x="Longitude",y="Latitude") +
  theme(panel.background=element_rect(fill="#bfbfbf", colour="#bfbfbf")) +
  theme(legend.title.align = 0.5,
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.box.just = "center", plot.caption = element_text(size = 7, face = "italic"),legend.position = "bottom")

ggsave(paste0(figs,"/PredictedFishingEffort_mod15.png"),PredictedFishingEffortMap_mod15,width = 20,height = 10,units ="cm")

#### Predicted Fishing Effort model 16 2018-- 4 colors 
PredictedFishingEffortMap_mod16 <- ggplot() +
  xlim(-180,180) +
  ylim(-90,90) +
  geom_tile(data=subset(point1), aes(x=lon_id, y=lat_id, 
                                     color=(PredictedasinhFishing_16))) +
  scale_color_gradientn(colors=custom_palette(100), 
                        name ="Model 16: Predicted asinh of Total Fishing in 2018, asinh(hrs/100km^2)",
                        limits = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]),
                        oob = scales::squish, 
                        guide = guide_colorbar(barwidth = 10)) +  
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  labs(x="Longitude",y="Latitude") +
  theme(panel.background=element_rect(fill="#bfbfbf", colour="#bfbfbf")) +
  theme(legend.title.align = 0.5,
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.box.just = "center", plot.caption = element_text(size = 7, face = "italic"),legend.position = "bottom")

ggsave(paste0(figs,"/PredictedFishingEffort_mod16.png"),PredictedFishingEffortMap_mod16,width = 20,height = 10,units ="cm")

#### Predicted Fishing Effort model 18 2018 
PredictedFishingEffortMap_mod18 <- ggplot() +
  xlim(-180,180) +
  ylim(-90,90) +
  geom_tile(data=subset(point1), aes(x=lon_id, y=lat_id, 
                                     color=(PredictedasinhFishing_18))) +
  scale_color_gradientn(colors=custom_palette(100), 
                        name ="Model 18: Predicted asinh of Total Fishing in 2018, asinh(hrs/100km^2)",
                        limits = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]),
                        oob = scales::squish, 
                        guide = guide_colorbar(barwidth = 10)) +  
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  labs(x="Longitude",y="Latitude") +
  theme(panel.background=element_rect(fill="#bfbfbf", colour="#bfbfbf")) +
  theme(legend.title.align = 0.5,
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.box.just = "center", plot.caption = element_text(size = 7, face = "italic"),legend.position = "bottom")

ggsave(paste0(figs,"/PredictedFishingEffort_mod18.png"),PredictedFishingEffortMap_mod18,width = 20,height = 10,units ="cm")



#### Predicted Fishing Effort model 19 2018 
PredictedFishingEffortMap_mod19 <- ggplot() +
  xlim(-180,180) +
  ylim(-90,90) +
  geom_tile(data=subset(point1), aes(x=lon_id, y=lat_id, 
                                     color=(PredictedasinhFishing_19))) +
  scale_color_gradientn(colors=custom_palette(100), 
                        name ="Model 19: Predicted asinh of Total Fishing in 2018, asinh(hrs/100km^2)",
                        limits = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]),
                        oob = scales::squish, 
                        guide = guide_colorbar(barwidth = 10)) +  
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  labs(x="Longitude",y="Latitude") +
  theme(panel.background=element_rect(fill="#bfbfbf", colour="#bfbfbf")) +
  theme(legend.title.align = 0.5,
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.box.just = "center", plot.caption = element_text(size = 7, face = "italic"),legend.position = "bottom")

ggsave(paste0(figs,"/PredictedFishingEffort_mod19.png"),PredictedFishingEffortMap_mod19,width = 20,height = 10,units ="cm")



#### Predicted Fishing Effort model 20 2018 
PredictedFishingEffortMap_mod20 <- ggplot() +
  xlim(-180,180) +
  ylim(-90,90) +
  geom_tile(data=subset(point1), aes(x=lon_id, y=lat_id, 
                                     color=(PredictedasinhFishing_20))) +
  scale_color_gradientn(colors=custom_palette(100), 
                        name ="Model 20: Predicted asinh of Total Fishing in 2018, asinh(hrs/100km^2)",
                        limits = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]),
                        oob = scales::squish, 
                        guide = guide_colorbar(barwidth = 10)) +  
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  labs(x="Longitude",y="Latitude") +
  theme(panel.background=element_rect(fill="#bfbfbf", colour="#bfbfbf")) +
  theme(legend.title.align = 0.5,
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.box.just = "center", plot.caption = element_text(size = 7, face = "italic"),legend.position = "bottom")

ggsave(paste0(figs,"/PredictedFishingEffort_mod20.png"),PredictedFishingEffortMap_mod20,width = 20,height = 10,units ="cm")


#### Predicted Fishing Effort model 21 2018 
PredictedFishingEffortMap_mod21 <- ggplot() +
  xlim(-180,180) +
  ylim(-90,90) +
  geom_tile(data=subset(point1), aes(x=lon_id, y=lat_id, 
                                     color=(PredictedasinhFishing_21))) +
  scale_color_gradientn(colors=custom_palette(100), 
                        name ="Model 21: Predicted asinh of Total Fishing in 2018, asinh(hrs/100km^2)",
                        limits = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]),
                        oob = scales::squish, 
                        guide = guide_colorbar(barwidth = 10)) +  
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  labs(x="Longitude",y="Latitude") +
  theme(panel.background=element_rect(fill="#bfbfbf", colour="#bfbfbf")) +
  theme(legend.title.align = 0.5,
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.box.just = "center", plot.caption = element_text(size = 7, face = "italic"),legend.position = "bottom")

ggsave(paste0(figs,"/PredictedFishingEffort_mod21.png"),PredictedFishingEffortMap_mod21,width = 20,height = 10,units ="cm")


#### asinh Fishing Effort 2018 (NOT PREDICTED, THI IS THE ACTUAL EFFORT)
asinhFishingEffortMap <- ggplot() +
  xlim(-180,180) +
  ylim(-90,90) +
  geom_tile(data=subset(point1), aes(x=lon_id, y=lat_id, 
                                     color=(asinh_total_fishing))) +
  scale_color_gradientn(colors=custom_palette(100), 
                        name ="asinh of Total Fishing in 2018, asinh(hrs/100km^2)",
                        limits = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]),
                        oob = scales::squish, 
                        guide = guide_colorbar(barwidth = 10)) +  
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  labs(x="Longitude",y="Latitude") +
  theme(panel.background=element_rect(fill="#bfbfbf", colour="#bfbfbf")) +
  theme(legend.title.align = 0.5,
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.box.just = "center", plot.caption = element_text(size = 7, face = "italic"),legend.position = "bottom")

ggsave(paste0(figs,"/asinhTotalFishingMap.png"),asinhFishingEffortMap,width = 20,height = 10,units ="cm")





# Find percentages of "fished" and "non-fished"
fished_number <- sum(point1$fished)
non_fished_number <- nrow(point1) - sum(point1$fished)
fished_percentage <- fished_number/nrow(point1)*100
non_fished_percentage <- non_fished_number/nrow(point1)*100
fished_percentage
non_fished_percentage
# check: the fished_percentage aligns with the mean from the summary of fished row. 
summary(point1$fished)



### Maps and code for mapping were moved to fa_04_maps.R






point1 <- point1 %>% select(-c(PredictedFished_1_10percent, PredictedFished_1_30percent, FalseZeros_mod1_30percent, FalseZeros_mod1_10percent))  # Remove multiple columns
gc(full=TRUE)






# Create columns to make the possible false zeros map
# point1 <- point1 %>%
#   mutate(FalseZeros_mod2_50percent = factor(case_when(
#     fished == 1 ~ "observed fished",
#     fished == 0 & PredictedFished_2_50percent == 0 ~ "possible true zero",
#     fished == 0 & PredictedFished_2_50percent == 1 ~ "possible false zero",
#   ), levels = c("observed fished", "possible true zero", "possible false zero"))) # Set factor levels

point1 <- point1 %>%
  mutate(FalseZeros_mod2_25percent = factor(case_when(
    fished == 1 ~ "observed fished",
    fished == 0 & PredictedFished_2_25percent == 0 ~ "possible true zero",
    fished == 0 & PredictedFished_2_25percent == 1 ~ "possible false zero",
  ), levels = c("observed fished", "possible true zero", "possible false zero"))) # Set factor levels

# Make a column that is NA for all the false zeros at 25 percent, and otherwise has the same data as asinh fishing.  Use this to do the bam()
point1 <- point1 %>%
  mutate(asinh_total_fishing_25_percent = ifelse(FalseZeros_mod1_25percent == "possible false zero", NA, asinh_total_fishing))

# check
sum(is.na(point1$asinh_total_fishing_25_percent))
sum(point1$FalseZeros_mod1_25percent == "possible false zero")

# Make a column that is NA for all the false zeros at 50 percent, and otherwise has the same data as asinh fishing.  Use this to do the bam()
point1 <- point1 %>%
  mutate(asinh_total_fishing_50_percent = ifelse(FalseZeros_mod1_50percent == "possible false zero", NA, asinh_total_fishing))

# check
sum(is.na(point1$asinh_total_fishing_50_percent))
sum(point1$FalseZeros_mod1_50percent == "possible false zero")


# Make a column that is NA for all the false zeros at 75 percent, and otherwise has the same data as asinh fishing.  Use this to do the bam()
point1 <- point1 %>%
  mutate(asinh_total_fishing_75_percent = ifelse(FalseZeros_mod1_75percent == "possible false zero", NA, asinh_total_fishing))

# check
sum(is.na(point1$asinh_total_fishing_75_percent))
sum(point1$FalseZeros_mod1_75percent == "possible false zero")



point1 <- point1 %>%
  mutate(FalseZeros_mod2_75percent = factor(case_when(
    fished == 1 ~ "observed fished",
    fished == 0 & PredictedFished_2_75percent == 0 ~ "possible true zero",
    fished == 0 & PredictedFished_2_75percent == 1 ~ "possible false zero",
  ), levels = c("observed fished", "possible true zero", "possible false zero"))) # Set factor levels







#code to run the models is in fa_05_models.R




# code for pseudo R^2 is in fa_09_r_squared.R

# Create maps for models 25, 50 and 75 percent
custom_palette <- colorRampPalette(c("white", "darkblue", "darkred", "yellow"))
asinh_total_fishing_q <- as.numeric(quantile(point1$asinh_total_fishing, probs = c(.01,.99), na.rm = TRUE))



#### Map for Actual Fishing Effort masked 25 percent 2018
ActualFishingEffortCompletelyMasked <- ggplot() +
  xlim(-180,180) +
  ylim(-90,90) +
  geom_tile(data=subset(point1_fished), aes(x=lon_id, y=lat_id,
                                            color=(asinh_total_fishing))) +
  scale_color_gradientn(colors=custom_palette(100),
                        name ="asinh of Total Fishing in 2018 masked; no zeros, asinh(hrs/100km^2)",
                        limits = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]),
                        oob = scales::squish,
                        guide = guide_colorbar(barwidth = 10)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  labs(x="Longitude",y="Latitude") +
  theme(panel.background=element_rect(fill="#bfbfbf", colour="#bfbfbf")) +
  theme(legend.title.align = 0.5,
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.box.just = "center", plot.caption = element_text(size = 7, face = "italic"),legend.position = "bottom")

ggsave(paste0(figs,"/ActualFishingEffort_completely_masked.png"),ActualFishingEffortCompletelyMasked,width = 20,height = 11,units ="cm")

rm(ActualFishingEffortCompletelyMasked)



#### Map for Actual Fishing Effort masked 25 percent 2018
ActualFishingEffortmasked25percent <- ggplot() +
  xlim(-180,180) +
  ylim(-90,90) +
  geom_tile(data=subset(point1), aes(x=lon_id, y=lat_id,
                                     color=(asinh_total_fishing_25_percent))) +
  scale_color_gradientn(colors=custom_palette(100),
                        name ="asinh of Total Fishing in 2018 masked; no zeros more than 25% likely to be false zeros, asinh(hrs/100km^2)",
                        limits = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]),
                        oob = scales::squish,
                        guide = guide_colorbar(barwidth = 10)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  labs(x="Longitude",y="Latitude") +
  theme(panel.background=element_rect(fill="#bfbfbf", colour="#bfbfbf")) +
  theme(legend.title.align = 0.5,
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.box.just = "center", plot.caption = element_text(size = 7, face = "italic"),legend.position = "bottom")

ggsave(paste0(figs,"/ActualFishingEffort_masked25percent.png"),ActualFishingEffortmasked25percent,width = 20,height = 11,units ="cm")

rm(ActualFishingEffortmasked25percent)

#### Map for Actual Fishing Effort masked 50 percent 2018
ActualFishingEffortmasked50percent <- ggplot() +
  xlim(-180,180) +
  ylim(-90,90) +
  geom_tile(data=subset(point1), aes(x=lon_id, y=lat_id,
                                     color=(asinh_total_fishing_50_percent))) +
  scale_color_gradientn(colors=custom_palette(100),
                        name ="asinh of Total Fishing in 2018 masked; no zeros more than 50% likely to be false zeros, asinh(hrs/100km^2)",
                        limits = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]),
                        oob = scales::squish,
                        guide = guide_colorbar(barwidth = 10)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  labs(x="Longitude",y="Latitude") +
  theme(panel.background=element_rect(fill="#bfbfbf", colour="#bfbfbf")) +
  theme(legend.title.align = 0.5,
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.box.just = "center", plot.caption = element_text(size = 7, face = "italic"),legend.position = "bottom")

ggsave(paste0(figs,"/ActualFishingEffort_masked50percent.png"),ActualFishingEffortmasked50percent,width = 20,height = 11,units ="cm")

rm(ActualFishingEffortmasked50percent)

#### Map for Actual Fishing Effort masked 75 percent 2018
ActualFishingEffortmasked75percent <- ggplot() +
  xlim(-180,180) +
  ylim(-90,90) +
  geom_tile(data=subset(point1), aes(x=lon_id, y=lat_id,
                                     color=(asinh_total_fishing_75_percent))) +
  scale_color_gradientn(colors=custom_palette(100),
                        name ="asinh of Total Fishing in 2018 masked; no zeros more than 75% likely to be false zeros, asinh(hrs/100km^2)",
                        limits = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]),
                        oob = scales::squish,
                        guide = guide_colorbar(barwidth = 10)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  labs(x="Longitude",y="Latitude") +
  theme(panel.background=element_rect(fill="#bfbfbf", colour="#bfbfbf")) +
  theme(legend.title.align = 0.5,
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.box.just = "center", plot.caption = element_text(size = 7, face = "italic"),legend.position = "bottom")

ggsave(paste0(figs,"/ActualFishingEffort_masked75percent.png"),ActualFishingEffortmasked75percent,width = 20,height = 11,units ="cm")

rm(ActualFishingEffortmasked75percent)


# rm(pseudo_r_squared_fished_mod21, pseudo_r_squared_fished_mod22, pseudo_r_squared_fished_mod25percent, pseudo_r_squared_fished_mod50percent, pseudo_r_squared_fished_mod75percent)
# rm(fished_temporary, fished_nonfished_pred1, logitmod1, logitmod1_rss, logitmod2, logitmod2_rss, logitmod3, logitmod3_rss, logitmod4, logitmod4_rss)
# rm(summary_1, summary_2, summary_3, summary_4)
# rm(mod1_fished_predictions_60_q, mod1_fished_predictions_70_q)
# rm(custom_palette3)
# rm(null_deviance_fished, non_fished_percentage, non_fished_number)
# rm(fished_nonfished_pred_1)
# gc(full = TRUE)
# 


#### Map for Actual Fishing Effort masked 25 percent 2018
asinh_total_fishing <- ggplot() +
  xlim(-180,180) +
  ylim(-90,90) +
  geom_tile(data=subset(point1), aes(x=lon_id, y=lat_id,
                                     color=(asinh_total_fishing))) +
  scale_color_gradientn(colors=custom_palette(100),
                        name ="asinh of Total Fishing in 2018; asinh(hrs/100km^2)",
                        limits = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]),
                        oob = scales::squish,
                        guide = guide_colorbar(barwidth = 10)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  labs(x="Longitude",y="Latitude") +
  theme(panel.background=element_rect(fill="#bfbfbf", colour="#bfbfbf")) +
  theme(legend.title.align = 0.5,
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.box.just = "center", plot.caption = element_text(size = 7, face = "italic"),legend.position = "bottom")

ggsave(paste0(figs,"/asinh_total_fishing.png"),asinh_total_fishing,width = 20,height = 11,units ="cm")

rm(asinh_total_fishing)



gc(full = TRUE)





#### Map for Predicted Fishing Effort model 25 percent 2018 
PredictedFishingEffortMap_mod25percent <- ggplot() +
  xlim(-180,180) +
  ylim(-90,90) +
  geom_tile(data=subset(point1_25percent), aes(x=lon_id, y=lat_id, 
                                               color=(fitted25percent))) +
  scale_color_gradientn(colors=custom_palette(100), 
                        name ="Predicted asinh of Total Fishing in 2018 excluding zeros more than 25% likely to be false zeros, asinh(hrs/100km^2)",
                        limits = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]),
                        oob = scales::squish, 
                        guide = guide_colorbar(barwidth = 10)) +  
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  labs(x="Longitude",y="Latitude") +
  theme(panel.background=element_rect(fill="#bfbfbf", colour="#bfbfbf")) +
  theme(legend.title.align = 0.5,
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.box.just = "center", plot.caption = element_text(size = 7, face = "italic"),legend.position = "bottom")

ggsave(paste0(figs,"/PredictedFishingEffort_mod25percent.png"),PredictedFishingEffortMap_mod25percent,width = 20,height = 11,units ="cm")

rm(PredictedFishingEffortMap_mod25percent)

#### Map for Predicted Fishing Effort model 50 percent 2018 
PredictedFishingEffortMap_mod50percent <- ggplot() +
  xlim(-180,180) +
  ylim(-90,90) +
  geom_tile(data=subset(point1_50percent), aes(x=lon_id, y=lat_id, 
                                               color=(fitted50percent))) +
  scale_color_gradientn(colors=custom_palette(100), 
                        name ="Predicted asinh of Total Fishing in 2018 excluding zeros more than 50% likely to be false zeros, asinh(hrs/100km^2)",
                        limits = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]),
                        oob = scales::squish, 
                        guide = guide_colorbar(barwidth = 10)) +  
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  labs(x="Longitude",y="Latitude") +
  theme(panel.background=element_rect(fill="#bfbfbf", colour="#bfbfbf")) +
  theme(legend.title.align = 0.5,
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.box.just = "center", plot.caption = element_text(size = 7, face = "italic"),legend.position = "bottom")

ggsave(paste0(figs,"/PredictedFishingEffort_mod50percent.png"),PredictedFishingEffortMap_mod50percent,width = 20,height = 11,units ="cm")

rm(PredictedFishingEffortMap_mod50percent)

#### Map for Predicted Fishing Effort model 75 percent 2018 
PredictedFishingEffortMap_mod75percent <- ggplot() +
  xlim(-180,180) +
  ylim(-90,90) +
  geom_tile(data=subset(point1_75percent), aes(x=lon_id, y=lat_id, 
                                               color=(fitted75percent))) +
  scale_color_gradientn(colors=custom_palette(100), 
                        name ="Predicted asinh of Total Fishing in 2018 excluding zeros more than 75% likely to be false zeros, asinh(hrs/100km^2)",
                        limits = c(asinh_total_fishing_q[1], asinh_total_fishing_q[2]),
                        oob = scales::squish, 
                        guide = guide_colorbar(barwidth = 10)) +  
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  labs(x="Longitude",y="Latitude") +
  theme(panel.background=element_rect(fill="#bfbfbf", colour="#bfbfbf")) +
  theme(legend.title.align = 0.5,
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.box.just = "center", plot.caption = element_text(size = 7, face = "italic"),legend.position = "bottom")

ggsave(paste0(figs,"/PredictedFishingEffort_mod75percent.png"),PredictedFishingEffortMap_mod75percent,width = 20,height = 11,units ="cm")





rm(PredictedFishingEffortMap_mod75percent)
rm(point1_fished_temporary_25, point1_fished_temporary_50, point1_fished_temporary_75, point1_fished_temporary_21)
rm(hist_25_percent_model, hist_50_percent_model, hist_75_percent_model, hist_combo, hist_mod21, hist_mod22)
rm(point1_25percent, point1_75percent, point1_50percent)
rm(FishedMap, hist_asinh_fishing_data, hist_mod21, hist_mod22, point1_ocean)
gc(full = TRUE)


