library(tidyverse)
library(ggplot2)
library(lubridate)
library(lme4)

setwd("C:/Users/wangs/OneDrive/Documents/GitHub/SDOH-Mobility/Rfiles")
income_occ_CT <- read.csv("../Data/SDOHranking_CT.csv")
mobilitySDOH_CT <- read.csv("../Data/mobilitySDOH_CT.csv")

# check number of CTID
length(unique(mobilitySDOH_CT$CTID))
###################################################
# Aggregate mobility by SDOH quintile

MobilityCT_Quintile <- function(mobilitySDOH_CT, QuintileCol){
  QuintileCol <- as.name(QuintileCol)
  
  # Clean data
  mobilitySDOH_CT <- mobilitySDOH_CT[!is.na(mobilitySDOH_CT$base_2019_percent_stay),]
  
  # Aggregate mobility into quintile level
  mobilityQ <- mobilitySDOH_CT %>% 
    group_by(!!QuintileCol,wk_day_1) %>% 
    summarise(
      w_o_y = unique(w_o_y),
      week = unique(week),
      
      Weighted_percent_stay = sum(percent_stay*avg_num_devices)/sum(avg_num_devices),
      Weighted_base2019_percent_stay = sum(base_2019_percent_stay*base_2019_avg_num_devices)/sum(base_2019_avg_num_devices),
      # base2019_percent_change_percent_stay = mean(base2019_percent_change_percent_stay),
      base2019_percent_change_percent_stay = 100*(Weighted_percent_stay-Weighted_base2019_percent_stay)/Weighted_base2019_percent_stay,
      
      Weighted_prop_at_home = sum(prop_at_home*avg_num_devices)/sum(avg_num_devices),
      Weighted_base2019_prop_at_home = sum(base_2019_prop_at_home*base_2019_avg_num_devices)/sum(base_2019_avg_num_devices),
      # base2019_percent_change_prop_at_home = mean(base2019_percent_change_prop_at_home),
      base2019_percent_change_prop_at_home = 100*(Weighted_prop_at_home - Weighted_base2019_prop_at_home)/Weighted_base2019_prop_at_home,
      
      avg_num_devices = sum(avg_num_devices),
      sum_num_devices = sum(sum_num_devices),
      population = sum(CT_pop))
}



# Call the function
mobilityCT_PHU <- MobilityCT_Quintile(mobilitySDOH_CT, QuintileCol="PHU_ID")
mobilityCT_ATIPPEQ <- MobilityCT_Quintile(mobilitySDOH_CT, QuintileCol="ATIPPE_quintile")
mobilityCT_EssentialServiceQ <- MobilityCT_Quintile(mobilitySDOH_CT, QuintileCol="employ_sales.trades.manufacturing.agriculture_quintile")


# Call the function with Tor and Pee only
mobilityCT_TorPee <- subset(mobilitySDOH_CT, PHU_ID %in% c("3895","2253"))
mobilityCT_ATIPPEQ_TorPee <-  MobilityCT_Quintile(mobilityCT_TorPee, QuintileCol="ATIPPE_quintile")
mobilityCT_EssentialServiceQ_TorPee <- MobilityCT_Quintile(mobilityCT_TorPee, QuintileCol="employ_sales.trades.manufacturing.agriculture_quintile")


# Save the data set
write.csv(mobilityCT_PHU, file="../Data/AggregatedMobility/mobilityCT_PHU.csv", row.names = F, quote = F, na = "NA")
write.csv(mobilityCT_ATIPPEQ, file="../Data/AggregatedMobility/mobilityCT_ATIPPEQ.csv", row.names = F, quote = F, na = "NA")
write.csv(mobilityCT_EssentialServiceQ, file="../Data/AggregatedMobility/mobilityCT_EssentialServiceQ.csv", row.names = F, quote = F, na = "NA")
write.csv(mobilityCT_ATIPPEQ_TorPee, file="../Data/AggregatedMobility/mobilityCT_ATIPPEQ_TorPee.csv", row.names = F, quote = F, na = "NA")
