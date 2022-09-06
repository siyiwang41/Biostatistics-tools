library(lubridate)
library(dplyr)
library(openxlsx)
library(readxl)


########### First to get the complete mobility data ########################

# Load mobility CSD and CT data
mobilityCSD1 <- read.csv("csduid_level_weekly_20210425_20210501.csv", fileEncoding="UTF-8-BOM")
mobilityCSD2 <- read.csv("csduid_level_weekly_20210912_20210918.csv", fileEncoding="UTF-8-BOM")
mobilityCSD <- unique(rbind(mobilityCSD1,mobilityCSD2))
mobilityCSD$wk_day_1 <- as.Date(mobilityCSD$wk_day_1, format="%Y-%m-%d")


mobilityCT1 <- read.csv("ctuid_level_weekly_20210425_20210501.csv", fileEncoding="UTF-8-BOM")
mobilityCT2 <- read.csv("ctuid_level_weekly_20210912_20210918.csv", fileEncoding="UTF-8-BOM")
mobilityCT <- unique(rbind(mobilityCT1,mobilityCT2))
mobilityCT$wk_day_1 <- as.Date(mobilityCT$wk_day_1, format="%Y-%m-%d")



########### Merge CSD and CT mobility to cover the whole Ontario #################
# Load link file between CSD and CT
csdct <- read_xlsx("CTs16_CSDs_v12a_04June2020_FINAL_wIRI.xlsx", sheet = 3)
length(unique(csdct$CT2016uid))


MergeMobility <- function(mobilityCSD,mobilityCT,csdct){
  
  # Only use the columns about ct and csd
  csdct <- unique(csdct[,c("CT2016uid","CSD2016uid")])
  # csdct has all csd and mobilityCSD only covers part of CSD
  CSDnames <- unique(mobilityCSD$CSDID)
  allCSDnames <- csdct$CSD2016uid
  # indicator TRUE means CSDs have been covered in mobilityCSD;
  # indicator FALSE means CSDs haven't been covered in mobilityCSD, should use mobilityCT
  csdct$indicator <-ifelse(allCSDnames %in% CSDnames, TRUE,FALSE)
  noCSD <- unique(subset(csdct,indicator==FALSE)) # 99 CSD not included in mobilityCSD
  # unique(noCSD$CSD2016uid)
  
  
  mobilityCT$CTID <- as.character(mobilityCT$CTID)
  # indicator TRUE means non-covered CSD can be use CT (in mobilityCT) instead;
  # indicator FALSE means non-covered CSD cannot be use CT (in mobilityCT) instead
  noCSD$indicator <-ifelse(noCSD$CT2016uid %in% mobilityCT$CTID, TRUE,FALSE)
  # addCT is the uid of CT can be found in mobilityCT to make up non-covered CSD
  addCT <- subset(noCSD,indicator==TRUE)[,1]
  # We only use a little part of mobilityCT here
  mobilityCT <- merge( mobilityCT,addCT, by.x = "CTID", by.y="CT2016uid")
  
  
  # Combine two mobility data sets
  # Change all the names (either CSD (character) or CT (numeric)) to character
  mobilityCSD$name <- as.character(mobilityCSD$name)
  mobilityCT$name <- as.character(mobilityCT$name)
  # Correct colunames CSDID & CTID to UID (so that two data set can be combined)
  names(mobilityCSD)[names(mobilityCSD)=="CSDID"] <- "UID"
  names(mobilityCT)[names(mobilityCT)=="CTID"] <- "UID"
  mobility <- rbind(mobilityCSD, mobilityCT)
  
  # # Merge link file and mobility data
  # link <- unique(linkphu[,c("UID","HUID")])
  # mphu <- merge(mobility, link, by="UID")
  return(mobility)
  
}


# Call the function to get the mobility with both CT and CSD level but can cover the whole ON
mobility <- arrange(MergeMobility(mobilityCSD,mobilityCT,csdct), by=wk_day_1)

# Unique CSD/CT in BlueDot data, most are CSD level mobility
# 516
length(unique(mobility$UID))

# Replace extreme values 0 and 100 to 1 and 99 for percent_stay and prop_at_home
# mobility$percent_stay <- ifelse(mobility$percent_stay==0, 1, mobility$percent_stay) 
mobility$percent_stay <- ifelse(mobility$percent_stay==100, 99, mobility$percent_stay) 
mobility$prop_at_home <- ifelse(mobility$prop_at_home==100, 99, mobility$prop_at_home) 


# Save the data set
write.csv(mobility, file="MobilityCSDCT_ON.csv", row.names = F, quote = F, na = "NA")

# FSA<DA<CT<CSD<PHU<ON
################# Aggregated mobility for whole Ontario with 2019 baseline and 2020 baseline #######################

# First generate some columns for 2020 Feb baseline
Feb=subset(mobility, wk_day_1 %in% as.Date(c("2020-02-02","2020-02-09")))
baseline=Feb %>%
  group_by(UID) %>%
  summarise(base_avg_num_devices = sum(avg_num_devices),
            base_sum_num_devices =  sum(base_2019_sum_num_devices))

# Add baseline columns to mobility data
# Give up adding base_avg_num_devices and base_sum_num_devices,
# There are too many missing data
mobility = merge(mobility,baseline, by="UID")


# Aggregated mobility for whole ON
ONmobility <- mobility %>%
  group_by(wk_day_1,year,w_o_y,base_2019_year,base_2019_w_o_y) %>%
  na.omit()%>%
  summarise(
    
    Weighted_percent_stay = sum(percent_stay*avg_num_devices)/sum(avg_num_devices),
    Weighted_prop_at_home = sum(prop_at_home*avg_num_devices)/sum(avg_num_devices),
    
    avg_num_devices = sum(avg_num_devices),
    sum_num_devices = sum(sum_num_devices),
    population = sum(population),
    
    
    # 2019 baseline
    Weighted_base2019_percent_stay = 
      sum(base_2019_percent_stay*base_2019_avg_num_devices)/sum(base_2019_avg_num_devices),
    Weighted_base2019_prop_at_home = 
      sum(base_2019_prop_at_home*base_2019_avg_num_devices)/sum(base_2019_avg_num_devices),
    base2019_percent_change_percent_stay = 
      100*(Weighted_percent_stay-Weighted_base2019_percent_stay)/Weighted_base2019_percent_stay,
    base2019_percent_change_prop_at_home = 
      100*(Weighted_prop_at_home-Weighted_base2019_prop_at_home)/Weighted_base2019_prop_at_home,
    
    base_2019_avg_num_devices = sum(base_2019_avg_num_devices),
    base_2019_sum_num_devices =  sum(base_2019_sum_num_devices),

    
    # 2020 Feb2-15 baseline
    # Note that we don't have base_avg_num_devices and sum_avg_num_devices
    # So we just simple average them without weights
    base_percent_stay = mean(base_percent_stay),
    base_prop_at_home = mean(base_prop_at_home),
    base_percent_change_percent_stay = mean(base_percent_change_percent_stay),
    base_percent_change_prop_at_home = mean(base_percent_change_prop_at_home))





# Save the data set
write.csv(ONmobility , file="AggregatedMobility_ON.csv", row.names = F, quote = F, na = "NA")




# Aggregate mobility by SDOH quintile or larger area-level
# If aggregate for larger area-level, QuintileCol could be the variable for the larger area level

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

