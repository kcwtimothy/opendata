# ============================================================================ #
# HK Open Data ===============================================================
# fire stat  =================================================================
# ============================================================================ #


# 0.Configuration ==============================================================
source("utils.R")
library(tidyverse)
library(DT)


# 1.Fetch Data =================================================================
x <- search_tool(combined_table, 2:4, "fire", 1)

combined_table %>% filter(id %in% c(37,41,130,174,148)) %>% datatable()

# @ Classification of Fires  --------------------------------------------------- 
# stat: fire occur times
# dimension: fire classification
# date dimension: yrmth 
fire_1 <- fetch_tool(combined_table, 37) %>%  
  .[, -17]

# @ Classification of Fires by Causes ------------------------------------------
# stat: fire occur times
# dimension:fire cause
# date dimension: yrmth
fire_2 <- fetch_tool(combined_table, 41) %>%  
  .[, -15]

# @ Fire Service Indicator -----------------------------------------------------
# stat: fire occur times
# dimension:fire service
# date dimension: yrmth
fire_3 <- fetch_tool(combined_table, 130) 

# @ Occupancy where major fire occurred (No. 3 Alarm and above) ----------------
fire_4 <- fetch_tool(combined_table, 174)     

# @ Injuries and Fatalities ----------------------------------------------------
# stat: 
  # "Injuries and Fatalities" 
  # "No. of Rescue"           
  # "Injuries of public"     
  # "Fatalities of public"    
  # "Injuries of FS member"   
  # "Fatalities of FS member"
# dimension: na
# date dimension: yrmth
fire_5 <- fetch_tool(combined_table, 148)     


# 2.Data Clean and Combine =====================================================

# @ Classification of Fires  --------------------------------------------------- 
x1 <- get_historical(combined_table, 37, "2016-01-01", "2020-04-01")
fire_1_1 <- fetch_tool(x1, 1)
fire_1_2 <- fetch_tool(x1, 2)

# useless archived data 

c_fire_1 <- fire_1 %>% 
  gather(
    key = "dimension1_factor", 
    value = "stat_fire_occur_times", 
    - `Classification of Fires`
    ) %>%
  mutate(dimension1_name = "fire_classification") %>%
  rename(yrmth = `Classification of Fires`) %>%
  select(stat_fire_occur_times, yrmth, dimension1_name, dimension1_factor)

# @ Classification of Fires by Causes ------------------------------------------
c_fire_2 <- fire_2 %>%
  gather(
    key = "dimension1_factor", 
    value = "stat_fire_occur_times", 
    - 1) %>%
  mutate(dimension1_name = "fire_cause") %>%
  rename(yrmth = 1) %>%
  select(stat_fire_occur_times, yrmth, dimension1_name, dimension1_factor)


# 3. consolidate all ===========================================================
fire_accident_stat <- rbind(c_fire_1, c_fire_2)


# for the last 3 tables, to be continue later if need ...



# ****** end ****** ============================================================












