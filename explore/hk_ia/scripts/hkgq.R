library(dplyr)
library(stringr)
library(data.table)
library(tidyverse)

load("data/consolidated_data/hk_insurance_stat_general_quarterly.rda")

# stat_value ==============
# remove total value and set row number as unique key ===========================================
value_df <- hk_insurance_stat_general_quarterly %>% 
  mutate_all(tolower) %>%
  mutate_all(~ ifelse(str_detect(., "non-"), 
                      str_replace(., "non-", "non_"), . )) %>% 
  filter(!(stat_name %like% "total"),
         !(dimot_factor_1 %like% "total")) %>%
  mutate(row_id = as.numeric(rownames(.)),
         stat_value = as.numeric(stat_value),
         stat_value = ifelse(str_detect(stat_name, "[$]"), 
                             stat_value * 1000, stat_value))

# dimot_name_2 =================================================================================  
dimot2_df <- value_df %>%
  mutate(coverage_dimot2 = ifelse(grepl("coverage", dimot_name_2),
                           dimot_factor_2, NA)) %>%
  select(row_id, coverage_dimot2)

# dimot_name_1 =================================================================================
## buz_class
dimot1_df <- value_df %>%
  mutate(trade_occupation_dimot1 = ifelse(grepl("trade occupation", dimot_name_1),
                                          paste0("trade_occupation@", dimot_factor_1), NA),
         motor_vehicle_type_dimot1 = ifelse(grepl("type of motor vehicle", dimot_name_1),
                                     paste0("motor_vehicle_type@", dimot_factor_1), NA),
         insurer_name_dimot1 = ifelse(grepl("name of insurer", dimot_name_1),
                               paste0("insurer_name@", dimot_factor_1), NA),
         buz_class_sep = ifelse(grepl("class of business", dimot_name_1),
                                dimot_factor_1, NA),
         co = str_count(buz_class_sep, "[-]")) %>% 
  separate(buz_class_sep, c("a", "b", "c"), sep = "-") %>% 
  mutate(buz_class_dimot1 = ifelse(str_detect(a, "[,]"),
                              gsub("(.*)\\,.*", "\\1", a), a),
         buz_class_aspect_dimot1 = str_match(a, "damage & liability"),
         coverage_dimot1 = ifelse(co %in% c(1, 2), b, NA),
         liability_type_dimot1 = ifelse(co == 2, c, NA)) %>% 
  select(row_id, contains("_dimot1")) 


# stat_name ====================================================================================
## stat_name with dimot_name_name that is not insurer_name====
stat_1 <- value_df %>%
  filter(grepl("^class|^trade|^type", dimot_name_1)) %>% 
  mutate(stat_name_stat = ifelse(str_detect(stat_name, "[$]"),
                                 str_remove(stat_name, "-.*"), 
                                 stat_name)) %>% 
  select(row_id, stat_name_stat)

## stat_name with dimot_name_1 that is insurer_name=======
stat_co <- value_df %>%
  filter(grepl("name of insurer", dimot_name_1)) %>% 
  mutate(stat_name = ifelse(str_detect(stat_name, "[$]"),
                            str_remove(stat_name, "[-][(]hk[$][']000[)]"),
                            stat_name),
         co = str_count(stat_name, "[-]"),
         stat_name_sep = stat_name) %>% 
  separate(stat_name_sep, c("a", "b", "c", "d", "e"), sep = "-") 

df_2 <- stat_co %>% 
  select(a, b, c, d, e, co) %>% unique()

## 0 - ==================
stat_2 <- stat_co %>% 
  filter(grepl("0", co)) %>%
  mutate(stat_name_stat = stat_name) %>%
  select(row_id, stat_name_stat)

## 1 - ==================
stat_3 <- stat_co %>% 
  filter(grepl("1", co)) %>%
  mutate(stat_name_stat = b,
         buz_class_stat = a) %>%
  select(row_id, stat_name_stat, buz_class_stat)

## 4 - ==================
stat_4 <- stat_co %>% 
  filter(grepl("4", co)) %>%
  mutate(stat_name_stat = e,
         liability_type_stat = d,
         buz_type_stat = c,
         buz_class_aspect_stat = b,
         buz_class_stat = a) %>%
  select(row_id, contains("_stat"))

## 3 - ==================
stat_5 <- stat_co %>% 
  filter(grepl("3", co)) %>%
  mutate(stat_name_stat = d,
         buz_type_stat = c,
         buz_class_aspect_stat = b,
         buz_class_stat = a) %>%
  select(row_id, contains("_stat")) 

## 2 - ==================
stat_6 <- stat_co %>% 
  filter(grepl("2", co)) %>%
  mutate(stat_name_stat = c,
         buz_class_aspect_stat = str_match(b, "damage & liability"),
         buz_type_stat = ifelse(str_detect(b, "damage & liability"), NA, b),
         buz_class_stat = a) %>%
  select(row_id, contains("_stat"))  

## combine stat_name tables=================
stat_df <- bind_rows(stat_1, stat_2, stat_3,
                     stat_4, stat_5, stat_6) %>%
  arrange(row_id) 

df_1 <- value_df %>% 
  select(resource_name) %>% unique()

# resource_name ================================================================================
resource_df <- df %>%
  mutate(underwriting_type_resource = str_match(resource_name, 
                                                "direct & reinsurance inward|direct|reinsurance inward|reinsurance"),
         coverage_resource = str_match(resource_name, "employees' compensation"),
         buz_class_resource = str_match(resource_name, "motor vehicle"),
         buz_class_aspect_resource = str_match(resource_name, "damage & liability"),
         stat_name_resource = ifelse(grepl("insurers", resource_name), 
                                     gsub(".*net (.*) \\of insurers.*", "\\1", resource_name), NA)) %>%
  select(row_id, contains("_resource"))

# combine tables ==============================================================================
## join tables ===============
com_df <- list(value_df, dimot1_df, dimot2_df, stat_df, resource_df) %>%
  reduce(left_join, by = "row_id")

## clean joined tables =======
com_df_clean <- com_df %>% 
  mutate(buz_class_com = coalesce(buz_class_dimot1, buz_class_stat, buz_class_resource),
         buz_class_aspect_com = coalesce(buz_class_aspect_resource, 
                                         buz_class_aspect_stat,
                                         buz_class_aspect_dimot1),
         coverage = coalesce(coverage_dimot1, coverage_dimot2, coverage_resource),
         coverage = ifelse(!is.na(coverage), paste0("coverage@", coverage), NA),
         buz_type_stat = ifelse(!is.na(buz_type_stat), paste0("buz_type@", 
                                                              buz_type_stat), NA),
         coverage_buztype_com = coalesce(coverage, buz_type_stat),
         liability_type_com = coalesce(liability_type_dimot1, liability_type_stat),
         stat_name_com = ifelse(!is.na(stat_name_resource), 
                                paste(stat_name_stat, stat_name_resource),
                                stat_name_stat),
         insurer_trade_motor_com = coalesce(insurer_name_dimot1, 
                                        trade_occupation_dimot1,
                                        motor_vehicle_type_dimot1),
         underwriting_type_com = ifelse(!is.na(underwriting_type_resource),
                                        paste0("underwriting_type@",
                                               underwriting_type_resource), NA),
         buz_class_com = ifelse(!is.na(buz_class_com),
                                paste0("buz_class@", buz_class_com), NA),
         buz_class_aspect_com = ifelse(!is.na(buz_class_aspect_com),
                                       paste0("buz_class_aspect@",
                                              buz_class_aspect_com), NA),
         liability_type_com = ifelse(!is.na(liability_type_com), 
                                     paste0("liability_type@", 
                                            liability_type_com), NA),
         dimdt_name = "year",
         dimdt_factor = "2018") %>%
  arrange(row_id) %>% 
  select(ct_id, stat_value, contains(c("_com", "dimdt_")))

# rename column names ========================================================================== 
com_df_colsplit <- com_df_clean %>%   
  select(insurer_trade_motor_com, buz_class_com,
         buz_class_aspect_com, coverage_buztype_com,
         liability_type_com, underwriting_type_com) 

## a function for separating columns
sep <- function(col){
  sep_cols <- com_df_colsplit %>% 
    separate(col, into = c(paste0("dimot_name_", col),  
                           paste0("dimot_factor_", col)), 
             sep = "@") %>% 
    select(contains("dimot_"))
  return(sep_cols)
}

# final table ==================================================================================
hkgq <- cbind(com_df_clean[, c("ct_id", "stat_name_com", 
                               "stat_value", "dimdt_name", 
                               "dimdt_factor")], 
                      sep(1),sep(2), sep(3), sep(4), sep(5), sep(6)) %>% 
  rename(stat_name = stat_name_com) %>% 
  mutate(dimot_factor_2 = str_squish(dimot_factor_2),
         dimot_factor_3 = str_squish(dimot_factor_3))

# save data ====================================================================================
save(hkgq, file = "explore/hk_ia/data/hkgq.rda")

