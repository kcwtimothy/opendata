library(stringr)
library(tidyverse)
library(data.table)

load("data/consolidated_data/hk_insurance_stat_general_annual.rda")

# stat_value ===================================================================================
value_df <- hk_insurance_stat_general_annual %>%
  mutate_all(tolower) %>%
  mutate_all(~ ifelse(str_detect(., "non-"), 
                      str_replace(., "non-", "non_"), . )) %>% 
  filter(!(stat_name %like% "total"),
         !(dimot_factor_1 %like% "total"),
         !(stat_name %like% "overall"),
         !(dimot_factor_1 %like% "overall"),
         !str_detect(stat_name, "increase")) %>%
  mutate(stat_value = ifelse(grepl("[()]", stat_value), 
                             str_replace(stat_value, "[(]", "-"),
                             stat_value),
         stat_value = str_remove(stat_value, "[)]"),
         stat_value = as.numeric(stat_value),
         stat_value = ifelse(str_detect(stat_name, "[$]"),
                             ifelse(str_detect(stat_name, "[$]m"), 
                                    stat_value * 1000000, stat_value * 1000),
                             stat_value),
         stat_value = ifelse(str_detect(dimot_factor_1, "[$]"),
                             stat_value * 1000000, stat_value),
         stat_value = ifelse(str_detect(dimot_factor_1, "[%]"),
                             stat_value / 100, stat_value),
         stat_value = ifelse(str_detect(stat_name, "[%]"),
                             stat_value / 100, stat_value), 
         row_id = 1:nrow(.))

# dimot_1 ======================================================================================
dimot1_df <- value_df %>% 
  mutate(buz_class_sep = ifelse(grepl("class of business", dimot_name_1),
                              ifelse(str_detect(dimot_factor_1, "\\d"),
                                     stat_name, dimot_factor_1), NA),
         buz_class_sep = str_remove(buz_class_sep, "[-][$]m"),
         insurer_name_dimot1 = ifelse(grepl("insurer", dimot_name_1),
                                 dimot_factor_1, NA),
         stat_name_dimot1 = ifelse(grepl("class of business", dimot_name_1),
                                   ifelse(str_detect(dimot_factor_1, "\\d"),
                                          sub("(.*)\\-2.*", "\\1", dimot_factor_1), stat_name), NA)) %>%
  separate(buz_class_sep, 
           c("buz_class_dimot1", "is_statutory_buz_dimot1", "coverage_dimot1"),
           sep = "-") %>%
  select(row_id, contains("_dimot1"))

# dimdt=========================================================================================
dimdt_df <- value_df %>%
  mutate(special_year_dimdt = ifelse(grepl("accident year", dimot_name_1),
                               paste0("accident_year@", stat_name), NA),
         special_year_dimdt = ifelse(grepl("underwriting year", dimot_name_1),
                               paste0("underwriting_year@", stat_name), special_year_dimdt),
         dimdt_factor_dimdt = ifelse(grepl("accident year|underwriting year", dimot_name_1),
                                            str_match(dimot_factor_1, "[0-9]{4}"), 
                                            NA),
         dimdt_name_dimdt = ifelse(grepl("accident year|underwriting year", dimot_name_1),
                                   "year of development", NA),
         stat_name_dimdt = ifelse(grepl("accident year|underwriting year", dimot_name_1),
                                  gsub("(.*)\\-[0-9]{4}.*", "\\1", dimot_factor_1), NA)) %>%
  select(row_id, contains("_dimdt")) 

# stat_name ===================================================================================
stat <- value_df %>% 
  filter(!grepl("^prior|[0-9]{4}", stat_name)) %>% 
  mutate(stat_name = str_remove(stat_name, "[-][(][$][']000[)]"),
         stat_sep = stat_name) %>% 
  separate(stat_sep, c("a", "b", "c", "d"), sep = "-") 

stat_1 <- stat %>%
  filter(grepl("vehicle business$|vessels business$", a)) %>%
  mutate(motor_vehicle_type_stat = ifelse(!is.na(c),
                                     paste0("motor_vehicle_type@", c),
                                     NA),
         buz_class_stat = a,
         coverage_stat = str_match(b, "under.*policies"),
         policy_status_stat = str_match(b, "in force"),
         stat_name_stat = str_remove(b, "under.*|in force"),
         stat_name_stat = str_replace(stat_name_stat,
                                      "number of policies.*",
                                      "number of policies")) %>%
  select(row_id, contains("_stat")) 

stat_2 <- stat %>%
  filter(grepl("general liability", a)) %>%
  mutate(buz_class_stat = a,
         stat_name_stat = case_when(!is.na(d) ~ d,
                                    grepl("^other", b) ~ c,
                                    !grepl("^other|^statutory", b) ~ b),
         is_statutory_buz_stat = str_match(b, "other|statutory"),
         coverage_stat = ifelse(grepl("^statutory", b), c, NA)) %>% 
  select(row_id, contains("_stat"))

stat_3 <- stat %>%
  filter(!grepl("vehicle business$|vessels business$|general liability", a))%>%
  mutate(a = ifelse(grepl("provision$", a), paste(a, b, sep = "-"), a),
         stat_name_stat = ifelse(str_detect(a, "accident|motor|aircraft|ships|goods|property|pecuniary|proportional|employees"), 
                                 b, a),
         stat_name_stat = ifelse(grepl("statutory|others", stat_name_stat), NA, stat_name_stat),
         buz_class_stat = ifelse(str_detect(a, "accident|motor|aircraft|ships|goods|property|pecuniary|proportional"), 
                                 a, NA),
         coverage_stat = str_match(a, "employee.*"),
         is_statutory_buz_stat = str_match(b, "other|statutory")) %>% 
  select(row_id, contains("_stat"))

stat_df <- bind_rows(stat_1, stat_2, stat_3) %>% arrange(row_id)

# resource_name ================================================================================
resource_df <- value_df %>% 
  mutate(resource_name = gsub("(.*) \\(english)", "\\1", resource_name),
         resource_name = sub("market statistics - |.*insurers' statistics - (.*)",
                             "\\1", resource_name),
         file_name_resource = paste0("file_name@", word(resource_name, 1)),
         underwriting_type_resource = 
           str_match(resource_name, 
                     "direct & reinsurance inward|direct|reinsurance inward|reinsurance"),
         insurer_type_resource = ifelse(str_detect(resource_name, "pure reinsurers"),
                                        paste0("insurer_type@", 
                                               str_match(resource_name, 
                                                         "pure reinsurers")), NA),
         resource_sep = resource_name) %>% 
  separate(resource_sep, c("a", "b", "c", "d"), sep = "-") %>% 
  mutate(buz_class_resource = ifelse(str_detect(b, "basis[)]")&!str_detect(b, "employee"), 
                                     b, NA),
         coverage_resource = str_match(b, "employees' compensation"),
         buz_class_resource = str_remove(buz_class_resource, 
                                         "\\(.*\\)|direct.*|reinsurance.*"),
         stat_name_buzclass = ifelse(str_detect(b, "basis[)]"), c, NA),
         stat_name_a = case_when(str_detect(a, "retention|premiums|claims|provision|reserves|commission") ~
                                   str_remove(a, word(a, 1)),
                                 str_detect(b, "retention|premiums|claims|provision|reserves|commission") ~ b),
         stat_name_resource = case_when(!is.na(stat_name_buzclass) ~ stat_name_buzclass,
                                        !is.na(stat_name_a) ~ stat_name_a)) %>% 
  select(row_id, contains("_resource"))

# combine and clean tables =====================================================================
## combine tables
com_df <- list(value_df, dimot1_df, dimdt_df, stat_df, resource_df) %>% 
  reduce(left_join, by = "row_id") %>%
  arrange(row_id)

## clean combine tables
com_df_clean <- com_df %>% 
  mutate(buz_class_com = case_when(!is.na(buz_class_dimot1) ~ buz_class_dimot1,
                                   !is.na(buz_class_stat) ~ buz_class_stat,
                                   !is.na(buz_class_resource) ~ buz_class_resource),
         buz_class_com = ifelse(!is.na(buz_class_com), paste0("buz_class@", buz_class_com), NA),
         coverage_com = case_when(!is.na(coverage_dimot1) ~ coverage_dimot1,
                                  !is.na(coverage_stat) ~ coverage_stat,
                                  !is.na(coverage_resource) ~ coverage_resource),
         coverage_com = ifelse(grepl("under", coverage_com),
                               str_match(coverage_com, "third party|comprehensive"),
                               coverage_com),
         coverage_com = ifelse(!is.na(coverage_com), paste0("coverage_com@", coverage_com), NA),
         is_statutory_buz_com = case_when(!is.na(is_statutory_buz_dimot1) ~ 
                                            is_statutory_buz_dimot1,
                                          !is.na(is_statutory_buz_stat) ~
                                            is_statutory_buz_stat),
         is_statutory_buz_com = ifelse(!is.na(is_statutory_buz_com), 
                                       paste0("is_statutory_buz@", is_statutory_buz_com), NA),
         reserves_type_com = str_match(stat_name_resource, "technical.*"),
         reserves_type_com = ifelse(!is.na(reserves_type_com), 
                                    paste0("reserves_type@", reserves_type_com), NA),
         stat_name_stat = ifelse(stat_name_stat %in% c("gross","net"),
                                 paste(stat_name_stat, 
                                       str_remove(stat_name_resource, ".*net ")), 
                                 stat_name_stat),
         stat_name_com = case_when(!is.na(stat_name_dimot1) ~ stat_name_dimot1,
                                   !is.na(stat_name_dimdt) ~ stat_name_dimdt,
                                   !is.na(stat_name_stat) ~ stat_name_stat),
         dimdt_factor_statcom = str_match(stat_name_com, "201.*"),
         stat_name_com = str_remove(stat_name_com, 
                                    ' at the end of.*| in each year.*| ["]ep["]'),
         stat_name_com = str_remove(stat_name_com, "[-][$]m"),
         stat_name_com = str_replace(stat_name_com, "provision-claims recoverable", 
                                     "provision for claims recoverable"),
         stat_name_com = str_replace(stat_name_com, "provision-provision for ibnr",
                                     "provision for ibnr"),
         stat_name_com = str_remove(stat_name_com, ".*premiums-|.*risks-|.*provision-"),
         dimdt_name_com = ifelse(!is.na(dimdt_name_dimdt), dimdt_name_dimdt, dimdt_name),
         dimdt_factor_com = case_when(!is.na(dimdt_factor_statcom) ~ dimdt_factor_statcom,
                                      !is.na(dimdt_factor_dimdt) ~ dimdt_factor_dimdt,
                                      TRUE ~ dimdt_factor),
         insurer_name_dimot1 = ifelse(!is.na(insurer_name_dimot1),
                                      paste0("insurer_name@", insurer_name_dimot1), NA),
         insurer_motor_year_com = coalesce(insurer_name_dimot1, insurer_type_resource,
                                       motor_vehicle_type_stat, special_year_dimdt),
         underwriting_policy_com = case_when(!is.na(underwriting_type_resource) ~
                                               paste0("underwriting_type@", 
                                               underwriting_type_resource), 
                                             !is.na(policy_status_stat) ~
                                               paste0("policy_status@", policy_status_stat)),
         file_name_com = file_name_resource) %>% 
  arrange(row_id) %>% 
  mutate_all(~ ifelse(str_detect(., "business"), 
                      str_remove(., "business"), . )) %>% 
  select(ct_id, stat_value, contains("_com")) 

# rename column names ========================================================================== 
## reorder columns
com_df_colsplit <- com_df_clean %>%   
  select(file_name_com, underwriting_policy_com, insurer_motor_year_com,
         buz_class_com, is_statutory_buz_com, coverage_com)

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
hkga <- cbind(com_df_clean[, c("ct_id", "stat_name_com", "stat_value", 
                               "dimdt_name_com", "dimdt_factor_com")], 
              sep(1),sep(2), sep(3), sep(4), sep(5), sep(6)) %>% 
  rename(stat_name = stat_name_com,
         dimdt_name = dimdt_name_com,
         dimdt_factor = dimdt_factor_com) %>% 
  mutate(dimot_factor_6 = str_replace(dimot_factor_6, "owner.*",
                                      "owners' corporation liability"),
         dimot_factor_6 = str_squish(dimot_factor_6),
         dimot_factor_4 = str_squish(dimot_factor_4),
         dimot_factor_5 = ifelse(dimot_factor_5 == "other", "others", dimot_factor_5),
         dimot_factor_5 = str_squish(dimot_factor_5))

# save table ===================================================================================
save(hkga, file = "explore/hk_ia/data/hkga.rda")
