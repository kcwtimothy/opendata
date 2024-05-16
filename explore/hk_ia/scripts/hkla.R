library(stringr)
library(data.table)
library(tm)
library(tidyverse)

load("data/consolidated_data/hk_insurance_stat_longterm_annual.rda")

# stat_value ===================================================================================
value_df <- hk_insurance_stat_longterm_annual %>% 
  mutate_all(tolower) %>% 
  filter(!(dimot_factor_1 %like% "total"),
         !(stat_name %like% "total")) %>%
  mutate(stat_value = as.numeric(stat_value),
         stat_value = ifelse(str_detect(stat_name, "\\$"), 
                             stat_value * ifelse(grepl("m$", stat_name), 1000000, 1000), 
                             stat_value),
         stat_value = ifelse(str_detect(stat_name, "\\%"),
                             stat_value / 100,
                             stat_value),
         stat_value = ifelse(str_detect(dimot_factor_1, "\\$"),
                             stat_value * 1000000,
                             stat_value),
         dimot_factor_1 = str_replace(dimot_factor_1, "non-", "non_"),
         stat_name = str_replace(stat_name, "non-", "non_"),
         resource_name = str_replace(resource_name, "non-", "non_"),
         row_id = as.numeric(rownames(.)))

# dimot_1 ======================================================================================
dimot1_df <- value_df %>% 
  mutate(buz_class_dimot1 = ifelse(grepl("class", dimot_name_1),
                                   paste0("buz_class@", 
                                          gsub(".*class (.*)", "\\1", dimot_factor_1)),
                                   NA),
         investment_type_dimot1 = str_match(dimot_factor_1, "linked|non_linked"),
         insurance_type_dimot1 = case_when(str_detect(dimot_factor_1, "linked") ~ 
                                             str_remove(dimot_factor_1, ".*linked:-|.*linked:"), 
                                           grepl("type of insurance", dimot_name_1) ~ 
                                             dimot_factor_1),
         insurance_type_dimot1 = ifelse(str_detect(insurance_type_dimot1, "[0-9]{4}"), 
                                        NA, insurance_type_dimot1),
         stat_name_dimot1 = ifelse(str_detect(dimot_factor_1, "[$]|policies[-]"), 
                                   gsub("(.*)\\-2.*", "\\1", dimot_factor_1), NA),
         insurer_name_dimot1 = ifelse(grepl("name of insurer", dimot_name_1),
                                      paste0("insurer_name@", dimot_factor_1), NA)) %>% 
  mutate_all(list(~na_if(.,""))) %>% 
  select(row_id, stat_name_dimot1, buz_class_dimot1, investment_type_dimot1, 
         insurance_type_dimot1, insurer_name_dimot1) 

# resource_name ================================================================================
resource_df <- value_df %>% 
  mutate(file_name_resource = paste0("file_name@", word(resource_name, 1)),
         resource_name = str_replace(resource_name, "in-", "in_"),
         resource_name = str_remove(resource_name, word(resource_name, 1)),
         resource_name = str_remove(resource_name, ".*statistics"),
         investment_type_resource = str_match(resource_name, "linked|non_linked"),
         insured_type_resource = str_match(resource_name, "individual|group"),
         policy_status_resource = str_match(resource_name, "new|in_force"),
         resource_sep = resource_name) %>% 
  separate(resource_sep, c("a", "b"), sep = "-") %>%
  mutate(c = removeWords(a, unique(c(investment_type_resource, 
                                  insured_type_resource, 
                                  policy_status_resource,
                                  "business", "(english)", "total"))),
         buz_type_resource = case_when(str_detect(c, "annuity|retirement") ~
                                          gsub("[[:punct:]]*", "", str_sub(c, start = 2)),
                                        str_detect(c, "life") ~ "life"),
         buz_type_resource = str_squish(buz_type_resource),
         d = removeWords(resource_name, unique(c(investment_type_resource, 
                                     insured_type_resource, 
                                     policy_status_resource,
                                     buz_type_resource,
                                     "business", "(english)", "total"))),
        stat_name_resource = gsub("[[:punct:]]|voluntary.*", "", d),
        stat_name_resource = str_sub(stat_name_resource, 2),
        stat_name_resource = str_squish(stat_name_resource)) %>% 
  mutate_all(list(~na_if(.,""))) %>% 
  select(row_id, contains("_resource"))

# stat_name ====================================================================================
stat_df <- value_df %>% 
  mutate(stat_name = str_replace(stat_name, "-profits", "_profits"),
         stat_name = str_replace(stat_name, "rate-", "rate_"),
         stat_name = str_replace(stat_name, "no. of", "number of"),
         stat_sep = stat_name,
         profit_type_stat = str_match(stat_name, "with_profits|without_profits"),
         insured_type_stat = str_match(stat_name, "individual|group"),
         investment_type_stat = str_match(stat_name, "linked|non_linked")) %>% 
  separate(stat_sep, c("a", "b", "c", "d"), sep = "-") %>% 
  mutate(payment_method_stat = str_match(b, ".*payment"),
         payment_method_stat = ifelse(is.na(payment_method_stat),
                                      str_match(stat_name, "single|annual"), 
                                      payment_method_stat),
         buz_type_stat = case_when(str_detect(a, "annuity") ~ "annuity",
                                   grepl("others:", a) ~ b),
         stat_name_stat = removeWords(a, unique(c(profit_type_stat, 
                                                  insured_type_stat, 
                                                  investment_type_stat, 
                                                  payment_method_stat,
                                                  buz_type_stat,
                                                  "[0-9]{4}", "[:]",
                                                  "others"))),
         stat_name_stat = str_remove(stat_name_stat,
                                     "breakdown of |_2.*|[:]|[(][])]")) %>% 
  mutate_all(list(~na_if(.,""))) %>% 
  select(row_id, stat_name_stat, profit_type_stat, 
         insured_type_stat, investment_type_stat, payment_method_stat,
         buz_type_stat)

# combine tables ===============================================================================
com_df <- list(value_df, dimot1_df, resource_df, stat_df) %>% 
  reduce(left_join, by = "row_id")

com_df_clean <- com_df %>% 
  mutate(buz_type = ifelse(!is.na(buz_type_stat), 
                           buz_type_stat, buz_type_resource),
         insured_type = ifelse(!is.na(insured_type_stat), 
                               insured_type_stat, insured_type_resource),
         investment_type = case_when(!is.na(investment_type_dimot1) ~
                                       investment_type_dimot1,
                                     !is.na(investment_type_resource) ~ 
                                       investment_type_resource,
                                     !is.na(investment_type_stat) ~
                                       investment_type_stat),
         profit_insurer_class = case_when(!is.na(profit_type_stat) ~
                                      paste0("profit_type@", profit_type_stat), 
                                    !is.na(insurer_name_dimot1) ~
                                      insurer_name_dimot1,
                                    !is.na(buz_class_dimot1) ~
                                      buz_class_dimot1),
         stat_name_com = case_when(!is.na(stat_name_dimot1) ~ stat_name_dimot1,
                                   !is.na(stat_name_stat) ~ stat_name_stat, 
                                   !is.na(stat_name_resource) ~ stat_name_resource),
         stat_name_com = str_squish(stat_name_com),
         payment_method_stat = str_remove(payment_method_stat, " payment")) %>% 
  mutate_all(list(~na_if(.,""))) %>% 
  arrange(row_id) %>% 
  select(ct_id, stat_name_com, stat_value, dimdt_name, dimdt_factor, file_name_resource,
         policy_status_resource, insured_type, profit_insurer_class, buz_type, 
         insurance_type_dimot1, payment_method_stat, investment_type) 

hkla <- com_df_clean %>% 
  mutate(dimot_name_2 = ifelse(!is.na(policy_status_resource), "policy_status", NA),
         dimot_name_3 = ifelse(!is.na(insured_type), "insured_type", NA),
         dimot_name_4 = ifelse(!is.na(investment_type), 
                               "investment_type", NA),
         dimot_name_6 = ifelse(!is.na(buz_type), 
                               "buz_type", NA),
         dimot_name_7 = ifelse(!is.na(insurance_type_dimot1), 
                               "insurance_type", NA),
         dimot_name_8 = ifelse(!is.na(payment_method_stat), 
                               "payment_method", NA)) %>% 
  separate(file_name_resource, c("dimot_name_1", "dimot_factor_1"), sep = "@") %>% 
  separate(profit_insurer_class, c("dimot_name_5", "dimot_factor_5"), sep = "@") %>% 
  rename(stat_name = stat_name_com,
         dimot_factor_2 = policy_status_resource,
         dimot_factor_3 = insured_type,
         dimot_factor_4 = investment_type,
         dimot_factor_6 =buz_type,
         dimot_factor_7 =insurance_type_dimot1,
         dimot_factor_8 = payment_method_stat) %>% 
  select(ct_id, stat_name, stat_value, dimdt_name, dimdt_factor,
         dimot_name_1, dimot_factor_1, dimot_name_2, dimot_factor_2,
         dimot_name_3, dimot_factor_3, dimot_name_4, dimot_factor_4,
         dimot_name_5, dimot_factor_5, dimot_name_6, dimot_factor_6,
         dimot_name_7, dimot_factor_7, dimot_name_8, dimot_factor_8)

# save data ====================================================================================
save(hkla, 
     file = "explore/hk_ia/data/hkla.rda")

