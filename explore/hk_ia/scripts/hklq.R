library(stringr)
library(tidyverse)
library(data.table)

load("data/consolidated_data/hk_insurance_stat_longterm_quarterly.rda")

# stat_value ===================================================================================
value_df <- hk_insurance_stat_longterm_quarterly %>% 
  mutate_all(tolower) %>%
  mutate_all(~ ifelse(str_detect(., "non-"), 
                      str_replace(., "non-", "non_"), . )) %>% 
  filter(!(stat_name %like% "total"),
         !(dimot_factor_1 %like% "total"),
         !(dimot_factor_2 %like% "total")) %>% 
  mutate(stat_value = as.numeric(stat_value),
         stat_value = ifelse(str_detect(stat_name, "[$]"),
                             stat_value * 1000, stat_value),
         row_id = as.numeric(rownames(.)))


# dimot_2 ======================================================================================
dimot_2_df <- value_df %>% 
  separate(dimot_factor_2, c("a", "b"), sep = "-") %>% 
  mutate(buz_type_2 = gsub("\\(.*\\) ", "", a),
         plan_class_2 = str_match(b, "base plan|supplementary contracts"),
         plan_type_2 = gsub("base plan|supplementary contracts(.*)",
                          "\\1", gsub("\\(.*\\) ", "", b)),
         plan_type_2 = str_remove(plan_type_2, ": "))  %>% 
  mutate_all(list(~na_if(.,""))) %>% 
  select(row_id, buz_type_2, plan_class_2, plan_type_2)
  
# dimot_1 ======================================================================================
dimot_1_df <- value_df %>% 
  mutate(buz_class_1 = ifelse(grepl("class", dimot_name_1), 
                            dimot_factor_1, NA),
         insurer_name_1 = ifelse(grepl("name of insurer", dimot_name_1),
                               paste0("insurer_name@", dimot_factor_1), NA)) %>% 
  select(row_id, buz_class_1, insurer_name_1)

# stat_name ====================================================================================
stat_df <- value_df %>% 
  mutate(stat_name = str_replace(stat_name, "no. of", "number of"),
         stat_name = str_replace(stat_name, "transferred-", "transferred_"),
         stat_name = str_replace(stat_name, "non-", "non_"),
         stat_name = str_replace(stat_name, "-13th", "_13th"),
         stat_name = str_replace(stat_name, "-25th", "_25th"),
         investment_type_stat = str_match(stat_name, "linked|non_linked"),
         policy_status_stat = str_match(stat_name, "new|inforce"),
         payment_method_stat = str_match(stat_name, "single|non_single|annualized"),
         insured_type_stat = str_match(stat_name, "individual|group"),
         stat_sep = stat_name) %>% 
  separate(stat_sep, c("a", "b", "c", "d"), sep = "-") %>% 
  mutate(curr_shore_distri_stat = ifelse(grepl("currency|^onshore|^distribution", a),
                                    paste(str_replace(a, " / | ", "_"), 
                                          gsub("\\(.*\\) |policy issued in ", "", b), 
                                          sep = "@"), NA),
         stat_name_curr = ifelse(grepl("currency|^onshore|^distribution", a), 
                                 str_match(c, ".*policies|premiums receivable|premiums|revenue premiums"), 
                                 NA),
         premium_term_stat = ifelse(grepl("premium term", a),
                                    ifelse(str_detect(c, "[$]"),
                                           paste0("premium_term@", 
                                                  gsub(".*premiums \\((.*)\\)", "\\1", b)),
                                           paste0("premium_term@", 
                                                  gsub(".*premiums \\((.*)\\)", "\\1", c))),
                                    NA),
         premium_term_stat = ifelse(str_detect(premium_term_stat, "single"),
                                    "premium_term@single", premium_term_stat),
         stat_name_term = ifelse(grepl("premium term", a),
                                 ifelse(str_detect(c, "[$]"),
                                        str_match(b, "premiums receivable|premiums|revenue premiums"),
                                        b), NA),
         stat_name_business = ifelse(str_detect(a, "business"), b, NA),
         buz_class_stat = ifelse(str_detect(a, "class"),
                                 gsub(".*class |.*classes (.*)", "\\1",
                                      str_remove(a, "[)]$")), NA),
         buz_type_stat = case_when(str_detect(a, "class")&str_detect(b, "[(]") ~
                                     gsub(".*) (.*)", "\\1", b),
                                   str_detect(a, "class|retirement") ~ 
                                     str_match(a, "retirement scheme|non_retirement scheme")),
         stat_name_class = ifelse(str_detect(a, "class")&!str_detect(a, "business"),
                                  str_match(stat_name, "revenue premiums|premiums"), NA),
         termination_type_stat = ifelse(str_detect(a, "terminated")&!is.na(b), 
                                        paste0("termination_type@", b), NA),
         stat_name_terminate = ifelse(str_detect(a, "terminated"), 
                                      gsub("(.*) \\in the period", "\\1", a), NA),
         reinsurance_type_stat = ifelse(str_detect(a, "reinsurance"), 
                                        paste0("reinsurance_type@", a), NA),
         stat_name_reinsurance = ifelse(str_detect(a, "reinsurance"),
                                        str_remove(b, " as at the end of the period| in revenue account in the period"), NA),
         contribution_type_stat = ifelse(str_detect(a, "contribution"), 
                                         paste0("contribution_type@", b), NA),
         benefit_type_stat = ifelse(str_detect(b, "benefits"),
                                    paste0("benefit_type@", b), NA),
         premium_type_stat = str_match(c, "first year|renewal"),
         premium_type_stat = ifelse(!is.na(premium_type_stat),
                                    paste0("premium_type@", premium_type_stat), NA),
         stat_name_others = ifelse(!str_detect(a,"^currency|^onshore|^distribution|term$|business|class|terminated|reinsurance"), 
                                   a, NA),
         stat_name_stat = coalesce(stat_name_business, stat_name_class, stat_name_curr,
                                   stat_name_others, stat_name_reinsurance,
                                   stat_name_term, stat_name_terminate)) %>% 
  select(row_id, contains("_stat"))

# resource_name ================================================================================
resource_df <- value_df %>% 
  mutate(file_name = paste0("file_name@", word(resource_name, 1)),
         resource_name = str_remove(resource_name, ".*statistics"),
         resource_name = str_replace(resource_name, "in-force", "in_force"),
         underwriting_type_resource = str_match(resource_name, "direct|reinsurance"),
         policy_status_resource = str_match(resource_name, "new|inforce|in_force"),
         insured_type_resource = str_match(resource_name, "individual|group"),
         is_mainland_visitors_resource = str_match(resource_name, "mainland visitors"),
         is_mainland_visitors_resource = str_replace(is_mainland_visitors_resource,
                                                     "mainland visitors",
                                                     "is_mainland_visitors@1")) %>% 
  select(row_id, file_name, contains("_resource"))

# combine and clean tables =====================================================================
com_df <- list(value_df, dimot_1_df, dimot_2_df, stat_df, resource_df) %>% 
  reduce(left_join, by = "row_id")

com_df_clean <- com_df %>% 
  mutate(buz_class_com = coalesce(buz_class_1, buz_class_stat),
         buz_type_com = coalesce(buz_type_2, buz_type_stat),
         insured_type_com = coalesce(insured_type_resource, insured_type_stat),
         policy_status_com = coalesce(policy_status_stat, policy_status_resource),
         policy_status_com = str_replace(policy_status_com, "inforce", "in_force"),
         stat_com = coalesce(curr_shore_distri_stat, premium_term_stat,
                             premium_type_stat, termination_type_stat,
                             benefit_type_stat, reinsurance_type_stat, 
                             contribution_type_stat),
         insurer_mainland_com = coalesce(insurer_name_1,
                                         is_mainland_visitors_resource),
         buz_class_com = ifelse(!is.na(buz_class_com), 
                                paste0("buz_class@", buz_class_com), NA),
         buz_type_com = ifelse(!is.na(buz_type_com),
                               paste0("buz_type@", buz_type_com), NA),
         insured_type_com = ifelse(!is.na(insured_type_com),
                                   paste0("insured_type@", insured_type_com), NA),
         policy_status_com = ifelse(!is.na(policy_status_com),
                                    paste0("policy_status@", policy_status_com), NA),
         investment_type_com = ifelse(!is.na(investment_type_stat), 
                                      paste0("investment_type@", investment_type_stat), NA),
         payment_method_com = ifelse(!is.na(payment_method_stat), 
                                     paste0("payment_method@", payment_method_stat), NA),
         plan_class_com = ifelse(!is.na(plan_class_2),
                                 paste0("plan_class@", plan_class_2), NA),
         plan_type_com = ifelse(!is.na(plan_type_2), 
                                paste0("plan_type@", plan_type_2), NA),
         underwriting_type_com = ifelse(!is.na(underwriting_type_resource), 
                                        paste0("underwriting_type@", 
                                               underwriting_type_resource), NA),
         stat_name = str_remove(stat_name_stat, 
                                " in revenue account in the period| as at the end of the period| in the period"),
         stat_name = str_remove(stat_name, "non_single|single|amount of"),
         stat_name = str_remove(stat_name, "annualized"),
         stat_name = str_squish(stat_name)) %>% 
  mutate_all(list(~na_if(.,""))) %>% 
  arrange(row_id) %>% 
  select(ct_id, stat_name, stat_value, file_name, contains(c("_com", "dimdt_")))

# rename column names ========================================================================== 
com_df_colsplit <- com_df_clean %>%   
  select(file_name, underwriting_type_com, policy_status_com, 
         insured_type_com, payment_method_com, insurer_mainland_com, 
         stat_com, investment_type_com, buz_class_com, buz_type_com,
         plan_class_com, plan_type_com) 

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
hklq <- cbind(com_df_clean[, c("ct_id", "stat_name", "stat_value", "dimdt_name", "dimdt_factor")], 
              sep(1),sep(2), sep(3), sep(4), sep(5), sep(6), sep(7), sep(8), sep(9),
              sep(10), sep(11), sep(12))

# save table ===================================================================================
save(hklq, file = "explore/hk_ia/data/hklq.rda")


