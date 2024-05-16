# ============================================================================ #
# compare web scrap and official API =========================================
# ============================================================================ #

# ============================================================================ #
# fidings:
# the data list acquired from official api is a subset of the list acquired from web scrapping
# most of the data in list_oa has historical data, except #28 (< 0.1 %)
# most of the data of historical data is in the list_oa except #881 (<0.5%), this might due to non-update
# so the conclusion is: the offical api is only support for those data with historical version,
# which means they archieve them
# ============================================================================ #


# 0.Configuration ==============================================================
source("utils.R")
library(DT)

chk_missing_missonly <- function(data){
  chk_missing <- data.frame(missing_cnt = colSums(is.na(data)))
  chk_missing <- data.frame(var = row.names(chk_missing),
                            chk_missing) %>%
    mutate(missing_pct = 100 * missing_cnt / nrow(data)) 
  
  a <- deparse(substitute(data))
  chk_missing %>%
    filter(missing_pct > 0) %>%
    arrange(desc(missing_pct)) %>%
    datatable(caption = paste("Percentage of Missings in [",a,"]"),
              options = list(pageLength = 100))
}


# 1. full list from official api ===============================================
full_list_api <- list_hist_file_nomax()

# chking... ------------------------------------------------------------------ #
nrow(full_list_api) # 12861
chk_missing_missonly(full_list_api) # no missing

n_distinct(paste(full_list_api$`dataset-id`,
                 full_list_api$`resource-name-en`,
                 # full_list_api$`format`,
                 # full_list_api$`total-size`,
                 full_list_api$`url`
                 )
           )

n_distinct(full_list_api$url) #12861

n_distinct(paste(
  full_list_api$`dataset-id`
  # full_list_api$`resource-name-en`,
)
)  # 1175

# findings:
# ~ [result]
# ---------------------------------------------------------------------------- #


# 2. full list from web scrapping ==============================================
load("data/hk_open_data_full_list.rda")

list_resource_df <- list_resource_df %>%
  mutate(blank_ind = as.numeric(data.resource.title.en != data.resource.name.en))

full_list_ws <- list_dataset_df %>% full_join(list_resource_df, by = "dataset_no")

# chking... ------------------------------------------------------------------ #
# dataset_df
chk_missing_missonly(dataset_df)  # no missing
nrow(dataset_df)  #1472
n_distinct(dataset_df$dataset_name)  #1467
n_distinct(paste(
  dataset_df$dataset_name,
  # dataset_df$dataset_category,
  dataset_df$dataset_provider
  )
  )#1468

n_distinct(dataset_df$dataset_link)  #1467

# list_dataset_df
chk_missing_missonly(list_dataset_df)  # no missing 
nrow(list_dataset_df)  #1472
n_distinct(list_dataset_df$data.dataset.id)  #1472
n_distinct(list_dataset_df$data.dataset.title.en)  #1467


# list_resource_df
chk_missing_missonly(list_resource_df)  # no missing 
table(list_resource_df$blank_ind, exclude = NULL)
nrow(list_dataset_df)  #1472

# full_list_ws
# dataset level 
# whether has historical is dataset level 
table(full_list_ws$data.resource.has.historical, exclude = NULL)

a <- full_list_ws %>% 
  group_by(dataset_no, data.resource.has.historical) %>% 
  summarise(n=n()) %>% 
  ungroup()

a %>% group_by(dataset_no) %>% summarise(n=n()) %>% filter(n>1)

# findings:
# ~ [result]
# ---------------------------------------------------------------------------- #


# 3.comparison =================================================================

# dataset level ----------------------------------------------------------------

n_distinct(full_list_api$`dataset-id`)  # 1175
n_distinct(list_dataset_df$data.dataset.id)  # 1472 

 
x <- unique(full_list_api$`dataset-id`)   # 1175
y <- unique(list_dataset_df$data.dataset.id)   # 1472

sum(x %in% y)  # 1175
sum(!x %in% y) # 0
sum(!y %in% x) # 297 = 1472 - 1175


# resource level ---------------------------------------------------------------
n_distinct(full_list_api$`resource-name-en`)  # 10442
n_distinct(full_list_ws$data.resource.name.en)  # 17473 


x <- unique(full_list_api$`resource-name-en`)   # 10442
y <- unique(full_list_ws$data.resource.name.en)   # 17473

sum(x %in% y)  # 10442
sum(!x %in% y) # 0
sum(!y %in% x) # 7031 = 17473 - 10442

full_list_ws <- full_list_ws %>%
  mutate(official_ind = as.numeric(
    data.resource.name.en %in% full_list_api$`resource-name-en`
    )
  )

table(full_list_ws$official_ind, exclude = NULL)

table(full_list_ws$official_ind, 
      full_list_ws$data.resource.has.historical, 
      exclude = NULL)

prop.table(
  table(full_list_ws$official_ind, 
        full_list_ws$data.resource.has.historical, 
        exclude = NULL)
  )

# N     Y
# 0  6744   811
# 1    28 12922
# 
# N           Y
# 0 0.328895391 0.039551329
# 1 0.001365521 0.630187759

# ****** end ****** ============================================================




