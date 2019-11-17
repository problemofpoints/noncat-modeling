
# upload non-cat files to analyze re

library(tidyverse)
library(analyzere)

# ---- import and create files -------------------------------------------------------------

noncat_yelt <- read_csv("three_lob_example.csv") %>% 
  mutate(lob_num = as.numeric(str_sub(eventid, 1, 1)),
         loss_type = if_else(str_sub(eventid, -1)=="0","attr","large"))


event_catalog_data <- noncat_yelt %>% 
  distinct(eventid) %>% 
  mutate(rate = 1/10000,
         lob_num = as.numeric(str_sub(eventid, 1, 1)),
         loss_type = if_else(str_sub(eventid, -1)=="0","attr","large"))

sim_grid <- noncat_yelt %>% 
  select(trialid, eventid, day) %>% 
  distinct(trialid, eventid, day) %>% 
  arrange(trialid, day, eventid)

# ---- upload to analyze re ----------------------------------------------------------------

are_set_credentials()

meta_data_list <- list(client = "XYZ Ins Co",
                       program_name = "Casualty XOL",
                       program_type = "Casualty",
                       year = 2020)

event_catalog <- are_EventCatalog(description = "3 non-cat lob example",
                                   meta_data = meta_data_list,
                                  source = "3 non-cat lob example") %>% 
  are_save() %>% 
  are_upload_data(data_or_file = event_catalog_data)

yelt <- are_LossSet(type = "YELTLossSet",
                    event_catalogs = event_catalog,
                    loss_type = "LossGross",
                    description = "3 non-cat lob example",
                    start_date = "2020-01-01",
                    trial_count = 10000,
                    meta_data = meta_data_list) %>% 
  are_save() %>% 
  are_upload_data(noncat_yelt %>% select(trialid, eventid, day, loss))

yelt1 <- are_LossSet(type = "YELTLossSet",
                    event_catalogs = event_catalog,
                    loss_type = "LossGross",
                    description = "3 non-cat lob example - lob1",
                    start_date = "2020-01-01",
                    trial_count = 10000,
                    meta_data = c(meta_data_list, "lob" = 1)) %>% 
  are_save() %>% 
  are_upload_data(noncat_yelt %>% filter(lob_num == 1) %>% 
                    select(trialid, eventid, day, loss))

yelt2 <- are_LossSet(type = "YELTLossSet",
                    event_catalogs = event_catalog,
                    loss_type = "LossGross",
                    description = "3 non-cat lob example - lob2",
                    start_date = "2020-01-01",
                    trial_count = 10000,
                    meta_data = c(meta_data_list, "lob" = 2)) %>% 
  are_save() %>% 
  are_upload_data(noncat_yelt %>% filter(lob_num == 2) %>% 
                    select(trialid, eventid, day, loss))

yelt3 <- are_LossSet(type = "YELTLossSet",
                    event_catalogs = event_catalog,
                    loss_type = "LossGross",
                    description = "3 non-cat lob example - lob3",
                    start_date = "2020-01-01",
                    trial_count = 10000,
                    meta_data = c(meta_data_list, "lob" = 3)) %>% 
  are_save() %>% 
  are_upload_data(noncat_yelt %>% filter(lob_num == 3) %>% 
                    select(trialid, eventid, day, loss))


simulate_set <- are_StaticSimulation(name = "3 non-cat lob example",
                                     description = "3 non-cat lob example",
                                     event_catalogs = event_catalog,
                                     start_date = "2020-01-01",
                                     trial_count = 10000,
                                     meta_data = meta_data_list) %>% 
  are_save()
simulate_set <- simulate_set %>% are_upload_data(sim_grid)

# create loss filters: all, by lob, all large, all attr

loss_filter_all <- are_LossFilter(type = "AnyFilter",
                                  name = "all",
                                  description = "All losses",
                                  attribute = "loss_type") %>% are_save()

loss_filter_large <- are_LossFilter(type = "AnyOfFilter",
                                  name = "large",
                                  description = "Large losses",
                                  attribute = "loss_type",
                                  values = "large") %>% are_save()

loss_filter_attr <- are_LossFilter(type = "AnyOfFilter",
                                    name = "attritional",
                                    description = "Attritional losses",
                                    attribute = "loss_type",
                                    values = "attr") %>% are_save()

loss_filter_lob1 <- are_LossFilter(type = "AnyOfFilter",
                                   name = "lob1",
                                   description = "lob1",
                                   attribute = "lob_num",
                                   values = "1") %>% are_save()

loss_filter_lob2 <- are_LossFilter(type = "AnyOfFilter",
                                   name = "lob2",
                                   description = "lob2",
                                   attribute = "lob_num",
                                   values = "2") %>% are_save()

loss_filter_lob3 <- are_LossFilter(type = "AnyOfFilter",
                                   name = "lob3",
                                   description = "lob3",
                                   attribute = "lob_num",
                                   values = "3") %>% are_save()

# create analysis profile
analysis_profile <- are_AnalysisProfile(description = "3 non-cat lob example",
                                        loss_filters = list(loss_filter_all,
                                                            loss_filter_large,
                                                            loss_filter_attr,
                                                            loss_filter_lob1,
                                                            loss_filter_lob2,
                                                            loss_filter_lob3),
                                        event_catalogs = event_catalog,
                                        simulation = simulate_set,
                                        exchange_rate_profile = are_ExchangeRateProfile() %>% 
                                          are_retrieve("e5cc85b3-869b-4790-9406-918048a8c38a"),
                                        meta_data = meta_data_list) %>% 
  are_save()



