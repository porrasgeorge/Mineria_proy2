
dataLog <- dataLog %>% filter(!grepl('Sub Laguna-SD79SWinDemand|Sub Zarcero-SD79SWinDemand', Meter))
## write_feather(dataLog, "featherFiles/dataLog_v2.feather")
