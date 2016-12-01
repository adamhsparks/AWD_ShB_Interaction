AWD <- rbind(DS2015, DS2016)

AWD <-
  AWD %>% mutate_each(funs(as.numeric), starts_with("SL"))

AWD <-
  AWD %>%
  # Make df tidy/long based on "MGW" variables; you could also use any of the
  # select functions listed in help(select_helpers, "dplyr")
  gather(variable, value, starts_with("SL")) %>%
  group_by(SMPL, HILL) %>%  # Group by row ID
  mutate(SL_row_mean = round(mean(value, na.rm = TRUE), 1)) %>% # Get the mean of all MGW variables in the group/ID
  spread(variable, value)  # Restore df to untidy/wide

