MWM_item_bank <- read.csv("data_raw/MWM_item_bank.csv", stringsAsFactors = F) %>%
  as_tibble() %>%
  mutate(audio_file = str_replace(merged_file, "Task", "mwm") %>% str_remove("_merged"),
         correct = c("same" = 1, "diff" = 2)[correct]) %>%
  select(item, answer = correct, discrimination, difficulty, guessing, inattention, audio_file)

usethis::use_data(MWM_item_bank, overwrite = TRUE)
