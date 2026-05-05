#MWM_dict_raw <- readRDS("data_raw/MWM_dict.RDS")
MWM_dict_raw <- readxl::read_xlsx("data_raw/MWM_dict.xlsx")
#names(MWM_dict_raw) <- c("key", "DE", "EN")
MWM_dict_raw <- MWM_dict_raw[,c("key", "EN", "DE","DE_F")]
MWM_dict <- psychTestR::i18n_dict$new(MWM_dict_raw)
usethis::use_data(MWM_dict, overwrite = TRUE)
