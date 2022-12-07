## code to prepare `data-raw/MST.xlsx` dataset goes here

SAA_dict_df <- readxl::read_excel("data-raw/SAA.xlsx")
SAA_dict_df$de <- "a"

SAA_dict <- musicassessr::dict(additional_dict = SAA_dict_df)

da <- SAA_dict$as.data.frame()

usethis::use_data(SAA_dict_df, SAA_dict, overwrite = TRUE, internal = TRUE)

