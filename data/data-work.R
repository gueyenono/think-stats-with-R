# Useful packages

library(fs)
library(stringr)
library(dplyr)
library(purrr)
library(readr)
library(stringdist)

# Transfer (stata) data and dictionary files to the data/raw directory

# fs::dir_ls("think-stats-repo/code/", all = TRUE) %>%
# 	stringr::str_subset(pattern = "\\.dat|\\.gz|\\.dct|\\.csv$") %>%
# 	fs::file_copy(new_path = "data/raw/")

# Custom function to turn stata dictionary files (.dct) into tibbles

dct_to_tbl <- function(dct){
	
	dictionary <- readLines(dct)
	relevant_rows <- str_which(dictionary, pattern = "column")
	
	dictionary[relevant_rows] %>%
		trimws() %>%
		tibble::tibble(content = .) %>%
		tidyr::extract(col = "content", into = c("column_id", "type", "column_name", "number_of_chars", "description"), regex = "(^\\S*)\\s+(\\S*)\\s+(\\S*)\\s+(\\S*)\\s+(.*$)") %>%
		mutate(column_id = str_extract(column_id, pattern = "\\d+") %>% as.integer,
				 number_of_chars = str_extract(number_of_chars, pattern = "\\d+") %>% as.integer,
				 description = str_replace_all(description, pattern = '\\\"', replacement = ""))
}


# list() of all dictionaries + individual dictionary saved to .rda files in the processed folder

dct_file_names <- fs::dir_ls("data/raw/") %>% # relative paths to all .dct files
	str_subset("\\.dct$")

dcts_list <- dct_file_names %>% # list() of all parsed dct files
	map(dct_to_tbl)

dcts_list %>% # save the list() of dictionaries to disk
	save(file = "data/processed/dct/r-list-of-dcts.Rda")

dct_file_names_no_ext <- dct_file_names %>% # save each individual dictionary to disk
	str_replace("^.*/(.*)\\.dct$", "\\1")

dct_file_names_no_ext %>% # save each individual dictionary to disk
	walk2(dcts_list, ., ~ write_rds(.x, paste0("data/processed/dct/", .y, ".Rds")))


# data frame (tibble) of .dat files and their corresponding .dct files (where applicable)

dat_file_names <- fs::dir_ls("data/raw/") %>% # relative paths to all .dat files
	str_subset("\\.dat\\.gz$")

dat_file_names_no_ext <- dat_file_names %>%
	str_replace("^.*/(.*)\\.dat\\.gz$", "\\1")

indices <- map(dct_file_names_no_ext, stringdist, b = dat_file_names_no_ext) %>%
	map_dbl(which.min)

dat_dct_tbl <- tibble(
	dat = dat_file_names[indices],
	dct = str_subset(dir_ls("data/processed/dct/"), "\\.Rds$")
)

# Convert .dat files into .Rda files using the information from their corresponding .dct files

dats_list <- pmap(dat_dct_tbl, function(dat, dct){
	dct <- read_rds(dct)
	read_fwf(dat, fwf_widths(widths = dct$number_of_chars, col_names = dct$column_name))
})

dats_list %>%
	walk2(dat_file_names_no_ext[indices], ~ saveRDS(.x, file = paste0("data/processed/dat/", .y, ".Rds")))
