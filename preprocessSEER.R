# function to read sas instruction file in order to parse SEER ASCII data files
preprocessSEER <- function(file_path) {
  library(tidyverse)
  library(stringr)
  sas.raw <- read_lines(file_path)
  sas.df <- tibble(raw = sas.raw) %>% 
    ## remove first few rows by insisting an @ that defines the start index of that field
    filter(str_detect(raw, "@")) %>% 
    ## extract out the start, width and column name+description fields
    mutate(start = str_replace(str_extract(raw, "@ [[:digit:]]{1,3}"), "@ ", ""),
           width = str_replace(str_extract(raw, "\\$char[[:digit:]]{1,2}"), "\\$char", ""),
           col_name = str_extract(raw, "[[:upper:]]+[[:upper:][:digit:][:punct:]]+"),
           col_desc = str_trim(str_replace(str_replace(str_extract(raw, "\\/\\*.+\\*\\/"), "\\/\\*", ""), "\\*\\/", "" )) ) %>% 
    ## coerce to integers
    mutate_at(vars(start, width), funs(as.integer)) %>% 
    ## calculate the end position
    mutate(end = start + width - 1)
  
  column_mapping <- sas.df %>% 
    select(col_name, col_desc)
}

