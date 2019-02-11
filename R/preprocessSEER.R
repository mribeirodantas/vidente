#' Reads a sas instruction file in order to parse SEER ASCII data files
#' @param file_path A path to the SAS file
#' @return A data frame with instruction for readSEER
#' @examples
#' # You must preprocess a SAS file before reading the data with readSEER
#' # Do not run
#' # preprocessSEER("read.seer.research.nov17.sas")
#' @import readr dplyr stringr 
#' @export
preprocessSEER <- function(file_path) {
  options(warn=-1)
  sas.raw <- readr::read_lines(file_path)
  sas.df <<- dplyr::tibble(raw = sas.raw) %>% 
    ## remove first few rows by insisting an @ that defines the start index of that field
    dplyr::filter(stringr::str_detect(raw, "@")) %>% 
    ## extract out the start, width and column name+description fields
    dplyr::mutate(start = stringr::str_replace(stringr::str_extract(raw, "@ [[:digit:]]{1,3}"), "@ ", ""),
           width = stringr::str_replace(stringr::str_extract(raw, "\\$char[[:digit:]]{1,2}"), "\\$char", ""),
           col_name = stringr::str_extract(raw, "[[:upper:]]+[[:upper:][:digit:][:punct:]]+"),
           col_desc = stringr::str_trim(stringr::str_replace(stringr::str_replace(stringr::str_extract(raw, "\\/\\*.+\\*\\/"), "\\/\\*", ""), "\\*\\/", "" )) ) %>% 
    ## coerce to integers
    dplyr::mutate_at(dplyr::vars(start, width), dplyr::funs(as.integer)) %>% 
    ## calculate the end position
    dplyr::mutate(end = start + width - 1)
  
  column_mapping <- sas.df %>% 
    dplyr::select(col_name, col_desc)
  options(warn=0)
}

