#' Preprocess SEER instruction files.
#'
#'
#' Datasets downloaded from SEER website (after the request process) come in
#' a compressed file containing, among other things, a .sas file with
#' instructions on data fields (fields width) to parse the text files and also
#' one sentence explanation for every field. One can also obtain data from SEER
#' through the SEER*Stat software. In this case, the text file may come in
#' different ways in a dictionary file (.dic).
#'
#' @param file_path A path to the .sas file or .dic file from SEER*Stat.
#' @param filetype The type of instruction file you are providing. If it is a
#'   a dictionary file from SEER*Stat the option is 'dictionary'. Otherwise the
#'   option is 'sas'.
#'
#' @return A list with instructions for \link{readSEER}.
#'
#' @examples
#' \dontrun{
#' instr <- preprocessSEER("read.seer.research.nov17.sas", filetype = 'sas')}
#'
#' @import readr dplyr stringr crayon
#' @importFrom rlang .data
#'
#' @export
preprocessSEER <- function(file_path, filetype) {
  if (filetype == 'sas') {
    options(warn = -1)
    sas.raw <- readr::read_lines(file_path)
    sas.df <- dplyr::tibble(raw = sas.raw) %>%
      # remove first few rows by insisting an @ that defines the start index
      # of that field
      dplyr::filter(stringr::str_detect(raw, "@")) %>%
        # extract out the start, width and column name+description fields
        dplyr::mutate(start = stringr::str_replace(
          stringr::str_extract(raw, "@ [[:digit:]]{1,3}"), "@ ", ""),
                      width = stringr::str_replace(
                        stringr::str_extract(raw,
                                             "\\$char[[:digit:]]{1,2}"),
                                             "\\$char", ""),
                      col_name = stringr::str_extract(
                        raw, "[[:upper:]]+[[:upper:][:digit:][:punct:]]+"),
                      col_desc = stringr::str_trim(
                        stringr::str_replace(stringr::str_replace(
                          stringr::str_extract(raw,
                                               "\\/\\*.+\\*\\/"),
                                               "\\/\\*", ""),
                                               "\\*\\/",
                                               "" )) ) %>%
      # coerce to integers
      dplyr::mutate_at(dplyr::vars(.data$start, .data$width), dplyr::funs(as.integer)) %>%
      # calculate the end position
      dplyr::mutate(end = .data$start + .data$width - 1)

    column_mapping <- sas.df %>%
      dplyr::select(.data$col_name, .data$col_desc)
    options(warn = 0)
    instructions <- list('sas', sas.df)
  } else if (filetype == 'dictionary') {
    con <- file(file_path, "r")
    column_labels <- NULL
    while (TRUE) {
      line <- readLines(con, n = 1)
      if (length(line) == 0) {
        break
      }
      if (grepl("^Var[0-9]*Name=", line)) {
        column_labels <- cbind(column_labels, gsub("Var[0-9]*Name=", "", line))
      }
      if (grepl("^Field delimiter=", line)) {
        separator <- gsub("^Field delimiter=", "", line)
      }
      if (grepl("^Variable names included=", line)) {
        col_names <- gsub("Variable names included=", "", line)
        if (col_names == 'true') {
          col_names <- TRUE
        } else {
          col_names <- FALSE
        }
      }
    }
    close(con)
    instructions <- list('dictionary', as.vector(column_labels),
                         separator, col_names)
  } else {
    print(paste(cat(crayon::red("Error:")), " Option for filetype parameter not recognized. You must choose either 'sas' or 'dictionary'."))
  }
}
