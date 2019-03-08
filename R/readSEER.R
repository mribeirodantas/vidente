#' Reads SEER data from ASCII data file(s).
#'
#' @param path A path or a vector with paths to either folder with SEER ASCII
#'   data files or directly to the file(s).
#' @param instructions The output variable from the buildSEERParser function
#' @param read_dir If true, it reads all .TXT files in the directory or
#'   directories provided in the path parameter. If this parameter is missing
#'   or FALSE, vidente will only read the TXT you explicitly informed. This
#'   function does not work with data exported from the SEER*Stat software.
#' @param year_dx Filter the result to this year of diagnosis (or range of
#'   years of diagnosis)
#' @param primary_site Filter the result to a primary site (or a vector of
#'   primary sites for SEER*Stat files, or a site category from
#'   listPrimarySite() for data downloaded from SEER).
#' @return A data frame with all SEER data from the ASCII data file(s) you
#'   provided, given the criteria you chose in the parameters.
#'
#' @examples
#' # First build parsing instructions
#' \dontrun{
#' instr <- buildSEERParser(file_path = 'read.seer.research.nov17.sas',
#'                          file_source = 'download')
#'
#' # Now you can read it
#' paths <- c('/home/yourusername/SEER/yr1973_2015.seer9/BREAST.TXT',
#'           '/home/yourusername/SEER/yr2000_2015.ca_ky_lo_nj_ga/BREAST.TXT')
#'
#' # I'm interested here in patients with breast cancer diagnosed between 2012
#' # and 2015
#' seer_data <- readSEER(path = paths,
#'                       instructions = instr,
#'                       year_dx = c(2012:2015),
#'                       primary_site = 'Breast')}
#'
#' @import readr
#' @export
readSEER <- function(path, instructions, read_dir = FALSE, year_dx,
                     primary_site) {
  # Some parameter checking
  # Path to files checking
  if (missing(path)) {
    stop("It's impossible to read the files if you supply no path to files.")
  }
  # Parsing instructions checking
  if (missing(instructions)) {
    stop("You must supply the instructions parameter.")
  } 
  if (!class(instructions) == 'list') {
    stop(paste('Option not recognized. The instructions parameter does not',
               'look like it came from the preprocessSEER function.'))
  }
  # Year of diagnosis parameter checking
  if (missing(year_dx)) {
    print(paste("Note: Year of diagnosis not supplied, including all years",
                "contained in the files provided."))
  }
  # Primary site parameter checking
  if (missing(primary_site)) {
    print(paste("Note: Cancer primary site not supplied, including all sites",
                "contained in the files provided."))
  } else {
    code <- primarySiteLookUp(primary_site)
    if (code == -1) {
      stop("Primary site name invalid. Check listPrimarySites()")
    }
  }
  # Directory reading parameter checking
  if (!missing(read_dir)) {
    if (instructions[[1]] != 'download') {
      stop("read_dir parameter does not work with data exported from SEER*Stat")
    }
  }
  ## read the file with the fixed width positions
  if (instructions[[1]] == 'download') {
    data.df_f <- NULL
    # TXT files will be read
    if (read_dir == FALSE) {
      if (!all(endsWith(path, ".TXT"))) {
        stop(paste("All the paths in your vector must point to a .TXT",
                   "file. Check the read_dir parameter if you want to",
                   "read all text files a directory."))
      }
      if (!all(file.exists(path))) {
        stop("At least one of the files in your vector does not exist.")
      }
      for (path_file in path) {
        data.df <- readr::read_fwf(
          path_file, readr::fwf_positions(instructions[[2]]$start,
                                          instructions[[2]]$end,
                                          instructions[[2]]$col_name))
        data.df_f <- rbind(data.df_f, data.df)
      }
      # Folders will be read
    } else {
      if (!all(dir.exists(path))) {
        stop(paste("At least one of the directories in your vector",
                   "does not exist."))
      }
      if (all(endsWith(path, ".TXT"))) {
        stop(paste("If read_dir is set to TRUE, your vector must",
                   "contain directory paths and not file paths."))
      }
      for (path_file in path) {
        for (file in dir(path_file)) {
          data.df <- readr::read_fwf(paste(path_file, file, sep = ""),
                                     readr::fwf_positions(
                                       instructions[[2]]$start,
                                       instructions[[2]]$end,
                                       instructions[[2]]$col_name))
          data.df_f <- rbind(data.df_f, data.df)
        }
      }
    }
    # select only those rows that match the chosen site
    if (!missing(primary_site)) {
      data.df_f <- data.df_f[data.df_f$SITERWHO %in% code, ]
    }
    # select only those rows that match the chosen year of diagnosis
    # (or range of)
    if (!missing(year_dx)) {
      data.df_f <- data.df_f[data.df_f$YEAR_DX %in% year_dx, ]
    }
  } else if (instructions[[1]] == 'seerstat') {
    # Name columns based on CSV or dict?
    sep <- ifelse(instructions[[3]] == 'tab', '\t', ',') 
    if (instructions[[4]] == TRUE) {
      data.df_f <- readr::read_delim(file = path, sep, col_names = TRUE)
    } else if (instructions[[4]] == FALSE) {
      data.df_f <- readr::read_delim(file = path, sep,
                                     col_names = instructions[[2]])
    } else {
      stop("readSEER can't interpret if variable names came or not in the data")
    }
    # select only those rows that match the chosen site
    if (!missing(primary_site)) {
      data.df_f <- data.df_f[
        data.df_f$`Site recode ICD-O-3/WHO 2008` %in% primary_site, ]
    }
    # select only those rows that match the chosen year of diagnosis
    # (or range of)
    if (!missing(year_dx)) {
      data.df_f <- data.df_f[data.df_f$`Year of diagnosis` %in% year_dx, ]
    }
  } else {
    stop(paste('Option not recognized. The instructions parameter does not',
               'look like it came from the preprocessSEER function.'))
  }
  return(data.df_f)
}
#' Converts primary site name to recode
#' @param site_name cancer primary site name
#' @return Recode code for the specified cancer primary site name
#' @import stringr utils data.tree
#' @keywords internal
primarySiteLookUp <- function(site_name) {
  # Gotta work on a way to help the user if he mispells the site name
  tree_node <- FindNode(node = sites, name = site_name)
  # Does the node exist?
  if (is.null(tree_node)) {
    # The node does not exist
    return(-1)
  } else if (tree_node$count > 0) {
    # It is a category node
    recodes <- c()
    for (leaf in tree_node$children) {
      recodes <- c(recodes, leaf$recode)
    }
    return(recodes)
  } else {
    # The node exists and it is a leaf
    return(tree_node$recode)
  }
}

#' List cancers primary sites that are supported by this package
#' @return It displays on the screen the cancer primary sites supported by this
#'   package.
#' @seealso \url{https://seer.cancer.gov/siterecode/icdo3_dwhoheme/index.html}
#' @export
listPrimarySites <- function() {
  print(sites)
}