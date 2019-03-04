#' Reads SEER data from ASCII data files.
#'
#' @param path A path or a vector with paths to either folder with SEER ASCII
#'   data files or directly to the file(s).
#' @param instructions The output variable from the preprocessSEER function
#' @param read_dir If true, it reads all .TXT files in the directory or
#'   directories provided in the path parameter. If this parameter is missing
#'   or FALSE, vidente will only read the TXT you explicitly informed.
#' @param year_dx Filter the result to this year of diagnosis (or range of
#'   years of diagnosis)
#' @param primary_site Filter the result to this primary site
#' @return A data frame with all SEER data from the ASCII data files you
#'   provided, given the criteria you chose in the parameters.
#'
#' @examples
#' # First preprocess a instruction file
#' \dontrun{
#' instr <- preprocessSEER('read.seer.research.nov17.sas', filetype = 'sas')
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
#' @import readr crayon
#' @export
readSEER <- function(path, instructions, read_dir = FALSE, year_dx,
                     primary_site='') {
  if (missing(path)) {
    stop("It's impossible to read the files if you supply no path to files.")
    }
    if (instructions[[1]] == 'sas') {
      ## read the file with the fixed width positions
        data.df_f <- NULL
        if (read_dir == FALSE) {
            if (all(endsWith(path, ".TXT"))) {
                if (!all(file.exists(path))) {
                  stop("At least one of the files in your vector does not
                       exist.")
                }
                for (path_file in path) {
                  data.df <- readr::read_fwf(
                    path_file, readr::fwf_positions(instructions[[2]]$start,
                                                    instructions[[2]]$end,
                                                    instructions[[2]]$col_name))
                  # , colClasses=c('character','integer','character','integer',
                  # 'character','integer','character',
                  # 'character','character','character'))
                  data.df_f <- rbind(data.df_f, data.df)
                }
            } else {
                stop(paste("All the paths in your vector must point to a .TXT",
                           "file. Check the read_dir parameter if you want to",
                           "read all text files a directory."))
            }
        } else {
            if (!all(dir.exists(path))) {
                stop(paste("At least one of the directories in your vector ",
                           "does not exist."))
            }
            if (all(endsWith(path, ".TXT"))) {
                stop(paste("If read_dir is set to TRUE, your vector must ",
                           "contain directory paths and not file paths."))
            }
            for (path_file in path) {
                for (file in dir(path_file)) {
                  data.df <- readr::read_fwf(paste(path_file, file, sep = ""),
                                             readr::fwf_positions(
                                               instructions[[2]]$start,
                                               instructions[[2]]$end,
                                               instructions[[2]]$col_name))
                  # , colClasses=c('character','integer','character','integer',
                  # 'character','integer','character',
                  # 'character','character','character'))
                  data.df_f <- rbind(data.df_f, data.df)
                }
            }
        }
        # select only those rows that match the chosen site
        # THIS SHOULD
        # BE DONE
        # AT THE BEGINNING
        if (primary_site == '') {
            print(paste(cat(crayon::red("Note:")), paste(" Cancer primary site",
                                                         " not supplied, ",
                                                         "including all sites",
                                                         " contained in the ",
                                                         "files provided.")))
        } else {
          code <- siteLookUp(primary_site)
          if (is.na(code)) {
            stop("Primary site name invalid. Check listPrimarySite()")
          } else {
            ata.df_f <- data.df_f[
                        data.df_f$SITERWHO == code, ]
          }
        }
        # select only those rows that match the chosen year of diagnosis
        # (or range of)
        if (missing(year_dx)) {
            print(paste(cat(crayon::red("Note:")), paste(" Year of diagnosis ",
                                                         "not supplied, ",
                                                         "including all years",
                                                         " contained in the ",
                                                         "files provided.")))
        } else {
            data.df_f <- data.df_f[data.df_f$YEAR_DX %in% year_dx, ]
        }
    } else if (instructions[[1]] == 'dictionary') {
        # Name columns based on CSV or dict?
        data.df_f <- readr::read_csv(file = path)
        # Pending to filter by primary_site and year_dx
    }
    return(data.df_f)
}

# Gotta hide it from the help page
#' Converts primary site name to recode
#' @param site_name cancer primary site name
#' @return Recode code for the specified cancer primary site name
#' @import stringr utils
siteLookUp <- function(site_name) {
    site_name <- stringr::str_to_title(site_name)
    code <- sites_recodes[sites_recodes$sites == site_name, ]$recodes
    code <- as.integer(as.character(code))
    return(ifelse(length(code)>0, code, NA))
}

#' List cancers primary sites that are supported by this package
#' @return It displays on the screen the cancer primary sites supported by the
#'   package
#' @export
listPrimarySites <- function() {
    cat(crayon::red("# Oral Cavity and Pharynx"))
    print(paste(sites_recodes$sites[1:10], collapse = ", "))
    cat(crayon::red("# Digestive System"))
    print(paste(sites_recodes$sites[11:13], collapse = ", "))
    cat(crayon::red("# Colon Excluding rectum"))
    print(paste(sites_recodes$sites[14:22], collapse = ", "))
    cat(crayon::red("# Rectum and Rectosigmoid Junction"))
    print(paste(sites_recodes$sites[23:25], collapse = ", "))
    cat(crayon::red("# Liver and Intrahepatic Bile Duct"))
    print(paste(sites_recodes$sites[26:33], collapse = ", "))
    cat(crayon::red("# Respiratory System"))
    print(paste(sites_recodes$sites[34:38], collapse = ", "))
    cat(crayon::red("# Bones and Joints"))
    print(paste(sites_recodes$sites[39], collapse = ", "))
    cat(crayon::red("# Soft Tissue including Heart"))
    print(paste(sites_recodes$sites[40], collapse = ", "))
    cat(crayon::red("# Skin excluding Basal and Squamous"))
    print(paste(sites_recodes$sites[41:42], collapse = ", "))
    cat(crayon::red("# Breast"))
    print(paste(sites_recodes$sites[43], collapse = ", "))
    cat(crayon::red("# Female Genital System"))
    print(paste(sites_recodes$sites[44:50], collapse = ", "))
    cat(crayon::red("# Male Genital System"))
    print(paste(sites_recodes$sites[51:54], collapse = ", "))
    cat(crayon::red("# Urinary System"))
    print(paste(sites_recodes$sites[55:58], collapse = ", "))
    cat(crayon::red("# Eye and Orbit"))
    print(paste(sites_recodes$sites[59], collapse = ", "))
    cat(crayon::red("# Brain and Other Nervous System"))
    print(paste(sites_recodes$sites[60:61], collapse = ", "))
    cat(crayon::red("# Endocrine System"))
    print(paste(sites_recodes$sites[62:63], collapse = ", "))
    cat(crayon::red("# Lymphoma Hodgkin"))
    print(paste(sites_recodes$sites[64:65], collapse = ", "))
    cat(crayon::red("# Lymphoma Non-Hodgkin"))
    print(paste(sites_recodes$sites[66:67], collapse = ", "))
    cat(crayon::red("# Myeloma"))
    print(paste(sites_recodes$sites[68], collapse = ", "))
    cat(crayon::red("# Lymphocyte Leukemia"))
    print(paste(sites_recodes$sites[69:72], collapse = ", "))
    cat(crayon::red("# Myeloid and Monocytic Leukemia"))
    print(paste(sites_recodes$sites[73:76], collapse = ", "))
    cat(crayon::red("# Other Leukemia"))
    print(paste(sites_recodes$sites[77:78], collapse = ", "))
    cat(crayon::red("# Mesothelioma"))
    print(paste(sites_recodes$sites[79], collapse = ", "))
    cat(crayon::red("# Kaposi Sarcoma"))
    print(paste(sites_recodes$sites[80], collapse = ", "))
    cat(crayon::red("# Miscellaneous and Invalid"))
    print(paste(sites_recodes$sites[81:82], collapse = ", "))
}