#' Reads SEER data from one (or several) SEER ASCII data files or from text
#'   exported from SEER*Stats.
#'
#' @param paths A path or paths to SEER ASCII data files
#' @param read_dir Read all .TXT files in the directories in the vector passed.
#'   If this parameter is missing or FALSE, vidente will only read the TXT you
#'   explicitly informed.
#' @param year_dx Filter the result to this year of diagnosis
#'   (or range of years of diagnosis)
#' @param primary_site Filter the result to this primary site
#' @return A data frame with all SEER data from the ASCII data files you
#'   provided, given the criteria you chose in the parameters.
#' @examples
#' # You must preprocess a SAS file before, so that readSEER
#' # knows the format of your SEER ASCII data files
#' \dontrun{
#' preprocessSEER('read.seer.research.nov17.sas')
#' }
#' 
#' # Now you can read it
#' \dontrun{
#' paths = c('/home/user/SEER/yr1973_2015.seer9/BREAST.TXT',
#'           '/home/user/SEER/yr2000_2015.ca_ky_lo_nj_ga/BREAST.TXT')}
#' 
#' # I'm interested here in patients with Breast cancer diagnosed
#' # between 2012 and 2015
#' \dontrun{
#' # seer_data <- readSEER(paths, c(2012:2015), primary_site='Breast')}
#' 
# If you don't use export, users can't see it
#' @import readr crayon
#' @export
readSEER <- function(paths, seerstats = FALSE, read_dir = FALSE, year_dx,
                     primary_site = FALSE) {
    if (seerstats == FALSE) {
        if (missing(paths)) {
            stop("It's impossible to read the files if you supply no path to
                 files.")
        }
        ## read the file with the fixed width positions
        data.df_f <- NULL
        if (read_dir == FALSE) {
            if (all(endsWith(paths, ".TXT"))) {
                if (!all(file.exists(paths))) {
                  stop("At least one of the files in your vector does not
                       exist.")
                }
                for (path_file in paths) {
                  data.df <- readr::read_fwf(
                    path_file, readr::fwf_positions(sas.df$start,
                                                    sas.df$end,
                                                    sas.df$col_name))
                  # , colClasses=c('character','integer','character','integer',
                  # 'character','integer','character',
                  # 'character','character','character'))
                  data.df_f <- rbind(data.df_f, data.df)
                }
            } else {
                stop("All the paths in your vector must point to a .TXT file.
                     Check the read_dir parameter if you want to read all text
                     files a directory.")
            }
        } else {
            if (!all(dir.exists(paths))) {
                stop("At least one of the directories in your vector does not
                     exist.")
            }
            if (all(endsWith(paths, ".TXT"))) {
                stop("If read_dir is set to TRUE, your vector must contain
                     directory paths and not file paths.")
            }
            for (path_file in paths) {
                for (file in dir(path_file)) {
                  data.df <- readr::read_fwf(paste(path_file, file, sep = ""),
                                             readr::fwf_positions(
                                               sas.df$start, sas.df$end,
                                               sas.df$col_name))
                  # , colClasses=c('character','integer','character','integer',
                  # 'character','integer','character',
                  # 'character','character','character'))
                  data.df_f <- rbind(data.df_f, data.df)
                }
            }
        }
        # select only those rows that match the chosen site
        if (primary_site == FALSE) {
            print(paste(cat(crayon::red("Note:")), " Cancer primary site not
                        supplied, including all sites contained in the files
                        provided."))
        } else {
            data.df_f <- data.df_f[
                          data.df_f$SITERWHO == siteLookUp(primary_site), ]
        }
        # select only those rows that match the chosen year of diagnosis
        # (or range of)
        if (missing(year_dx)) {
            print(paste(cat(crayon::red("Note:")), " Year of diagnosis not
                        supplied, including all years contained in the files
                        provided."))
        } else {
            data.df_f <- data.df_f[data.df_f$YEAR_DX %in% year_dx, ]
        }
    } else {
        # Name columns based on CSV or dict?
        data.df_f <- readr::read_csv(file = paths)
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
    return(subset(sites_recodes, sites == site_name)$recodes)
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