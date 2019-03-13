#' Plots a histogram on the proportion of NA
#' 
#' Plots a histogram on the proportion of missing values (NA) for all the
#' features in the SEER dataframe.
#' 
#' @param dataframe The dataframe with SEER data
#' @param summary If set to TRUE, the function will print a summary on the NA
#'   values apart from the histogram.
#' @param additional_na A vector with additional symbol(s) that also should be
#'   considered NA. This is important for some datasets exported from SEER*Stat
#'   software that come with NA values and also strings 'Blank(s)' representing
#'   also lack of values.
#' @param binwidth This parameter is automatically set by ggplot2. You can set
#'   it to a specific number if you want.
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
#'                       primary_site = 'Breast')
#' # Plot the histogram
#' histNA(seer_data, summary=TRUE)}
#' @import ggplot2 scales
#' @export
histNA <- function(dataframe, summary=FALSE, additional_na, binwidth) {
  options(warn = -1)
  # Convert additional NAs to default NA symbol
  if (!missing(additional_na)) {
    title <- "Proportion of missing values in features"
    for (i in seq_along(dataframe)) {
      dataframe[[i]][dataframe[[i]] %in% additional_na] <- NA
    }
    # The amount of missing values calculation could be replaced by:
    # {
    #   is.something <- function(obj, values){
    #   df <- sapply(obj, function(x) x %in% values)
    #   return(df)
    # }
    # But surprisingly the for + is.na version is faster
  } else {
    title <- "Proportion of NA values in features"
  }
  # Full NA values are always removed
  amount_of_na <- colMeans(is.na(dataframe))

  if (!missing(binwidth)) {
    seer_hist <- qplot(amount_of_na,
                       geom = "histogram",
                       binwidth = binwidth,
                       xlab = "Proportion of NA",
                       ylab = "Amount of features",
                       alpha = I(.2),
                       fill = I("blue"),
                       main = title) +
                 scale_x_continuous(labels = scales::percent)
  } else {
    seer_hist <- qplot(amount_of_na,
                       geom = "histogram",
                       xlab = "Proportion of NA",
                       ylab = "Amount of features",
                       alpha = I(.2),
                       fill = I("blue"),
                       main = title) +
                 scale_x_continuous(labels = scales::percent)
  }
  if (summary == TRUE) {
    x <- table(amount_of_na)
    x <- as.data.frame(x)
    rownames(x) <- NULL
    colnames(x) <- c("Proportion", "Number of features")
    print(x)
    print(summary(amount_of_na))
  }
  return(seer_hist)
}
