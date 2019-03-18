#' Remove full-NA value features in a SEER dataframe
#' 
#' Some features in a SEER dataframe are specific to certain types of cancer.
#' If you're analyzing only patients with breast cancer, features that have
#' information on prostate cancer, for example, will have only NA values. These
#' features are useless to your analysis and should be removed. \code{plotHistNA}
#' is a good function to plot this scenario so that you know if you have
#' features with all their rows as NA values. By making use of
#' removeFullNAFeatures you can get rid of these features.
#' 
#' @param dataframe The dataframe with SEER data
#' @param additional_na A vector with additional symbol(s) that should also be
#'   considered NA. This is important for some datasets exported from SEER*Stat
#'   software that come with NA values and also strings 'Blank(s)' representing
#'   also lack of values.
#'
#' @export
removeFullNAFeatures <- function(dataframe, additional_na) {
  # Full NA values are always removed
  features_full_na <- which(colMeans(is.na(dataframe)) == 1)
  if (length(features_full_na) > 0) {
    dataframe <- dataframe[, -features_full_na]
  }

  if (!missing(additional_na)) {
    for (term in additional_na) {
      features_full_na_add <- which(colMeans(dataframe == term) == 1)
      if (length(features_full_na_add) > 0) {
        dataframe <- dataframe[, -features_full_na_add]
      }
    }
  }

  return(dataframe)
}
