#' Find features with a single valeu for all rows
#'
#' Some features may have the same value for all rows depending on the way you
#' perform a subset. In these cases, this feature is not adding information to
#' the analysis and therefore should be removed. This function helps you to
#' find such features and provides you also the unique value of such a feature.
#'
#' You can provide the return$`Unique value` of this function as the argument
#' to the \code{removeFullNAFeatures} function for even if an unique value for
#' a feature is also a value in another feature, it will only be removed if it
#' is the only valeu for all rows for this specific feature.
#'
#' @param dataframe The dataframe with SEER data
#'
#' @export
findSingleValueFeatures <- function(dataframe) {
  features <- dataframe[sapply(dataframe, function(x) length(unique(x)) == 1)]
  unique_value <- sapply(dataframe[, names(features)], function(x) unique(x))
  result_table <- cbind(names(features), unique_value)
  rownames(result_table) <- NULL
  colnames(result_table) <- c("Feature name", "Unique value")
  result_table <- as.data.frame(result_table, stringsAsFactors = FALSE)
  return(result_table)
}
