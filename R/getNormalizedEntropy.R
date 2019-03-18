#' Gets the normalized entropy for features in a data frame
#' 
#' Gets the normalized entropy for every feature in a data frame.
#' 
#' @param dataframe The dataframe with SEER data.
#'
#' @import infotheo
#' @export
getNormalizedEntropy <- function(dataframe) {
  features_entropy <- apply(dataframe, 2, entropy)
  features_log <- apply(dataframe, 2, function(x) log(length(unique(x))))
  
  return(features_entropy/features_log)
}