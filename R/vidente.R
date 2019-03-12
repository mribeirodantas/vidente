#' vidente: A package for parsing and analyzing SEER data.
#'
#' The vidente package provides two categories of important functions.
#' Functions to parse SEER data and functions to analyze it.
#' 
#' @section Parsing data:
#'  The \link{buildSEERParser} function builds parsing instructions based on the
#'  instructions in the downloaded folder (.sas file) or in the dictionary file
#'  exported from SEER*Stat software.
#'
#'  The \link{readSEER} function reads the SEER data from ASCII text files
#'  downloaded from SEER website or exported from SEER*Stat software based on
#'  the instructions provided in the dictionary (.dic) or .sas file.
#'  
#'  The \link{listPrimarySites} function provides a list of keywords recognized
#'  recognized as primary site names in the terminology adopted by SEER so that
#'  you know what primary sites you can provide as the primary_site parameter
#'  for the readSEER function. 
#'  
#' @section Analyzing data:
#'  The \link{histNA} function plots a histogram of the proportion of NA
#'  values for every feature in the dataframe.
#'  
#'  The \link{removeFullNAFeatures} function removes features whose all values
#'  are NA (or along with some additional NA value such as "Blank(s)", as some
#'  datasets exported from SEER*Stat software).
#'
#' @docType package
#' @name vidente
NULL
