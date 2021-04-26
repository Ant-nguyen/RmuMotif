#' patternCount
#'
#' Given a pattern will count how many times the pattern occurs in a line of text.
#'
#' @param patt A character string
#' @param txt A character string
#' @return The number of times \code{patt} occurs in \code{txt}
#' @examples
#' patternCount("AGC","GTGCAGCTAGC")
#' @export
patternCount <-function(patt,txt){
  pattlocation <-gregexpr(patt,txt)[[1]] # int vector of where each pattern starts
  return(length(pattlocation))
}
