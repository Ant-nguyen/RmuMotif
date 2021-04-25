#' Takes the most common nucleotide in each position of a group motifs
#' @param Motifs A vector of character string of the DNA Motifs.
#' @return A vector of the most common nucleotide in each position
#' @export
consensus <- function(Motifs){
  profile <- muCount(Motifs)
  conStrand <- vector(mode='character')
  for (i in 1:nchar(Motifs[[1]])) {
    conStrand <- c(conStrand,names(which.max(profile[,i])))
  }
  return(conStrand)
}
