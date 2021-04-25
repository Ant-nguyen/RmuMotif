#' This function gives a score of how the consensus nucleotide compares to the other in a motif.
#' @param Motifs A vector of character string of the DNA Motifs.
#' @return A numeric value representing a score of the consensus based on Motifs.
#' The lower the score the stronger the consensus compares to Motifs
#' @export
muScore <- function(Motifs){
  count <- muCount(Motifs)
  score <- sum(count)
  for (i in 1:nchar(Motifs[[1]])) {
    score<- (score - max(count[,i]))
  }
  return(score)
}
