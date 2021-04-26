#' GreedyMotifSearch
#'
#' Given a series of DNA sequences this function will return the most probably motif in each sequence taking into account
#' the other DNA sequences.This uses a 'Greedy" method that is very computationally simple allowing for quick fast results.
#'
#' @param DNAseqs A vector of character strings representing a series of DNA sequences
#' @param k An integer Value representing how long of kmer ins interested
#' @param t An integer value representing how many of the DNAseqs one wish to incorporate
#' @param pseudo A TRUE or FALSE condition, default FALSE. If TRUE will implement pseudo counts which alleviates biases
#' against some single point mutations
#'
#' @return A vector of best scoring Motifs
#'
#' @examples GreedyMotifSearch(sampleDNAseq,3,4)
#' @export
GreedyMotifSearch <- function(DNAseqs,k,t,pseudo=FALSE){
  BestMotifs <- c()
  for(i in DNAseqs){
    BestMotifs <-c(BestMotifs,substr(i,1,k))
  }
  n <- nchar(DNAseqs[1])
  for(j in 1:(n-k+1)){
    Motifs <- substr(DNAseqs[1],j,(j+k-1))
    for(m in 2:t){
      p <- muProfile(Motifs,pseudo=pseudo)
      Motifs <- c(Motifs,profileMostProbable(DNAseqs[m],k,p))
    }
    if(muScore(Motifs)< muScore(BestMotifs)){
      BestMotifs <- Motifs
    }
  }
  return(BestMotifs)
}
