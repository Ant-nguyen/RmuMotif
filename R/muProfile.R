#' muProfile
#'
#' Generates a probability profile based on a series of sequences 'Motifs'
#'
#' @param Motifs Vector of character DNA sequences
#' @param pseudo TRUE or FALSE condition, default FALSE. If TRUE will implement pseudo counts which alleviates biases
#' against some single point mutations
#' @return A profile class object with
#' @examples muProfile(sampleMotif)
#' @export
muProfile <- function(Motifs,pseudo=FALSE){
  if (pseudo== FALSE){
    counts <- muCount(Motifs)
    n <- length(Motifs)
  }
  else{
    counts <- muCount(Motifs,pseudo=TRUE)
    n <- length(Motifs)+4
  }
  profile <- (counts/n)
  class(profile) <- "profile" #classification to allow for specific plot method
  return(profile)
}
