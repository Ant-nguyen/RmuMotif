#' Generates a probability profile based on a series of sequences 'Motifs'
#' @param Motifs Vector of character DNA sequences
#' @param pseudo TRUE or FALSE condition, default FALSE. If TRUE will implement pseudo counts which alleviates biases
#' against some single point mutations
#' @return A profile class object with
#' @export
muProfile <- function(Motifs,pseudo=FALSE){
  if (pseudo== FALSE){
    counts <- muCount(Motifs)
  }
  else{
    counts <- muCount(Motifs,pseudo=TRUE)
  }
  n <- length(Motifs)
  profile <- (counts/n)
  class(profile) <- "profile" #classification to allow for specific plot method
  return(profile)
}
