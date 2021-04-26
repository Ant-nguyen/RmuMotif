#' With a profile, calculate most probable kmer in a specific sequence(txt)
#' @param txt A character string of a DNA sequence that one wants to know most probable kmer
#' @param k A numeric value of the length of a kmer
#' @param profile A matrix of a profile that will determine most probable kmer
#' @return A character string of the DNA kmer most probable
#' @examples profileMostProbable("GTCGTCA",3,sampleProfile)
#' @export
profileMostProbable <- function(txt,k,profile){
  num <- nchar(txt)
  bestscore <- -1
  for(i in 1:(num-k+1)){
    x <- substr(txt,i,(i+k-1))
    if(pr(x,profile) > bestscore){
      bestscore <- pr(x,profile)
      mostprob <- x
    }
  }
  return(mostprob)
}
