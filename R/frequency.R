#' Extract frequency of specific kmers (snippets of specific length) from a larger sequence.
#' @param k An Integer representing length of kmer
#' @param txt A character string of DNA sequence
#' @return A List of frequency of kmers in txt sequence, Class of object is frequencyMap allowing
#' for specifc plot method.
#' @export
frequencyMap <- function(k,txt){ # k is an integer value representing a length of sequence (kmer)
  freq <- list()
  n <- nchar(txt)-2
  for(i in 1:n){
    x <- (k+i-1)
    pattern <- substr(txt,i,x)
    freq[[pattern]] <- patternCount(pattern,txt)
  }
  class(freq) <-"frequencyMap"  #classify end result to frequencyMap, allowing specific plotting
  return(freq)
}


pr<- function(txt,profile){
  p <- 1
  txtstr <- strsplit(txt,"")[[1]]
  for(i in 1:length(txtstr)){
    p <- profile[txtstr[i],i]*p
  }
  return(as.numeric(p))
}

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
