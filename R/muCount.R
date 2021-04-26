#' muCount
#'
#' A function that takes a series of motifs and return a count of each AGTC at each position.
#'
#' @param Motifs vector of motifs of interest
#' @param pseudo TRUE or FALSE statement, if pseudo is true than will implement pseudo counts.
#' @return A matrix with labeled Nucleotide
#' @examples muCount(sampleMotifs,pseudo=TRUE)
#' @export
muCount <- function(Motifs,pseudo=FALSE){
  k <-nchar(Motifs[1])
  count <- genematrix(k,pseudo = pseudo)
  for(i in Motifs){
    nuc <- strsplit(i,"")[[1]]
    for(j in 1:k){
      count[nuc[j],j]<- count[nuc[j],j]+1
    }
  }
  return(count)
}

#simple function that makes empty Matrix with AGTC row titles,
#if pseudo is on all cell in matrix start with 1 instead of 0
genematrix <- function(k,pseudo=FALSE){
  gmatrix <- matrix(0,nrow = 4,ncol = k)
  rownames(gmatrix) <- c("A","C","G","T")
  if(pseudo==TRUE){
    gmatrix <- gmatrix + 1
  }
  return(gmatrix)
}
