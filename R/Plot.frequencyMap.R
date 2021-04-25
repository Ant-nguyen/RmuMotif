#' Method for generic plot function that visualize different kmer frequency
#' @param frequencyMap An object with the frequencyMap class (a List)
#' @return a ggplot bargraph of the different kmers and their frequency
#' @examples plot(sampleFrequency)
#' @importFrom ggplot2 ggplot aes geom_col xlab ylab
#' @export
plot.frequencyMap <- function(frequencyMap){
  library(ggplot2)
  datas <- unlist(frequencyMap)
  dfmap <- data.frame(datas)
  ggplot(dfmap,aes(rownames(dfmap),datas))+geom_col()+xlab("kmer sequence")+ylab("Frequency")
}
