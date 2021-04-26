#' From a profile return the probability a DNA sequence fits from the profile
#' @param txt A character string reprenting the DNA sequence in question.
#' @param profile A matrix representing the probability of each nucleuotide in specific locations
#' @return A number value of the probability of the txt sequence based on the profile matrix
#' @examples pr("ACTG",sampleProfile)
#' @export
pr<- function(txt,profile){
  p <- 1
  txtstr <- strsplit(txt,"")[[1]]
  for(i in 1:length(txtstr)){
    p <- profile[txtstr[i],i]*p
  }
  return(as.numeric(p))
}
