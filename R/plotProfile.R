#' plot.profile
#'
#' Method for generating sequence logo graphical representation for profile class objects.
#'
#' @param profile A profile class Matrix.
#' @param prob A TRUE or FALSE argument. Default TRUE, will present visualization based on
#' probability. If FALSE will present based on weight of each sequence.
#' @return A ggseqlogo generated sequence logo plot.
#' @seealso [muMotif::muProfile()] creates profile class matrix
#' @importFrom ggplot2 ggplot aes geom_col xlab ylab
#' @importFrom ggseqlogo ggseqlogo
#' @export
#' @examples
#' plot(sampleProfile)
plot.profile <- function(profile,prob=TRUE){
  bit <- ggseqlogo(profile)
  pro <- ggseqlogo(profile,method = 'prob')
  if (prob == TRUE){
    return(pro)
  }
  else{
    return(bit)
  }
}
