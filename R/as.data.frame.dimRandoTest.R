#' Converts result from dimRandoTest() to tidy data.frame
#'
#' @method as.data.frame dimRandoTest
#' @inheritParams dimRandoTest
#' @param x dimRandoTest object
#' @param ... ignored
#'
#' @noRd
#'
#' @export
as.data.frame.dimRandoTest <- function(x,...){

  # Clean up - convert back to data.frame format
  frame <- matrix(unlist(x),nrow = length(x),byrow = T)

  # extract labels, separate results to frame
  labels <- frame[nrow(frame),]
  frame <- frame[1:(nrow(frame)-1),]

  # apply labels
  dimnames(frame) <- list(labels,labels)

  frame[upper.tri(frame)] <- NA # sets redundant entries to NA

  # final convert & return
  frame <- as.data.frame(frame)
  return(frame)
}
