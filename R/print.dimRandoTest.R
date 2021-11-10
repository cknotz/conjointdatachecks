#' Prints results of dimRandoTest() function
#'
#' @method print dimRandoTest
#' @inheritParams dimRandoTest
#' @param x dimRandoTest object
#' @param ... ignored
#'
#' @noRd
#'
#' @export
print.dimRandoTest <- function(x,...){

  # Clean up - convert back to data.frame format
  frame <- matrix(unlist(x),nrow = length(x),byrow = T)

  # extract labels, separate results to frame
  labels <- frame[nrow(frame),]
  frame <- frame[1:(nrow(frame)-1),]

  # apply labels
  dimnames(frame) <- list(labels,labels)

  frame[upper.tri(frame)] <- NA # sets redundant entries to NA

  rownames(frame) <- NULL
  frame <- cbind(labels,frame)

  # construct separator
  longest_lab <- max(sapply(labels,function(x){nchar(x)}))
  lengths_lab <- sapply(labels,function(x){nchar(x)})
  lengths_lab[which(lengths_lab<8)] <- 8 # to avoid gaps; cell content has length 8

  attributes(lengths_lab) <- NULL
  sep <- c(paste(rep("-",longest_lab),collapse = ""),sapply(lengths_lab,function(x){
    paste(rep("-",x),collapse = "")
  }))

  # Add separators to matrix
  frame <- rbind(c(paste(rep("-",longest_lab),collapse = ""),sapply(lengths_lab,function(x){
    paste(rep("-",x),collapse = "")
  })),frame,c(paste(rep("-",longest_lab),collapse = ""),sapply(lengths_lab,function(x){
    paste(rep("-",x),collapse = "")
  })))

  # Add empty row at bottom
  frame <- rbind(frame,rep(" ",length(ncol(frame))))

  # Apply labels
  collabs <- c(" ",labels)

  # Print
  cat("\n")
  cat(" Test for successful randomization of vignette attributes\n")
  cat("",sep,"\n")
  prmatrix(frame, collab = collabs, rowlab = rep_len("", nrow(frame)),
           na.print = " ", quote = F)
  cat(" Numbers in cells are Cramer's V statistics.\n Significance levels: * p<0.1; ** p<0.05; *** p<0.01\n Significances based on chi-squared tests.")
}
