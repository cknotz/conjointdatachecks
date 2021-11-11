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
  # Convert list to data.frame
  frame <- as.data.frame(t(matrix(unlist(x),
                                  nrow = length(x),
                                  byrow = T)))

  if(unique(frame$V1=="categorical")){
    # Adjust column types
    frame$V4 <- as.integer(frame$V4)
    frame$V5 <- as.numeric(frame$V5)
    frame$V6 <- as.numeric(frame$V6)


    if(all(is.na(frame$V7))){

      frame$V7 <- NULL
      # Label columns
      colnames(frame) <- c("vartype","respondentvar","Attribute","df","chi-squared","p")
    }else{
      # Label columns
      colnames(frame) <- c("vartype","respondentvar","Attribute","df","chi-squared","p","Note")
    }

    return(frame)
  }else{
    # Adjust column types
    frame$V4 <- as.integer(frame$V4)
    frame$V5 <- as.numeric(frame$V5)
    frame$V6 <- as.numeric(frame$V6)

    colnames(frame) <- c("vartype","respondentvar","Attribute","df","F value","p")
    return(frame)


  }
}
