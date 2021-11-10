#' Converts result of carryTest() to tidy data.frame
#'
#' @method as.data.frame carryTest
#' @inheritParams carryTest
#' @param x carryTest object
#' @param ... ignored
#'
#' @noRd
#'
#' @export
as.data.frame.carryTest <- function(x,...){
  # Covert list to data.frame
  frame <- data.frame(t(matrix(unlist(x), nrow = length(x), byrow = T)))

  # Add attribute labels
  frame <- cbind(gsub("`","",attributes(x)$row.names),frame)

  # Add column names
  colnames(frame) <- c("Attribute","F","k","df","p")

  # Convert to numeric
  frame$F <- as.numeric(frame$F)
  frame$k <- as.numeric(frame$k)
  frame$df <- as.numeric(frame$df)
  frame$p <- as.numeric(frame$p)

  # Return result
  return(frame)
}
