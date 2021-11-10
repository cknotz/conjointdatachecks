#' Quick & easy option to plot result from dimRandoTest() as heatmap
#'
#' @method plot dimRandoTest
#' @param x dimRandoTest object
#' @param margins Width of margins for labels. As margins parameter for heatmap().
#' @param cexRow Size of row labels. As cexRow parameter for heatmap().
#' @param cexCol Size of column labels. As cexCol parameter for heatmap().
#' @param col Color scheme. As col parameter for heatmap().
#' @param ... ignored.
#'
#' @importFrom stats heatmap
#' @importFrom grDevices gray.colors
#'
#' @examples
#' \dontrun{
#' result <- dimRandoTest(data=experimentdata,
#' dims=c("age","nationality","gender"),
#' labels=c("Age","Nationality","Gender"))
#'
#' plot(result)
#' }
#'
#' @export
plot.dimRandoTest <- function(x,margins=NULL,cexRow=NULL,cexCol=NULL,col=NULL,...){

  # Set default values for margins, cexRow, & cexCol
  if(is.null(margins)){
    margins <- c(7,7)
  }
  if(is.null(cexRow)){
    cexRow <- .8
  }
  if(is.null(cexCol)){
    cexCol <- .8
  }

  # Set default color palette
  if(is.null(col)){
    col <- grDevices::gray.colors(n = length(x),
                       start = 0.05, end = 0.95, rev = T)
  }

  # Clean up - convert back to data.frame format
  frame <- matrix(unlist(x),nrow = length(x),byrow = T)

  # extract labels, separate results to frame
  labels <- frame[nrow(frame),]
  frame <- frame[1:(nrow(frame)-1),]

  # apply labels
  dimnames(frame) <- list(labels,labels)

  # extract Cramer's V stats <- hacky, redo if possible
  frame <- as.data.frame(frame)
  crams <- as.data.frame(sapply(frame,function(x){as.numeric(gsub("\\D+","", x))}/100))
  row.names(crams) <- labels
  crams <- as.matrix(crams)

  stats::heatmap(crams,Rowv = NA, Colv = NA,
          margins = margins,col = col,
          cexRow = cexRow, cexCol = cexCol)
}
