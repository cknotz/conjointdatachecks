#' Produces simple bar plot of carryTest() result
#'
#' @method plot carryTest
#' @param x carryTest object
#' @param showsig Show vertical line indicating sign. threshold (default=TRUE)?
#' @param lcol Color of sign. threshold line (corresponds to 'col' argument for abline()).
#' @param ltype Type of sign. threshold line (corresponds to 'lty' argument for abline()).
#' @param siglev Significance threshold; the default is 0.05.
#' @param ... ignored
#'
#' @importFrom graphics barplot par abline
#'
#' @examples
#' \dontrun{
#' result <- carryTest(data=experimentdata, attributes=c("gender","age","income",
#' "education"),outcome="rating",task="taskID")
#'
#' plot(result, lcol = "red", ltype=5)
#' }
#'
#' @export
plot.carryTest <- function(x,lcol=NULL,ltype=NULL,siglev=NULL,showsig=NULL,...){

  # Set sign. line as default T
  if(is.null(showsig)){
    showsig <- T
  }

  # set default line color
  if(is.null(lcol)){
    lcol <- "black"
  }

  # set default line type
  if(is.null(ltype)){
    ltype <- 5
  }

  # set default significance level
  if(is.null(siglev)){
    siglev <- 0.05
  }

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

  # Set plot parameters (from statsmethods.net)
  graphics::par(las=2) # text perpendicular
  graphics::par(mar=c(5,8,4,2)) # wider margin

  # Output plot
  graphics::barplot(frame$p, names.arg = frame$Attribute, horiz = T,
          cex.names=0.8, xlim = c(0,1),
          ylab="",xlab="F-test p-value")
  if(showsig==T){ # adds reference line unless set to F
    graphics::abline(v=siglev, col=lcol, lty = ltype)
  }
}
