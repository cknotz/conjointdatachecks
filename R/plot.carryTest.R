#' Produces simple bar plot of carryTest() result
#'
#' This creates a bar plot (using base R graphics) showing the main results
#' from a test for carryover effects: p-values per vignette attribute. For
#' custom (and publication-quality) graphs, export result as data.frame with
#' as.data.frame().
#'
#' @method plot carryTest
#' @param x carryTest object
#' @param showsig Show vertical line to indicate sign. threshold? (optional,
#' default=T)
#' @param lcol Color of sign. threshold line (optional, changes 'col' argument
#' for abline()).
#' @param ltype Type of sign. threshold line (optional, changes 'lty' argument
#' for abline()).
#' @param siglev Significance threshold (optional, the default is 0.05).
#' @param margins Plot margins (optional; changes par(mar())); must be a numeric
#' vector of length 4.
#' @param ... ignored
#'
#' @importFrom graphics barplot par abline title
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
plot.carryTest <- function(x,showsig=NULL,lcol=NULL,ltype=NULL,siglev=NULL,margins=NULL,...){

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

  # Set default plot margins
  if(is.null(margins)){
    margins <- c(2,8,2,2)
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
  graphics::par(mar=margins) # plot margins

  # Output plot
  graphics::barplot(frame$p, names.arg = frame$Attribute, horiz = T,
          cex.names=0.8, xlim = c(0,1),
          ylab="",xlab="F-test p-value")
  if(showsig==T){ # adds reference line & caption unless set to F
    graphics::abline(v=siglev, col=lcol, lty = ltype)
    graphics::title(main = "", cex.sub = .5,
                    sub = paste0("Vertical line indicates ",siglev," significance threshold."))
  }
}
