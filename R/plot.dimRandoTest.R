#' Quick & easy option to plot result from dimRandoTest()
#'
#' This creates a bar plot (using base R graphics) showing the main results
#' from a test for successful vignette attribute randomization. Shown are
#' p-values per vignette attribute based on chi-squared tests or one-way
#' ANOVA estimations (see ?dimRandoTest for details). For custom (and
#' publication-quality) graphs, export result as data.frame with as.data.frame().
#'
#' @method plot dimRandoTest
#' @param x dimRandoTest object
#' @param showsig Show vertical line to indicate sign. threshold? (optional, default=T)
#' @param lcol Color of sign. threshold line (optional, changes 'col' argument
#' for abline()).
#' @param ltype Type of sign. threshold line (optional, changes 'lty' argument
#' for abline()).
#' @param siglev Significance threshold (optional, the default is 0.05).
#' @param margins Plot margins (optional; changes par(mar())); must be a numeric
#' vector of length 4.
#' @param ... ignored.
#'
#' @importFrom graphics barplot par abline title
#'
#' @examples
#' \dontrun{
#' result <- dimRandoTest(data=experimentdata,
#' attributes=c("age","nationality","gender"),
#' resvar = "respondent_gender",
#' vartype = "categorical")
#'
#' plot(result)
#' }
#'
#' @export
plot.dimRandoTest <- function(x,showsig=NULL,lcol=NULL,ltype=NULL,siglev=NULL,margins=NULL,...){

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

    # Set parameters
    graphics::par(las=2) # text perpendicular
    graphics::par(mar=margins) # plot margins

    # Output plot
    graphics::barplot(frame$p, names.arg = frame$Attribute, horiz = T,
                      cex.names=0.8, xlim = c(0,1),
                      ylab="",xlab="Chi-squared test p-value")
    if(showsig==T & is.null(frame$Note)){ # adds reference line & caption unless set to F
      graphics::abline(v=siglev, col=lcol, lty = ltype)
      graphics::title(main = unique(frame$respondentvar), cex.sub = .5,
                      sub = paste0("Vertical line indicates ",siglev," significance threshold."))
    }else if(showsig==T & !is.null(frame$Note)){
      graphics::abline(v=siglev, col=lcol, lty = ltype)
      graphics::title(main = unique(frame$respondentvar), cex.sub = .5,
                      sub = paste0("Vertical line indicates ",siglev," significance threshold. Note: some expected freq. in chi-squared test were <5!"))
    }

  }else{
    # Adjust column types
    frame$V4 <- as.integer(frame$V4)
    frame$V5 <- as.numeric(frame$V5)
    frame$V6 <- as.numeric(frame$V6)

    colnames(frame) <- c("vartype","respondentvar","Attribute","df","F value","p")

    # Set parameters
    graphics::par(las=2) # text perpendicular
    graphics::par(mar=c(5,8,4,2)) # wider margin

    # Output plot
    graphics::barplot(frame$p, names.arg = frame$Attribute, horiz = T,
                      cex.names=0.8, xlim = c(0,1),
                      ylab="",xlab="F-test p-value")
    if(showsig==T){ # adds reference line & caption unless set to F
      graphics::abline(v=siglev, col=lcol, lty = ltype)
      graphics::title(main = unique(frame$respondentvar), cex.sub = .5,
                      sub = paste0("Vertical line indicates ",siglev," significance threshold."))


    }
  }
}
