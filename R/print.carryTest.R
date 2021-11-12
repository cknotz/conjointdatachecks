#' Prints results of carryTest() function
#'
#' @method print carryTest
#' @inheritParams carryTest
#' @param x carryTest object
#' @param ... ignored
#'
#' @noRd
#'
#' @export
print.carryTest <- function(x,...){

  # Formatting results data.frame
  out <- as.data.frame(cbind(attributes(x)$row.names,x$V1,x$V2,x$V3,x$V4))
  rownames(out) <- NULL
  colnames(out) <- c("Attribute","F","k","df","p")
  out$Attribute <- format(gsub("`","",out$Attribute),justify="left")
  out$Significance <- " "
  out$Significance <- ifelse(out$p<0.1,"*",out$Significance)
  out$Significance <- ifelse(out$p<0.05,"**",out$Significance)
  out$Significance <- ifelse(out$p<0.01,"***",out$Significance)
  out$Significance <- format(out$Significance, justify = "left")
  out$p <- format.pval(as.numeric(out$p), justify="right",
                       digits = 3, eps = 0.001)
  out$F <- format(as.numeric(out$F),digits=3,nsmall=3)

  # separators
  longest_att <- max(sapply(out$Attribute,function(x){nchar(x)})) # longest attribute
  longest_df <- max(sapply(out$df,function(x){nchar(x)})) # longest attribute

  sep <- c(paste(rep("-",longest_att+2),collapse = ""),
           paste(rep("-",5),collapse = ""),
           paste(rep("-",3),collapse = ""),
           paste(rep("-",longest_df+2),collapse = ""),
           paste(rep("-",7),collapse = ""),
           paste(rep("-",12),collapse = ""))

  out <- rbind(sep,out)

  cat("\n")
  cat(" Test for carryover effects (linear hypothesis F-tests)\n")
  cat("",sep)
  cat("\n")
  print(out,row.names=F, right = F)
  cat("",sep)
  cat("\n")
  cat(" Significance levels: * p<0.1; ** p<0.05; *** p<0.01\n")
  cat(" k: Number of constraints\n df: Degrees of freedom of unconstrained model")
}
