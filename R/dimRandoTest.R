#' Checks if vignette attributes are uncorrelated
#'
#' \code{dimRandoTest} checks data from conjoint experiments for randomization
#' problems by checking for associations between any pair of profile attributes.
#' it cross-tabulates all combinations of profile attributes and computes chi-squared
#' tests to test if there are any significant associations between these attributes.
#' A rejected null (small p-value) indicates that there is some statistically
#' significant association between profile attributes. A Cramer's V test statistic
#' is also provided for each attribute combination to indicate the strength of any
#' association between the attributes. The output can be labeled.
#'
#' @param data A data.frame.
#' @param dims A character vector of vignette attributes' names in the dataset.
#' @param labels Optional. A character vector of labels for the vignette attributes
#' (Important: the order of the labels must correspond to the order of attributes in dims!).
#' @return A list of class 'dimRandoTest'.
#' @examples
#' \dontrun{dimRandoTest(data=experimentdata,dims=c("age","nationality",
#' "gender"),labels=c("Age","Nationality","Gender"))}
#'
#' @importFrom stats chisq.test
#'
#' @export
dimRandoTest <- function(data,dims,labels=NULL){
  cramtest <- as.list(invisible(lapply(gsub("`","",dims),function(y){
    invisible(lapply(gsub("`","",dims),function(x){

      # Runs chi-squared test
      chitest <- chisq.test(table(data[[x]],data[[y]]))

      p <- chitest$p.value # Chi-sq p-value
      sig <- " "
      sig <- ifelse(chitest$p.value<0.1,"*",sig)
      sig <- ifelse(chitest$p.value<0.05,"**",sig)
      sig <- ifelse(chitest$p.value<0.01,"***",sig)
      chi <- chitest$statistic # test statistic
      df <- chitest$parameter # df
      n <- sum(chitest$observed) # N
      mindim <- min(c(length(attributes(chitest$observed)$dimnames[[1]])-1, # min of i-1,j-1
                      length(attributes(chitest$observed)$dimnames[[2]])-1))
      cramv <- format(round(sqrt(chi/(n*mindim)),digits=2),nsmall=2) # Cramer's V


      as.list(paste0(cramv," ",sig))
    }))
  })))

  class(cramtest) <- "dimRandoTest"

  # Add labels
  if(!is.null(labels)){
    cramtest$labels <- labels
  }else{
    cramtest$labels <- gsub("`","",dims)
  }

  return(cramtest)
}
