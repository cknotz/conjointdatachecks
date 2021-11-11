#' Check conjoint data for carryover effects
#'
#' \code{carryTest} checks data from conjoint experiments for carryover effects
#' (which can arise when respondents base their evaluation of one vignette on their evaluation of previous ones).
#' It estimates linear regression models including one vignette attribute/dimension at a time interacted with an
#' ID for the task number as predictors and then estimates an F-test to test for the joint significance of the
#' interaction terms. A rejected null indicates carryover effects are present in the case of a particular
#' vignette attribute (see also Hainmueller et al. 2014, Political Analysis, p. 22).
#'
#' The result can be plotted (using plot()) or exported as a tidy data.frame
#' (using as.data.frame()).
#'
#' @param data A data.frame.
#' @param outcome The outcome variable, rating or choice, entered as a string.
#' @param attributes A character vector of vignette attributes/dimensions.
#' @param task The task- or contest-ID; should be a factor in the data, entered here as a string.
#' @return A list of class 'carryTest'. Can be converted to tidy data.frame with as.data.frame().
#' @examples
#' \dontrun{
#' carryTest(data=experimentdata,
#' attributes=c("gender","age","income","education"),
#' outcome="rating",
#' task="taskID")
#' }
#'
#' @importFrom stats as.formula lm model.frame pf resid
#'
#' @export
carryTest <- function(data, outcome, attributes, task) {

  # This computes F-tests per vignette attribute
  res <- as.data.frame(t(invisible(sapply(attributes,function(x){

    # Constructs equation for full model
    equation <- as.formula(paste(outcome,
                                 paste(
                                   paste(x, task, sep = "*"), collapse = " + "
                                 ),
                                 sep = " ~ "))

    # Runs model & stores info
    mod <- lm(formula = equation, data = noquote(data))
    urss <- sum(resid(mod)^2)
    df_full <- mod$df

    # Constructs equation for reduced model
    equation <- as.formula(paste(outcome,
                                 paste(x,task,sep = " + "),sep = " ~ "))

    # Runs reduced model & stores info
    red <- lm(formula = equation, data = model.frame(mod))
    rrss <- sum(resid(red)^2)
    df_red <- red$df

    s <- df_red-df_full # restrictions

    # F-test
    fstat <- ((rrss-urss)/s)/(urss/(df_full))
    fstat

    pval <- format.pval(pf(fstat,s,df_full,lower.tail = F),
                        eps = .001, digits = 4)

    res <- c(fstat,s,df_full,pval)
  }))))

  # Final output
  class(res) <- "carryTest"
  return(res)
}

