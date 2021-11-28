#' Check conjoint data for carryover effects
#'
#' \code{carryTest} checks data from conjoint experiments for carryover effects
#' (which can arise when respondents base their evaluation of one vignette on their evaluation of previous ones).
#' It estimates linear regression models including one vignette attribute/dimension at a time interacted with an
#' ID for the task number as predictors and then estimates an F-test to test for the joint significance of the
#' interaction terms. A rejected null indicates carryover effects are present in the case of a particular
#' vignette attribute (see also Hainmueller et al. 2014, Political Analysis, p. 22).
#'
#' Important: This test should only be run on vignette attributes that are unconstrained.
#' Please see the package vignettes (browseVignettes("conjointdatachecks")) for details.
#'
#' The result can be plotted (using plot()) or exported as a tidy data.frame
#' (using as.data.frame()).
#'
#' @param data A data.frame (in long format).
#' @param outcome The outcome variable, rating or choice, entered as a string.
#' @param attributes A character vector of vignette attributes/dimensions.
#' @param task The task- or contest-ID; should be a factor in the data, entered here as a string.
#' @param resID The respondent ID; should be a factor in the data, entered here as a string.
#' @return A list of class 'carryTest'. Can be converted to tidy data.frame with as.data.frame().
#' @examples
#' \dontrun{
#' carryTest(data=experimentdata,
#' attributes=c("gender","age","income","education"),
#' outcome="rating",
#' task="taskID",
#' resID="respondent_ID")
#' }
#'
#' @importFrom stats as.formula lm model.frame pf resid vcov
#' @importFrom clubSandwich vcovCR Wald_test constrain_zero
#'
#' @export
carryTest <- function(data, outcome, attributes, task, resID) {

  waldres <- as.data.frame(t(invisible(sapply(attributes,function(x){

    # Constructs model equation
    equation <- stats::as.formula(paste(outcome,
                                 paste(
                                   paste(x, task, sep = "*"), collapse = " + "
                                 ),
                                 sep = " ~ "))

    # Runs model
    mod <- stats::lm(formula = equation, data = noquote(data))

    # Identifies interaction terms
    terms <- grep(pattern = ":", colnames(stats::vcov(mod)), value = F)

    #Adjusts VCOV
    mod_V <- clubSandwich::vcovCR(mod,
                    cluster = data[[resID]],
                    type = "CR1S")

    #Runs Wald-test on model results
    testres <- clubSandwich::Wald_test(mod,
                         vcov = mod_V,
                         constraints = clubSandwich::constrain_zero(terms),
                         test = "Naive-F")

    rm(mod,mod_V)

    # Ready for export
    fstat <- testres$Fstat
    s <- testres$df_num
    df_full <- testres$df_denom
    pval <- testres$p_val
    rm(testres)

    res <- c(fstat,s,df_full,pval)

  }))))
  class(waldres) <- "carryTest"
  return(waldres)
}

