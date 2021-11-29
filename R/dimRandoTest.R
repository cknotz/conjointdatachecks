#' Checks if vignette attributes are uncorrelated
#'
#' \code{dimRandoTest} checks data from conjoint experiments for randomization
#' problems by checking for associations between vignette attributes and
#' respondent-level variables.
#'
#' If the respondent-level variable is categorical (e.g., gender), the function
#' cross-tabulates each vignette attribute with the respondent-level variable,
#' computes a chi-squared test, and compiles the results.
#'
#' If the respondent-level variable is metric (e.g., income), the function runs
#' a one-way ANOVA with the respondent-level variable as the dependent and each
#' vignette attribute as the independent variable and compiles the results.
#'
#' Users need to specify whether their respondent-level variable is metric or
#' categorical.
#'
#' Important: If the experiment involved multiple rating or choice rounds per respondent,
#' then the tests should be run separately for each round.
#'
#' In both cases, a rejected null (small p-value) indicates
#' that there is some statistically significant association between a given
#' profile attribute and the respondent variable, which in turn signals that
#' there might have been issues with the random assignment of vignettes to
#' respondents.
#'
#' The result can be plotted (using plot()) or exported as a tidy data.frame
#' (using as.data.frame()).
#'
#' @param data A data.frame (in long format).
#' @param attributes A character vector of vignette attributes' names in the dataset.
#' @param resvar A respondent-level variable.
#' @param vartype The type (measurement level) of the respondent variable.
#' ("metric" or "categorical"). If vartype is "categorical", the variable
#' needs to be stored as a factor in the dataset.
#' @return A list of class 'dimRandoTest'. Can be converted to a tidy data.frame
#' with as.data.frame().
#' @examples
#' \dontrun{
#' dimRandoTest(data=experimentdata[which(experimentdata$round==1),],
#' attributes=c("age","nationality","gender"),
#' resvar = "respondent_gender",
#' vartype = "categorical")
#'
#' dimRandoTest(data=experimentdata[which(experimentdata$round==1),],
#' attributes=c("age","nationality","gender"),
#' resvar = "respondent_income",
#' vartype = "metric")
#' }
#'
#' @importFrom stats chisq.test aov
#'
#' @export
dimRandoTest <- function(data,attributes,resvar,vartype){

  if(vartype=="metric"){

    # Set up data.frame for results
    results <- data.frame(vartype=rep("metric",length(attributes)),
                          Resvar=rep(resvar,length(attributes)),
                          Attribute = attributes,
                          df = rep(NA,length(attributes)),
                          F.value = rep(NA,length(attributes)),
                          p = rep(NA,length(attributes)))

    # Set up loop
    for(x in attributes){

      # Set up model equation
      equation <- as.formula(paste(resvar,x,sep = " ~ "))

      # Estimate
      model <- summary(stats::aov(equation, data = data))

      # Extract results
      results$df[which(results$Attribute==x)] <- model[[1]][["Df"]][1]
      results$F.value[which(results$Attribute==x)] <- model[[1]][["F value"]][1]
      results$p[which(results$Attribute==x)] <- model[[1]][["Pr(>F)"]][1]

    }

    # clean attribute strings
    results$Attribute <- gsub("`","",results$Attribute)

    class(results) <- "dimRandoTest"
    return(results)

  }else if(vartype=="categorical") {

    # Set up results data.frame
    results <- data.frame(vartype=rep("categorical",length(attributes)),
                          Resvar=rep(resvar,length(attributes)),
                          Attribute = attributes,
                          df = rep(NA,length(attributes)),
                          chi = rep(NA,length(attributes)),
                          p = rep(NA,length(attributes)),
                          note = rep(NA, length(attributes)))

    for(x in attributes){

      # Run test
      res <- stats::chisq.test(table(data[[resvar]],data[[gsub("`","",x)]]),
                               correct = F)

      # Record results
      results$chi[which(results$Attribute==x)] <- res$statistic
      results$p[which(results$Attribute==x)] <- res$p.value
      results$df[which(results$Attribute==x)] <- res$parameter
      if(min(res$expected)<5){
        results$note[which(results$Attribute==x)] <- "Caution: Minimum of exp. values <5!"
      }
    }

    # Clean attribute strings
    results$Attribute <- gsub("`","",results$Attribute)

    class(results) <- "dimRandoTest"
    return(results)

  }
}
