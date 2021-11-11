#' Prints results of dimRandoTest() function
#'
#' @method print dimRandoTest
#' @inheritParams dimRandoTest
#' @param x dimRandoTest object
#' @param ... ignored
#'
#' @noRd
#'
#' @export
print.dimRandoTest <- function(x,...){

  # Convert list to data.frame
  frame <- as.data.frame(t(matrix(unlist(x),
                                  nrow = length(x),
                                  byrow = T)))

  if(unique(frame$V1)=="metric"){
    frame$V1 <- NULL # vartype is known
    frame$V4 <- as.integer(frame$V4)
    frame$Significance <- rep(" ", length(frame$V2))
    frame$Significance <- ifelse(frame$V5<0.1,"*",frame$Significance)
    frame$Significance <- ifelse(frame$V5<0.05,"**",frame$Significance)
    frame$Significance <- ifelse(frame$V5<0.01,"***",frame$Significance)
    frame$V5 <- format(round(as.numeric(frame$V5), digits = 3),nsmall=3)
    frame$V6 <- format(round(as.numeric(frame$V6), digits = 3),nsmall=3)

    # Extract respondent var:
    respvar <- unique(frame$V2)
    frame$V2 <- NULL

    # Label columns
    colnames(frame) <- c("Attribute","df","F value","p","Significance")

    # separators
    longest_att <- max(sapply(frame$Attribute,function(x){nchar(x)})) # longest attribute
    longest_df <- max(sapply(frame$df,function(x){nchar(x)})) # longest attribute

    sep <- c(paste(rep("-",longest_att),collapse = ""),
             paste(rep("-",longest_df+2),collapse = ""),
             paste(rep("-",8),collapse = ""),
             paste(rep("-",6),collapse = ""),
             paste(rep("-",12),collapse = ""))

    frame <- rbind(sep,frame,sep)

    cat(paste0(" Test for randomization across respondents (using ",respvar,")\n"))
    cat("",sep,"\n")
    print(frame, row.names=F, right = F)
    cat(paste0(" Results from one-way ANOVA estimations with ",respvar,"\n as the dep. variable.\n"))
    cat(" Significance levels: * p<0.1; ** p<0.05; *** p<0.01\n")
  }else
    if(unique(frame$V1=="categorical")){

      frame <- as.data.frame(t(matrix(unlist(x),
                                      nrow = length(x),
                                      byrow = T)))



      # Clean data.frame
      respvar <- unique(frame$V2)
      frame$V2 <- NULL
      frame$V1 <- NULL # no longer necessary

      frame$V4 <- as.integer(frame$V4)
      frame$Significance <- rep(" ", length(frame$V3))
      frame$Significance <- ifelse(frame$V6<0.1,"*",frame$Significance)
      frame$Significance <- ifelse(frame$V6<0.05,"**",frame$Significance)
      frame$Significance <- ifelse(frame$V6<0.01,"***",frame$Significance)
      frame$V5 <- format(round(as.numeric(frame$V5), digits = 3),nsmall=3)
      frame$V6 <- format(round(as.numeric(frame$V6), digits = 3),nsmall=3)

      # reorder columns
      neword <- c("V3","V4","V5","V6","Significance","V7")
      frame <- frame[,neword]
      rm(neword)

      # Label
      colnames(frame) <- c("Attribute","df","chi-squared","p","Significance","Note")

      # Identify attribute lengths
      longest_att <- max(sapply(frame$Attribute,function(x){nchar(x)})) # longest attribute
      longest_df <- max(sapply(frame$df,function(x){nchar(x)})) # longest attribute

      # Eliminate note if not relevant (no e<5) & define separator for smaller table
      if(all(is.na(frame$Note))){
        frame$Note <- NULL

        sep <- c(paste(rep("-",longest_att),collapse = ""),
                 paste(rep("-",longest_df+2),collapse = ""),
                 paste(rep("-",12),collapse = ""),
                 paste(rep("-",6),collapse = ""),
                 paste(rep("-",12),collapse = ""))
      } else{ # define separator for wider table, w/ note
        sep <- c(paste(rep("-",longest_att),collapse = ""),
                 paste(rep("-",longest_df+2),collapse = ""),
                 paste(rep("-",12),collapse = ""),
                 paste(rep("-",6),collapse = ""),
                 paste(rep("-",12),collapse = ""),
                 paste(rep("-",12),collapse = ""))

      }

      # Create output table
      frame <- rbind(sep,frame,sep)

      cat(paste0(" Test for randomization across respondents (using ",respvar,")\n"))
      cat("",sep,"\n")
      print(frame, row.names=F,right=F)
      cat(paste0(" Results from chi-squared tests on contingency tables \n of ",respvar," and each attribute.\n"))
      cat(" Significance levels: * p<0.1; ** p<0.05; *** p<0.01\n")
    }
}
