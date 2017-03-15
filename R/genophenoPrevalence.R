#' Generates a table with the prevalence of each phenotype
#'
#'Given an object of class \code{genophenoComor} obtained from a comorbidity analysis, 
#' a prevalence table is obtained.
#'
#' @param input A \code{genophenoComor} object, obtained 
#' by applying the \code{comorbidityAnalysis} function
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @param warnings By default \code{TRUE}. Change it to \code{FALSE} to don't see
#' the warnings.
#'  
#' @return A table
#' @examples
#' load(system.file("extdata", "genophenoComor.RData", package="genophenoR"))
#' prevalence <- genoPhenoPrevalence( 
#'               input            = genophenoComor
#'               )
#' @export genoPhenoPrevalence
#' 

genoPhenoPrevalence <- function( input, verbose = FALSE, warnings = FALSE){
    
    if( verbose == TRUE ){
        message("Checking the input object")
    }

    checkClass <- class(input)[1]
    
    if(checkClass != "genophenoComor" ){
        message("Check the input object. Remember that this
                    object must be obtained after applying the query
                    function to your input file. The input object class must
                    be:\"genophenoComor\" ")
        stop()
    }
    
    if(class(input)[1]== "genophenoComor"){
        patients <- input@tpatients
        input  <- input@result

        disPrev1 <- input[ , c( 1, 3 ) ]
        colnames ( disPrev1 ) <- c( "dis", "patients" )
        disPrev2 <- input[ , c( 2, 4 ) ]
        colnames ( disPrev2 ) <- c( "dis", "patients" )
        
        disPrev <- rbind ( disPrev1, disPrev2)
        disPrev <- disPrev[ !duplicated( disPrev$dis ), ]
        disPrev$prevalence <- round((as.numeric(disPrev$patients)/patients)*100,2)
        disPrev$dis <- paste0( disPrev$dis, " (yes)")
        
        disPrev <- disPrev[with(disPrev, order(-prevalence)), ]
        colnames( disPrev ) <- c("phenotype", "N_patients", "Prevalence(%)")
        
        for( cont in 1:nrow(disPrev)){
            confint <-  binom.test( as.numeric(disPrev[cont,2]), patients)$conf.int
            disPrev$ConfidenceInterval[cont] <- paste0("[", round(confint[[1]]*100,1),"-",round(confint[[2]]*100,1), "]")
            
        }
        
        return( disPrev )
    }
}
