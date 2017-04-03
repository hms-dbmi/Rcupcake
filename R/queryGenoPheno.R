#' Query your data and generates a \code{genopheno}
#'
#' Given a tabulated file, checks if it contains the data in the 
#' correct format and generates a \code{phenotype} object.
#'
#' @param inputDataFile Determines the file with the complete path where the required 
#' input file is located. This input file must contain three columns: "patient_id" with
#' the patient identifier, "Gender" and "Age". Mutation columns must start with "M." while
#' phenotype ones must start with a "P."
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @param warnings By default \code{TRUE}. Change it to \code{FALSE} to don't see
#' the warnings.
#' @return An object of class \code{phenotype}
#' @examples
#' queryExample <- queryGenoPheno( inputDataFile = paste0(system.file("extdata", package="genophenoR"), 
#'                                                 "/queryOutput.txt"),
#'                                   verbose     = TRUE)
#' queryExample
#' @export queryGenoPheno


queryGenoPheno <- function( inputDataFile ,verbose = FALSE, warnings= TRUE) {
   
    if( verbose == TRUE){
        message( "Loading the input datasets" )
    } 
    
    patients <- read.delim( inputDataFile, 
                            header = TRUE, 
                            sep = "\t", 
                            colClasses = "character" )
    
    if( verbose == TRUE){
        message("Checking the inputData file structure")
    }
    
        colnamesPatients   <- c("patient_id","Gender", "Age")   
        check <- colnamesPatients[colnamesPatients %in% colnames(patients)]
        if(length(check) != length(colnamesPatients)){
            message("Check the inputData file structure. Remember that this
                    file must contain at least three columns with the column 
                    names as follows:\n -> patient_id \n -> Gender \n -> Age")
            stop()
        }

        
        if( verbose == TRUE){
            message("Removing duplicated data")
        }
    
        patientComplete <- patients[! duplicated( patients), ]

    
        if( verbose == TRUE) {
        message( "There are ", length( unique ( patientComplete$patient_id)), " patients in your input data whith complete information for all your variables, from the initial ", length( unique ( patients$patient_id ) ), " patients in your list.")
        message("Checking the number of mutations in the inputData file")
        }
    
    
        colnamesInput   <- colnames(patientComplete) 
        check <- as.data.frame(as.character(colnamesInput))
        check <- check[! check[,1] %in% colnamesPatients,]
        check <- as.data.frame(check)
        check$firstL <- substr( check[,1], 1, 1)
        check$variable <- substr( check[,1], 3, nchar(as.character(check[,1])))
        check <- check[! duplicated(check),]
        
        mutations <- check[ check$firstL == "M", ]
        phenotypes <- check[ check$firstL == "P", ]
        

        if( verbose == TRUE) {
            message("Generating the result object")
        }
            #with the data we have, we create a comorbidity object
   
        result <- new( "genopheno", 
                       nMutations   = nrow(mutations),
                       nPhenotype   = nrow(phenotypes),
                       nPatient     = length( unique ( patientComplete$patient_id ) ), 
                       iresult      = patientComplete, 
                       mutations    = mutations, 
                       phenotypes   = phenotypes
        )
        return( result )

}



