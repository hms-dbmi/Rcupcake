#' Query your data and generates a \code{phenoytpe}
#'
#' Given 1 file (testResults.txt), generates some RData and create an object of type 
#' \code{phenotype}.}.
#'
#' @param databasePth Determines the path where the required input file 
#' islocated. 
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @param warnings By default \code{TRUE}. Change it to \code{FALSE} to don't see
#' the warnings.
#' @return An object of class \code{phenotype}
#' @examples
#' ex1 <- queryPheno( databasePth = system.file("extdata", package="phenotypeR"),
#'                    verbose     = FALSE)
#' @export queryGenoPheno


queryGenoPheno <- function( databasePth,verbose = FALSE, warnings= TRUE) {
    
    message( "Loading the input datasets" )
    patients <- read.delim( file.path( databasePth, "testResults.txt"  ), 
                                header = TRUE, sep = "\t", 
                                colClasses = "character" )
    
    message("Checking the inputData file structure")
        colnamesPatients   <- c("patient_id","Gender", "Age")   
        check <- colnamesPatients[colnamesPatients %in% colnames(patients)]
        if(length(check) != length(colnamesPatients)){
            message("Check the inputData file structure. Remember that this
                    file must contain at least three columns with the column 
                    names as follows:\n -> patient_id \n -> Gender \n -> Age")
            stop()
        }
        
        message("Removing those patients for which there is not complete information")
        patientComplete <- patients[complete.cases(patients),]
        message("Removing duplicated data")
        patientComplete <- patientComplete[! duplicated( patientComplete), ]

    if( verbose ) {
        message( "There are ", length( unique ( patientComplete$patient_id)), " patients in your input data whith complete information for all your variables, from the initial ", length( unique ( patients$patient_id ) ), " patients in your list.")
    }
    
    message("Checking the number of mutations in the inputData file")
        colnamesInput   <- colnames(patientComplete) 
        check <- as.data.frame(as.character(colnamesInput))
        check <- check[! check[,1] %in% colnamesPatients,]
        check <- as.data.frame(check)
        check$firstL <- substr( check[,1], 1, 1)
        check$variable <- substr( check[,1], 3, nchar(as.character(check[,1])))
        check <- check[! duplicated(check),]
        
        mutations <- check[ check$firstL == "M", ]
        phenotypes <- check[ check$firstL == "P", ]
        

    
    #with the data we have, we create a comorbidity object
    result <- new( "phenotype", 
                   nMutations   = nrow(mutations),
                   nPhenotype   = nrow(phenotypes),
                   nPatient     = length( unique ( patientComplete$patient_id ) ), 
                   iresult      = patientComplete, 
                   mutations    = mutations, 
                   phenotypes   = phenotypes
    )
    
    #we create a second object with all the data of the dataset
    allData <- new( "phenotype", 
                    nMutations   = nrow(mutations),
                    nPhenotype   = nrow(phenotypes),
                    nPatient     = length( unique ( patientComplete$patient_id ) ), 
                    iresult      = patients, 
                    mutations    = mutations, 
                    phenotypes   = phenotypes )
    save(allData, file=paste0(databasePth, "allData.RData"))
    
    return( result )

}



