#' Patients selection
#'
#' Given an object of type \code{cupcakeData} and two phenotypes of
#' interest, the patients having both phenotypes are selected.
#'
#' @param input  A \code{cupcakeData} object, obtained with the \code{my.data} function. 
#' @param pth Determines the path where the required input file with 
#' the phenotypic data is located.
#' @param phenotypeA One of the phenotypes of interest and the value 
#' in which user is interested. For example, c("Arthritis", "Yes")
#' @param phenotypeB The second phenotype of interest and the value 
#' in which user is interested. For example, c("Diabetes", "Yes")
#' @param aggregate By default TRUE. Change it to FALSE if you want to 
#' analyze the co-occurrence taking into all the values of each phenotype.
#' @param ageRange Determines what is the age range of interest for
#' performing the co-occurrence analysis. By default it is set from 0 to 100 
#' years old. 
#' @param gender Determine what is the gender of interest for 
#' performing the co-occurrence analysis. By default \code{ALL}. Change it to the 
#' gender of interest for your co-occurrence analysis.
#' @param variation Determine what is the variation value of interest for 
#' performing the co-occurrence analysis. By default \code{c("ALL", "ALL")}. Change it to the 
#' value of interest for your co-occurrence analysis. For example, \code{c("CHD8", "yes")}
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an
#' on-time log from the function.
#' @param warnings By default \code{TRUE}. Change it to \code{FALSE} to don't see
#' the warnings.
#' @return An object of class \code{cgpAnalysis}
#' @examples
#' load(system.file("extdata", "RcupcakeDataExData.RData", package="Rcupcake"))
#' ex1 <- patient.selection( 
#'               input         = RcupcakeExData,
#'               pth           = system.file("extdata", package="Rcupcake"),
#'               phenotypeA    = c("Arthritis", "Yes"),
#'               phenotypeB    = c("Diabetes", "Yes"),
#'               aggregate     = TRUE, 
#'               ageRange      = c(0,100),
#'               gender        = "male", 
#'               )
#' @export patient.selection

patient.selection <- function ( input, pth, ageRange=c(0,100), phenotypeA, phenotypeB, aggregate = TRUE, gender="ALL", variation=c("ALL", "ALL"), verbose = FALSE, warnings = TRUE ){
    
    
    message("Checking the input object")
    checkClass <- class(input)[1]
    
    if(checkClass != "cupcakeData"){
        message("Check the input object. Remember that this
                object must be obtained after applying the dataframe2cupcake
                function to your input file. The input object class must
                be:\"cupcakeData\"")
        stop()
    }
    
    data <- input@iresult
    
    message( "Loading the phenotype data file" )
 
    codes <- read.delim ( file.path(pth, "phenoSummary.txt"),
                          header=TRUE, 
                          sep="\t", 
                          colClasses="character" ) 
    
    if( aggregate == TRUE ){

        message( "Checking the phenotype data file" )
        checkPheno <- as.data.frame( summary( as.factor( codes$yesno ) ) )
        good       <-  c("no", "yes")
        
        if( nrow( checkPheno) != 2 | 
            ! tolower( rownames( checkPheno)[1] ) %in% good |
            ! tolower( rownames( checkPheno)[2] ) %in% good){
            message("The yesno column in the phenoSummary file is not filled correctly. Please, revise it,\nand check that the only possible values for this column are: yes and no.")
            stop()
        }

        message( "Aggregating the phenotypes values as yes/no" )
        pAcolumn <- which(colnames(data) == paste0("P.", phenotypeA[1]))
        pBcolumn <- which(colnames(data) == paste0("P.", phenotypeB[1]))
        codesSelection <- codes[ codes$phenotype == phenotypeA[1] | 
                                     codes$phenotype == phenotypeB[1], c(1,2,ncol(codes)),]
            
        for( j in 1:nrow(codesSelection)){
                data[ ,pAcolumn][ data[ ,pAcolumn] == codesSelection$phenotypeValue[j]] <- codesSelection$yesno[j]
                data[ ,pBcolumn][ data[ ,pBcolumn] == codesSelection$phenotypeValue[j]] <- codesSelection$yesno[j]
                }
        
        qresult <- data[ data[,pAcolumn] == phenotypeA[2] & data[,pBcolumn] == phenotypeB[2], ]
        
        if ( !missing( ageRange ) ) {
            
            naCheck <- qresult[! is.na( qresult$Age), ]
            
            if( nrow(naCheck) != nrow( qresult)){
                message("There is not age information for all the patients.")
                noAge <- nrow( qresult ) - nrow( naCheck )
                message("The ", noAge, " patients without age data will be removed")
                qresult  <- qresult[! is.na( qresult$Age), ]
            }
            
            qresult$Age <- as.numeric(qresult$Age)
            qresult <- qresult[ qresult$Age >= ageRange[ 1 ] & qresult$Age <= ageRange[ 2 ], ]
        }
        
        if ( !missing( gender ) ) {
            if(gender!="ALL"){
                qresult <- qresult[ qresult$Gender == gender, ]
            }
        }
            
        if ( !missing( variation ) ) {
            if(variation[2] !="ALL"){
                mt <- input@variations
                ncolumn <- which(colnames(qresult) == as.character(mt$check[1]))
                qresult <- qresult[ qresult[,ncolumn] == variation[2], ]
            }
        }
            
        if( nrow(qresult) != 0){
            message("There are ", length(unique(qresult$patient_id)), " patients following your input requirements")
            return(unique(qresult$patient_id))
        }else{
            message( "There is not any patient with the input requirements")
            stop()
        }
        }else if( aggregate == FALSE ){
            
            message( "For each phenotypes, all the possible values will be used")
            pAcolumn <- which(colnames(data) == paste0("P.", phenotypeA[1]))
            pBcolumn <- which(colnames(data) == paste0("P.", phenotypeB[1]))
            qresult <- data[ tolower(data[,pAcolumn]) == tolower(phenotypeA[2]) & tolower(data[,pBcolumn]) == tolower(phenotypeB[2]), ]
            
            if ( !missing( ageRange ) ) {
                
                naCheck <- qresult[! is.na( qresult$Age), ]
                
                if( nrow(naCheck) != nrow( qresult)){
                    message("There is not age information for all the patients.")
                    noAge <- nrow( qresult ) - nrow( naCheck )
                    message("The ", noAge, " patients without age data will be removed")
                    qresult  <- qresult[! is.na( qresult$Age), ]
                }
                
                qresult$Age <- as.numeric(qresult$Age)
                qresult <- qresult[ qresult$Age >= ageRange[ 1 ] & qresult$Age <= ageRange[ 2 ], ]
            }
            
            if ( !missing( gender ) ) {
                if(gender!="ALL"){
                    qresult <- qresult[ qresult$Gender == gender, ]
                }
            }
            
            if ( !missing( variation ) ) {
                if(variation[2] !="ALL"){
                    mt <- input@variations
                    ncolumn <- which(colnames(qresult) == as.character(mt$check[1]))
                    qresult <- qresult[ qresult[,ncolumn] == variation[2], ]
                }
            }
            
            if( nrow(qresult) != 0){
                message("There are ", length(unique(qresult$patient_id)), " patients following your input requirements")
                return(unique(qresult$patient_id))
            }else{
                message( "There is not any patient with the input requirements")
                stop()
            }

           
        }
    }







