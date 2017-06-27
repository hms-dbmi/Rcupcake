#' Comparison by pairs of variables in a  \code{cupcakeData} object
#'
#' Given an object of type \code{cupcakeData} and two variables of the 
#' data.frame a statistically comparison is performed. According to the 
#' type of variable different test will be run (fisher test, t-test or 
#' phearson test). As a result the p-value and some other information 
#' will be return. 
#' 
#' @param input  A \code{cupcakeData} object, obtained after applying the my.data function. 
#' @param variable1 The name of the first variable that has to be the same that 
#' the colname of the variable in the input data.frame.   
#' @param variable2 The name of the second variable that has to be the same that 
#' the colname of the variable in the input data.frame. 
#' @param nfactor By default 10. Change it into other number if you consider there is any
#' continuous variable with less than \code{nfactor} values. 
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an
#' on-time log from the function.
#' @return An object of class \code{cupcakeResults}
#' @examples
#' load(system.file("extdata", "RcupcakeExData.RData", package="Rcupcake"))
#' comparison <- comparison2b2( 
#'               input         = RcupcakeExData, 
#'               variable1     = "Age",
#'               variable2     = "Gender"
#'               )
#' @export comparison2b2

comparison2b2 <- function ( input, variable1, variable2, nfactor = 10, verbose = FALSE){
    
    if( verbose == TRUE){
        message("Checking the input object")
    } 
    
    checkClass <- class(input)[1]
    
    if(checkClass != "cupcakeData"){
        message("Check the input object. Remember that this
                object must be obtained after applying the my.data
                function to your input file. The input object class must
                be:\"cupcakeData\"")
        stop()
    }
    
    data <- input@iresult
    
    
    if( verbose == TRUE){
        message( "Checking the variables names" )
    }
    
    if( ! variable1 %in% colnames(data) | ! variable2 %in% colnames(data) ){
        message("Check the variables names. Remember that these
                variables names must writen in the same way they appear in
                the column name of the data frame.")
        stop()
        
    }
    
    selection <- data[, c(variable1, variable2)]
    selection[selection == ""]  <- NA 
    selection <- selection[complete.cases(selection), ]
    
    noptionsVariable1 <- length(unique( selection[, 1 ] ) )
    noptionsVariable2 <- length(unique( selection[, 2 ] ) )
    
    if( noptionsVariable1 > nfactor ){
        message( paste0(variable1, " will be considered as a continuous variable"))
        class1 <- "continuous"
    }else if( noptionsVariable1 == 2 ){
        message( paste0(variable1, " will be considered as a binary variable"))
        class1 <- "binary"
    }else {
        message( paste0(variable1, " is not a continuous nor binary variable. 
                        No comparison is available for this kind of variable."))
        stop()
    }
 
    if( noptionsVariable2 > nfactor ){
        message( paste0(variable2, " will be considered as a continuous variable"))
        class2 <- "continuous"
    }else if( noptionsVariable2 == 2 ){
        message( paste0(variable2, " will be considered as a binary variable"))
        class2 <- "binary"
    }else {
        message( paste0(variable2, " is not a continuous nor binary variable. 
                        No comparison is available for this kind of variable."))
        stop()
    }
    
    
    if( class1 == "binary" & class2 == "binary"){
        message("Fisher test will be performed")
        
        contingencyTable <-  table( selection )
        
        message("\n Contingency table:")
        print( addmargins( contingencyTable ) )
        
        message("\n Proportion by row:")
        print( rowSums(prop.table(contingencyTable ) ) )
        
        
        output <- fisher.test( contingencyTable )
        

        }else if( class1 == "continuous" & class2 == "continuous"){
        message("Pearson test will be performed")
            
            output <- cor.test( as.numeric(selection[, variable1]), as.numeric(selection[, variable2]), 
                                method     = "pearson", 
                                conf.level = 0.95)
            
            }else{
                message("T-test will be performed")
                if( class1 == "continuous" ){
                    options <- unique( selection[, variable2])
                    dataA <- as.numeric( selection[ selection[, variable2] == options[1], variable1] )
                    dataB <- as.numeric( selection[ selection[, variable2] == options[2], variable1] )
                    
                    message( paste0( variable2, "(", options[1],")-> ", variable1 ,": mean ", round(mean( dataA ), 2), "; standar deviation: ", round(sd( dataA ), 2)))
                    message( paste0( variable2, "(", options[2],")-> ", variable1 ,": mean ", round(mean( dataB ), 2), "; standar deviation: ", round(sd( dataB ), 2)))
                    
                    output <- t.test(dataA,dataB, 
                                     var.equal = TRUE, 
                                     paired    = FALSE)
                    
                }else if ( class2 == "continuous"){
                    options <- unique( selection[, variable1])
                    dataA <- as.numeric( selection[ selection[, variable1] == options[1], variable2] )
                    dataB <- as.numeric( selection[ selection[, variable1] == options[2], variable2] )
                    
                    
                    message( paste0( variable1, "(", options[1],")-> ", variable2 ,": mean ", round(mean( dataA ), 2), "; standar deviation: ", round(sd( dataA ), 2)))
                    message( paste0( variable1, "(", options[2],")-> ", variable2 ,": mean ", round(mean( dataB ), 2), "; standar deviation: ", round(sd( dataB ), 2)))
                    
                    
                    output <- t.test(dataA,dataB, 
                                     var.equal = TRUE, 
                                     paired    = FALSE)
                    
                }
                
                
        
            }
    
    return(output)
   
}









