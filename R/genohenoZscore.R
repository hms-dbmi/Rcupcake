#' A graphical summary of the phenotype values 
#'
#' Given an object of class \code{genopheno}, a figure containing a barplot for each
#' phenotype is displayed. Each barplot shows the population percentage suffering each
#' type of the phenotypes according to the values it takes, and distinguishing between
#' those having or not a mutation. Furthermore, a data.frame with the numerical variables
#' is obtained.  
#' @param input Object of \code{genopheno} class. 
#' @param nfactor By default 10. Change it into other number if you consider there is any
#' categorical variable with more than nfactor values. 
#' @param cutOff Z-score cut-off to categorize the continuous variable. By default it is set 
#' to -2 and 2. 
#' @param showTable By default FALSE. Change it into TRUE in order to visualize the table
#' with the ressults. 
#' @param outputFile By default FALSE. Change it into TRUE in order to generate a file with the 
#' phenotypes and the values for each of them and an extra column to be filled by the user
#' for further analysis. The output file is called 'phenoSummary.txt'
#' @param path By default the working directory. Define the path where you want the file to 
#' be saved
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @return A multiple graph containing a barplot with age distribution,  
#' a boxplot representing age distribution by gender and a pie chart representing 
#' gender distribution.
#' @examples
#' load(system.file("extdata", "genopheno.RData", package="genophenoR"))
#' genohenoZscore( input      = queryExample,
#'                 verbose    = FALSE 
#'            )
#' @export genohenoZscore

genohenoZscore <- function( input, cutOff = c(-2, 2), nfactor = 10, showTable = FALSE, outputFile = FALSE, path = "", verbose = FALSE ){
    
    
    message("Checking the input object")
    checkClass <- class(input)[1]
    
    if(checkClass != "genopheno"){
        message("Check the input object. Remember that this
                object must be obtained after applying the queryPheno
                function to your input file. The input object class must
                be:\"genopheno\"")
        stop()
    }
    
    if( verbose == TRUE ){
        message( "Creating a summary table with the main characteristics of population" )
    }
    
    tt <- input@iresult
    ph <- input@phenotypes


    for( i in 1:nrow( ph )){
        
        pcolumn <- which(colnames(tt) == as.character(ph[i,1]))
        
        if( length( unique( tt[,pcolumn])) <= nfactor){
            
            message( as.character(ph$variable[i]), " phenotype is considered as a categorical variable")
            message( "Z-score will not be applied to ", as.character(ph$variable[i]), " variable")
            
        }else{
            
            message( as.character(ph$variable[i]), " phenotype is considerede as a continuous variable")
            message("Checking is the variable follows a normal distribution")
            
            normalDist <- shapiro.test(as.numeric(tt[,pcolumn]))
            
            if( normalDist$p.value < 0.05){
                
                message("Z-score will be estimated for this variable")
                selection <- tt[! is.na(tt$Age),]
                selection <- selection[! is.na( selection[,pcolumn]),]
                
                message("Checking if there is correlation between age and ",  as.character(ph$variable[i]))
                correlationsTest <- cor.test(as.numeric(selection[, pcolumn]), as.numeric(selection$Age))
                
                if( correlationsTest$p.value < 0.05){
                    message("There is a correlation between ", colnames(selection)[pcolumn], 
                            " variable and age.")
                    message("Fitting linear model")
                    lm1<- lm(as.numeric(selection[, pcolumn]) ~ as.numeric(selection$Age), data= selection)
                    selection$lm1 <- lm1$residuals
                    contVariable <- selection$lm1
                    pcolumn <- which(colnames(selection) == "lm1")
                    
                }else{
                    message("There is not a correlation between ", colnames(selection)[pcolumn], 
                            " variable and age.")
                    contVariable <- as.numeric(selection[, pcolumn])
                }
                
                
                #2. population parameter calculations
                pop_sd <- sd(contVariable, na.rm = TRUE)*sqrt((length(contVariable)-1)/(length(contVariable)))
                pop_mean <- mean(contVariable, na.rm = TRUE)
                
                selection$zScore    <- NA
                selection$zScoreCat <- NA
                
                for( z in 1:nrow( selection )){
                    if(! is.na(selection[z,pcolumn])){
                        
                        selection$zScore[z] <- ( as.numeric(selection[z,pcolumn]) - pop_mean) / pop_sd
                        
                        if( selection$zScore[z] <= cutOff[1]){
                            selection$zScoreCat[z] <- "under"
                        }else if( selection$zScore[z] >= cutOff[2]){
                            selection$zScoreCat[z] <- "over"
                        }else{
                            selection$zScoreCat[z] <- "normal"
                        }
                        
                    }
                }
                
                for( j in 1:nrow(tt)){
                    if( tt$patient_id[j] %in% selection$patient_id){
                        tt[j,pcolumn-1] <- selection[selection$patient_id == tt$patient_id[j], "zScoreCat"]
                    }else{
                        tt[j,pcolumn-1] <- NA
                    }
                } 
                
                
            }
            else{
                message("The variable ",  as.character(ph$variable[i]), " does not follow a normal distribution")
                message("Z-score will not be estimated for this variable")
                
            }

        }
        
        

    }
    
    input@iresult <- tt
    return( input )       
    
}
