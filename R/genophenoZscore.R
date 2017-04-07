#' Transform continuous in categorical variables and generates a new \code{genopheno} object.
#'
#' Given an object of class \code{genopheno}, it transforms continuous into categorical variable 
#' applying Z-score. As a result a new \code{genopheno} object is generated. Note that if the number
#' of individuals is lower than 5000 a Saphiro test is done to test the normal distribution, otherwise
#' a Kolmogorov-Smirnov test is performed. 
#' 
#'     
#' @param input Object of \code{genopheno} class. 
#' @param nfactor By default 10. Change it into other number if you consider there is any
#' categorical variable with more than nfactor values. 
#' @param cutOff Z-score cut-off to categorize the continuous variable. By default it is set 
#' to -2 and 2. 
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an
#' on-time log from the function.
#' @return A \code{genopheno} class object with the continuous variable transformed into a categorical
#' variable, if possible. 
#' @examples
#' load(system.file("extdata", "genophenoExData.RData", package="genophenoR"))
#' genophenoZscore( input      = genophenoExData,
#'                 verbose    = FALSE 
#'            )
#' @export genophenoZscore

genophenoZscore <- function( input, cutOff = c(-2, 2), nfactor = 10, verbose = FALSE ){
    
    if( verbose == TRUE){
        message("Checking the input object")
    } 
    
    checkClass <- class(input)[1]
    
    if(checkClass != "genopheno"){
        message("Check the input object. Remember that this
                object must be obtained after applying the queryPheno
                function to your input file. The input object class must
                be:\"genopheno\"")
        stop()
    }
    

    tt <- input@iresult
    ph <- input@phenotypes


    for( i in 1:nrow( ph )){
        
        pcolumn <- which(colnames(tt) == as.character(ph[i,1]))
        
        if( length( unique( tt[,pcolumn])) <= nfactor){
            
            if( verbose == TRUE){
                message( as.character(ph$variable[i]), " phenotype is considered as a categorical variable")
                message( "Z-score will not be applied to ", as.character(ph$variable[i]), " variable")
            } 

        }else{
            
            if( verbose == TRUE){
                message( as.character(ph$variable[i]), " phenotype is considered as a continuous variable")
                message("Checking is the variable follows a normal distribution")
            } 
            
            if( nrow( tt ) < 5000 ){
                normalDist <- shapiro.test(as.numeric(tt[,pcolumn]))
                
            }else{
                normalDist <- ks.test(x=rnorm(as.numeric(tt[,pcolumn])),y='pnorm',alternative='two.sided')
                
            }
            
            
            if( normalDist$p.value < 0.05){
                
                if( verbose == TRUE){
                    message("Z-score will be estimated for this variable")
                } 
                
                selection <- tt[! is.na(tt$Age),]
                selection <- selection[! is.na( selection[,pcolumn]),]
                
                if( verbose == TRUE){
                    message("Checking if there is correlation between age and ",  as.character(ph$variable[i]))
                }
                

                correlationsTest <- cor.test(as.numeric(selection[, pcolumn]), as.numeric(selection$Age))
                
                if( correlationsTest$p.value < 0.05){
                    
                    if( verbose == TRUE){
                        message("There is a correlation between ", colnames(selection)[pcolumn], 
                                " variable and age.")
                        message("Fitting linear model")
                    } 
                    

                    lm1<- lm(as.numeric(selection[, pcolumn]) ~ as.numeric(selection$Age), data= selection)
                    selection$lm1 <- lm1$residuals
                    contVariable <- selection$lm1
                    pcolumn <- which(colnames(selection) == "lm1")
                    
                }else{
                    
                    if( verbose == TRUE){
                        message("There is not a correlation between ", colnames(selection)[pcolumn], 
                                " variable and age.")
                    }

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
                
                if( verbose == TRUE){
                    message("The variable ",  as.character(ph$variable[i]), " does not follow a normal distribution")
                    message("Z-score will not be estimated for this variable")
                }

                
            }

        }
        
        

    }
    
    input@iresult <- tt
    return( input )       
    
}
