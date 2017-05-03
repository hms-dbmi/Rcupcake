#' Plot the co-occurrence analysis results in a heatmap.
#'
#' Given an object of class \code{cupcakeResults} obtained from a co-occurrence analysis, 
#' a heatmap is obtained. 
#'
#' @param input A \code{cupcakeResults} object, obtained 
#' by applying the \code{co.occurrence} function
#' @param representedVariable By default \code{"patientsPhenoAB"} variable will be selected. Change
#' it to any of the other possible variables (\code{'score'},(\code{'fdr'},\code{'oddsRatio'}, 
#' \code{'phi'}, \code{'relativeRisk'}, \code{'PercentagePhenoAB'}).  
#' @param variableCutOff By default \code{'0'}. The value of the argument can be changed 
#' to any other numeric variable, according to the range of the selected value.
#' @param coocPatients by default \code{'0'}.  The value of the argument can be changed
#' to any other numeric variable to show in the heatmap only those comorbidities 
#' suffered by at least \code{coocPatients} of patients.
#' @param interactive Determines if the output heatmap is interactive or not. 
#' By default the \code{interactive} argument is set up as \code{FALSE}. The value 
#' of the argument can be changed to \code{TRUE}, as a result an interactive 
#' heatmap will be obtained.
#' @param lowColor Determines the heatmap color for the lowest value.
#' By default it is set to \code{"#cde6ff"}. 
#' @param highColor Determines the heatmap color for the highest value.
#' By default it is set to \code{"red"}. 
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an
#' on-time log from the function.
#' @return A heatmap
#' @examples
#' load(system.file("extdata", "RcupcakeExResult.RData", package="Rcupcake"))
#' htmp <- cooc.heatmap( input        = cupcakeResults, 
#'               representedVariable  = "patientsPhenoAB", 
#'               variableCutOff       = 1, 
#'               coocPatients         = 1
#'               )
#' htmp
#' @export cooc.heatmap
#' 


cooc.heatmap <- function( input , representedVariable = "patientsPhenoAB", variableCutOff = 0, coocPatients = 0, interactive = FALSE, lowColor = "#cde6ff", highColor = "red", verbose = FALSE ) {

    
    if( verbose == TRUE){
        message("Checking the input object")
    } 
    
    checkClass <- class(input)[1]
    
    if(checkClass != "cupcakeResults"){
        message("Check the input object. Remember that this
                    object must be obtained after applying the query
                    function to your input file. The input object class must
                    be:\"cupcakeResults\"")
        stop()
    }
    
    if(class(input)[1]== "cupcakeResults"){
        obj <- input@result
        obj <- obj[as.numeric(obj$patientsPhenoAB) >= coocPatients, ]

        if( representedVariable %in% c("score", "oddsRatio", "relativeRisk", "PercentagePhenoAB", "patientsPhenoAB")){
            column <- which(colnames(obj )==representedVariable)
            obj$value <- as.numeric( obj[,column] )
            obj  <- obj [obj$value > variableCutOff,]
            
        }else if( representedVariable %in% c( "fdr", "phi" )){
            column <- which(colnames(obj )==representedVariable)
            obj$value <- as.numeric( obj[,column] )
            obj  <- obj [obj$value < variableCutOff,]
        }else{
            message("Please check the representedVariable. Remember that the possible values for this argument
                    are: patientsPhenoAB, PercentagePhenoAB, score, fdr, oddsRatio, phi and relativeRisk")
            stop()
        }
        
        for( i in 1:nrow( obj ) ){
            
            if( unlist(strsplit( obj$phenotypeA[ i ], ":"))[2] == " " & 
                nchar( unlist(strsplit( obj$phenotypeA[ i ], ":"))[2] ) == 1){
                
                obj$phenotypeA[ i ] <- paste0( obj$phenotypeA[ i ], "/")
            }
            if( unlist(strsplit( obj$phenotypeB[ i ], ":"))[2] == " " & 
                nchar( unlist(strsplit( obj$phenotypeB[ i ], ":"))[2] ) == 1){
                
                obj$phenotypeB[ i ] <- paste0( obj$phenotypeB[ i ], "/")
            }
        }

 
        obj [,column] <- as.numeric(obj [,column])
        m <- max(obj [,column])
        n <- min(obj [,column])

        
        if( interactive == FALSE ){
            p <- ggplot2::ggplot(obj , ggplot2::aes ( phenotypeA, phenotypeB ) ) +
                ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "black") +
                ggplot2::scale_fill_gradient(limits = c(n,m), low = lowColor,   high = highColor, na.value = "black") +
                ggplot2::theme_grey(base_size = 13) +
                ggplot2::labs ( title = "Phenotype co-occurrence", x = "phenotypes", y = "phenotypes") +
                ggplot2::scale_x_discrete(expand = c(0, 0)) +
                ggplot2::geom_text(ggplot2::aes(label = value ) ) +
                ggplot2::theme( plot.margin = grid::unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ),
                                axis.text.x = ggplot2::element_text ( angle = 45, size = 11, hjust = 1 ), panel.background = ggplot2::element_blank() )
            p
        }else if(interactive == TRUE){
            tt <- reshape2::acast(obj, phenotypeB~phenotypeA, value.var=representedVariable)
            d3heatmap::d3heatmap(tt, dendrogram = "none", scale = "none",
                                 colors =c("mediumorchid4","black"), 
                                 yaxis_font_size = 10,
                                 xaxis_font_size = 10
            )
        }
    }
}





