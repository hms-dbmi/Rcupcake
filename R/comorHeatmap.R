#' Plot the comorbidity analysis results in a heatmap.
#'
#' Given an object of class \code{cAnalysis} obtained from a comorbidity analysis, 
#' a heatmap is obtained. 
#'
#' @param input A \code{cAnalysis} object, obtained 
#' by applying the \code{comorbidityAnalysis} function
#' @param selectValue By default \code{"AB"} variable will be selected. Change
#' it to any of the other possible variables (\code{'score'},(\code{'fdr'},\code{'odds ratio'}, 
#' \code{'phi'}, \code{'rr'}).  
#' @param cutOff By default \code{'0.05'}. The value of the argument can be changed 
#' to any other numeric variable, according to the range of the selected value.
#' @param npairs by default \code{'0'}.  The value of the argument can be changed
#' to any other numeric variable to show in the network only those comorbidities 
#' suffered by at least \code{npairs} of patients.
#' @param interactive Determines if the output heatmap is interactive or not. 
#' By default the \code{interactive} argument is set up as \code{FALSE}. The value 
#' of the argument can be changed to \code{TRUE}, as a result an interactive 
#' heatmap will be obtained.
#' @param lowColor Determines the heatmap color for the lowest value.
#' By default it is set to \code{"#cde6ff"}. 
#' @param highColor Determines the heatmap color for the highest value.
#' By default it is set to \code{"red"}. 
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @param warnings By default \code{TRUE}. Change it to \code{FALSE} to don't see
#' the warnings.
#' @return A heatmap
#' @examples
#' load(system.file("extdata", "phenoEx.RData", package="genophenoR"))
#' htmp <- comorHeatmap( input = phenoEx, 
#'               selectValue  = "AB", 
#'               cutOff       = 1, 
#'               npairs       = 1
#'               )
#' htmp
#' @export comorHeatmap
#' 


comorHeatmap <- function( input , selectValue = "score", cutOff = 0.05, npairs = 0, interactive = FALSE, lowColor = "#cde6ff", highColor = "red", verbose = FALSE ) {

    message("Checking the input object")
    checkClass <- class(input)[1]
    
    if(checkClass != "cAnalysis"){
        message("Check the input object. Remember that this
                    object must be obtained after applying the query
                    function to your input file. The input object class must
                    be:\"cAnalysis\"")
        stop()
    }
    
    if(class(input)[1]== "cAnalysis"){
        obj <- input@result
        obj <- obj[as.numeric(obj$AB) >= npairs, ]
        column <- which(colnames(obj )==selectValue)
        obj$value <- as.numeric( obj[,column] )
        obj  <- obj [obj$value >= cutOff,]
        obj [,column] <- as.numeric(obj [,column])
        
        casted_rr <- reshape::cast(obj , disAcode ~ disBcode, value= selectValue  )
        l <- dim(casted_rr)[2]        
        Rowm  <- rowMeans(casted_rr[2:l], na.rm = T)       
        casted_rr<- cbind(casted_rr, Rowm)
        ordered<-  casted_rr[order(casted_rr["Rowm"]), "disAcode" ]          
        obj$disAcode  <- factor(obj $disAcode  , levels= as.factor(ordered))
        m <- max(obj [,column])
        n <- min(obj [,column])

        
        if( interactive == FALSE ){
            p <- ggplot2::ggplot(obj , ggplot2::aes ( disAcode, disBcode ) ) +
                ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "black") +
                ggplot2::scale_fill_gradient(limits = c(n,m), low = lowColor,   high = highColor, na.value = "black") +
                ggplot2::theme_grey(base_size = 13) +
                ggplot2::labs ( title = "Comorbidity between diseases", x = "diagnosis code under study", y = "disease comorbidities") +
                ggplot2::scale_x_discrete(expand = c(0, 0)) +
                ggplot2::geom_text(ggplot2::aes(label = value ) ) +
                ggplot2::theme( plot.margin = grid::unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ),
                                axis.text.x = ggplot2::element_text ( angle = 45, size = 11, hjust = 1 ), panel.background = ggplot2::element_blank() )
            p
        }else if(interactive == TRUE){
            tt <- reshape2::acast(obj, disBcode~disAcode, value.var=selectValue)
            d3heatmap::d3heatmap(tt, dendrogram = "none", scale = "none",
                                 colors =c("mediumorchid4","black"), 
                                 yaxis_font_size = 10,
                                 xaxis_font_size = 10
            )
        }
    }
}





