#' Plot the co-occurrence analysis results in a network
#'
#' Given an object of class \code{cupcakeResults} obtained from a co-occurrence analysis, 
#' a network is obtained.
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
#' @param layout By default \code{'layout.circle'}. It can be set 
#' to any other of the possible igraph layouts. 
#' @param nodeProportion  Determines the node size proportionality. By default it is set to 
#' \code{1}. The value of the argument can be changed to any other numeric variable.
#' @param interactive Determines if the output network is interactive or not. 
#' By default the \code{interactive} argument is set up as \code{FALSE}. The value 
#' of the argument can be changed to \code{TRUE}, as a result an interactive 
#' network will be obtained.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an
#' on-time log from the function.
#'  
#' @return A network
#' @examples
#' load(system.file("extdata", "RcupcakeExResult.RData", package="Rcupcake"))
#' cooc.network <- network( 
#'               input               = cupcakeResults,
#'               representedVariable = "patientsPhenoAB",
#'               variableCutOff      = 1.5,
#'               coocPatients        = 2
#'               )
#' @export cooc.network
#' 

cooc.network <- function( input, layout = "layout.circle", representedVariable = "patientsPhenoAB", variableCutOff = 0, coocPatients = 0, nodeProportion  = 1, interactive = FALSE,verbose = FALSE ) {
    
    
    if( verbose == TRUE){
        message("Checking the input object")
    } 
    
    checkClass <- class(input)[1]
    
    if(checkClass != "cupcakeResults" ){
        message("Check the input object. Remember that this
                    object must be obtained after applying the co.occurrence
                    function. The input object class must
                    be:\"cupcakeResults\" ")
        stop()
    }
    
    if(class(input)[1]== "cupcakeResults"){
        patients <- input@patients
        input  <- input@result
        input  <- input[as.numeric(input$patientsPhenoAB) >= coocPatients, ]

        if( representedVariable %in% c("score", "oddsRatio", "relativeRisk", "PercentagePhenoAB", "patientsPhenoAB")){
            column <- which(colnames(input )==representedVariable)
            input$value <- as.numeric( input[,column] )
            input  <- input [input$value > variableCutOff,]
            
        }else if( representedVariable %in% c( "fdr", "phi" )){
            column <- which(colnames(input )==representedVariable)
            input$value <- as.numeric( input[,column] )
            input  <- input [input$value < variableCutOff,]
        }else{
            message("Please check the representedVariable. Remember that the possible values for this argument
                    are: patientsPhenoAB, PercentagePhenoAB, score, fdr, oddsRatio, phi and relativeRisk")
            stop()
        }
        
        
        input$pair   <- NA
        for(cont in 1:nrow(input)){
            pairDis <- sort(c(input$phenotypeA[cont], input$phenotypeB[cont]))
            input$pair[cont] <- paste(pairDis[1], pairDis[2], sep="*")
        }
        
        input <- input[!duplicated(input$pair),]
        
        
        for( i in 1:nrow( input ) ){
            
            if( unlist(strsplit( input$phenotypeA[ i ], ":"))[2] == " " & 
                nchar( unlist(strsplit( input$phenotypeA[ i ], ":"))[2] ) == 1){
                
                input$phenotypeA[ i ] <- paste0( input$phenotypeA[ i ], "/")
            }
            if( unlist(strsplit( input$phenotypeB[ i ], ":"))[2] == " " & 
                nchar( unlist(strsplit( input$phenotypeB[ i ], ":"))[2] ) == 1){
                
                input$phenotypeB[ i ] <- paste0( input$phenotypeB[ i ], "/")
            }
        }
        

        

        edges <- data.frame( input[ , 1 ], input[ , 2 ] )
        netw  <- igraph::graph.data.frame( edges, directed = FALSE )
        netw  <- igraph::simplify( netw )
        
        if(layout == "layout.fruchterman.reingold"){
            lay   <- igraph::layout.fruchterman.reingold( netw )
        }else if(layout == "layout.auto"){
            lay   <- igraph::layout.auto( netw )
        }else if(layout == "layout.random"){
            lay   <- igraph::layout.random( netw )
        }else if(layout == "layout.circle"){
            lay   <- igraph::layout.circle( netw )
        }else if(layout == "layout.sphere"){
            lay   <- igraph::layout.sphere( netw )
        }else if(layout == "layout.fruchterman.reingold"){
            lay   <- igraph::layout.fruchterman.reingold( netw )
        }else if(layout == "layout.kamada.kawai"){
            lay   <- igraph::layout.kamada.kawai( netw )
        }else if(layout == "layout.spring"){
            lay   <- igraph::layout.spring( netw )
        }else if(layout == "layout.reingold.tilford"){
            lay   <- igraph::layout.reingold.tilford( netw )
        }else if(layout == "layout.fruchterman.reingold.grid"){
            lay   <- igraph::layout.fruchterman.reingold.grid( netw )
        }else if(layout == "layout.lgl"){
            lay   <- igraph::layout.lgl( netw )
        }else if(layout == "layout.svd"){
            lay   <- igraph::layout.svd( netw )
        }else if(layout == "layout.graphopt"){
            lay   <- igraph::layout.graphopt( netw )
        }else if(layout == "layout.norm"){
            lay   <- igraph::layout.norm( netw )
        }else{
            message("Check if your layout is included in the igraph layout options: 
                    layout.auto, layout.random, layout.circle, layout.sphere, 
                    layout.fruchterman.reingold, layout.kamada.kawai, layout.spring, 
                    layout.reingold.tilford, layout.fruchterman.reingold.grid, 
                    layout.lgl, layout.graphopt, layout.svd, layout.norm"
            )
            stop()
        } 

        disPrev1 <- input[ , c( 1, 3 ) ]
        colnames ( disPrev1 ) <- c( "dis", "patients" )
        disPrev2 <- input[ , c( 2, 4 ) ]
        colnames ( disPrev2 ) <- c( "dis", "patients" )
        
        disPrev <- rbind ( disPrev1, disPrev2)
        disPrev <- disPrev[ !duplicated( disPrev$dis ), ]
        disPrev$prevalence <- (as.numeric(disPrev$patients)/patients)*100
        


        sizes <- as.numeric(disPrev[ , 3 ])
        names( sizes ) <- disPrev[ , 1 ]
        column <- which(colnames(input )==representedVariable)

        input=as.matrix(input)
        g=igraph::graph.edgelist(input[,1:2],directed=FALSE)
        igraph::E(g)$weight=input[,column] 
        
        if( verbose == TRUE ) {
            message( "The network contains ", igraph::vcount( g ), " nodes and ", igraph::ecount( g ), " edges." )
        }
        

      
        if(interactive == FALSE){
            plot( g, 
                  vertex.frame.color = "white",
                  layout              = lay,
                  vertex.color        = "lightblue", 
                  vertex.label.dist   = 0,      #puts the name labels slightly off the dots
                  vertex.frame.color  = 'blue', #the color of the border of the dots
                  vertex.label.color  = 'black',#the color of the name labels
                  vertex.label.font   = 0,      #the font of the name labels
                  vertex.label        = igraph::V( g )$names, #specifies the lables of the vertices. in this case the 'name' attribute is used
                  edge.label          = igraph::E(g)$weight,
                  edge.color          = "darkgrey",
                  edge.width          = 1,
                  edge.arrow.size     = 0.5,
                  vertex.size         = as.numeric( sizes[ igraph::V( g )$name ] * nodeProportion),
                  #vertex.size         = 0.5,
                  vertex.label.cex    = 0.8    #specifies the size of the font of the labels

            ) 

        }else if (interactive == TRUE){
            tkid <- igraph::tkplot(g, 
                                   vertex.frame.color = "white",
                                   layout              = lay,
                                   vertex.color        = "lightblue", 
                                   vertex.label.dist   = 0,      #puts the name labels slightly off the dots
                                   vertex.frame.color  = 'blue', #the color of the border of the dots
                                   vertex.label.color  = 'black',#the color of the name labels
                                   vertex.label.font   = 0,      #the font of the name labels
                                   vertex.label        = igraph::V( g )$names, #specifies the lables of the vertices. in this case the 'name' attribute is used
                                   edge.label          = igraph::E(g)$weight,
                                   edge.color          = "darkgrey",
                                   edge.width          = 1,
                                   edge.arrow.size     = 0.5,
                                   vertex.size         = as.numeric( sizes[ igraph::V( g )$name ]* nodeProportion),
                                   #vertex.size         = 0.5,
                                   vertex.label.cex    = 0.8    #specifies the size of the font of the labels
            ) 

            
        }
    }
}
