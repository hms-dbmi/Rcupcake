#' Plot the comorbidity analysis results in a network
#'
#'Given an object of class \code{genophenoComor} obtained from a comorbidity analysis, 
#' a network is obtained.
#'
#' @param input A \code{genophenoComor} object, obtained 
#' by applying the \code{comorbidityAnalysis} function
#' @param databasePth Determines the path where the intermediate RData objects 
#' have been created. It is the same path where the three required input files 
#' (patientData, diagnosisData, admissionData) are located. 
#' @param layout By default \code{'layout.fruchterman.reingold'}. It can be set 
#' to any other of the possible igraph layouts. 
#' @param selectValue By default \code{"patientsPhenoAB"} variable will be selected. Change
#' it to any of the other possible variables (\code{'score'},(\code{'fdr'},\code{'odds ratio'}, 
#' \code{'phi'}, \code{'rr'}).  
#' @param cutOff By default \code{'0.05'}. The value of the argument can be changed 
#' to any other numeric variable, according to the range of the selected value.
#' @param npairs by default \code{'0'}.  The value of the argument can be changed
#' to any other numeric variable to show in the network only those comorbidities 
#' suffered by at least \code{npairs} of patients.
#' @param prop  Determines the node size proportionality. By default it is set to 
#' \code{1}. The value of the argument can be changed to any other numeric variable.
#' @param title Determines the title of the network figure. By default 
#' \code{'Comorbidity network'}. 
#' @param interactive Determines if the output network is interactive or not. 
#' By default the \code{interactive} argument is set up as \code{FALSE}. The value 
#' of the argument can be changed to \code{TRUE}, as a result an interactive 
#' network will be obtained.
#' @param comorColor  Determines the node color for the comorbid disorders. 
#' By default it is set to \code{"lightblue"}.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#'  
#' @return A network
#' @examples
#' load(system.file("extdata", "genophenoComor.RData", package="genophenoR"))
#' genoPhenoNetwork <- network( 
#'               input            = genophenoComor,
#'               selectValue      = "score",
#'               cutOff           = 1.5,
#'               npairs           = 2,
#'               title            = "Example comorbidity network"
#'               )
#' @export genoPhenoNetwork
#' 

genoPhenoNetwork <- function( input, layout = "layout.circle", selectValue = "patientsPhenoAB", title = "Phenotype comorbidity network", cutOff = 0, npairs = 0, prop  = 1, interactive = FALSE, comorColor = "lightblue", verbose = FALSE ) {
    
    
    if( verbose == TRUE){
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
        patients <- input@patients
        input  <- input@result
        input  <- input[as.numeric(input$patientsPhenoAB) >= npairs, ]
        input  <- input[input[[selectValue]] >= cutOff,]
        

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
        disPrev$dis <- paste0( disPrev$dis, ": yes")
        
        sizes <- as.numeric(disPrev[ , 3 ])
        names( sizes ) <- disPrev[ , 1 ]
        column <- which(colnames(input )==selectValue)
        
      
        input[,1]=paste0( as.character(input[,1]), ": yes") 
        input[,2]=paste0( as.character(input[,2]), ": yes") 
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
                  vertex.size         = as.numeric( sizes[ igraph::V( g )$name ] * prop),
                  #vertex.size         = 0.5,
                  vertex.label.cex    = 0.8,    #specifies the size of the font of the labels
                  main                = title
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
                                   vertex.size         = as.numeric( sizes[ igraph::V( g )$name ]* prop),
                                   #vertex.size         = 0.5,
                                   vertex.label.cex    = 0.8,    #specifies the size of the font of the labels
                                   main                = title
            ) 

            
        }
    }
}
