#' Plot the phenotype prevalence related to a mutation in a network
#'
#'Given mutation and a \code{genophenoComor} object table a network is obtained.
#'
#' @param input A \code{genophenoComor} object, obtained 
#' by applying the \code{comorbidityAnalysis} function
#' @param layout By default \code{'layout.fruchterman.reingold'}. It can be set 
#' to any other of the possible igraph layouts. 
#' @param title Determines the title of the network figure. By default 
#' \code{'Comorbidity network'}. 
#' @param interactive Determines if the output network is interactive or not. 
#' By default the \code{interactive} argument is set up as \code{FALSE}. The value 
#' of the argument can be changed to \code{TRUE}, as a result an interactive 
#' network will be obtained.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @param warnings By default \code{TRUE}. Change it to \code{FALSE} to don't see
#' the warnings.
#'  
#' @return A network
#' @examples
#' load(system.file("extdata", "genophenoComor.RData", package="genophenoR"))
#' ntwk <- prevalenceNetwork( 
#'               input            = genophenoComor 
#'               )
#' @export prevalenceNetwork
#' 

prevalenceNetwork <- function( input, layout = "layout.circle", title = "Phenotype prevalence network", interactive = FALSE, verbose = FALSE ) {
    
    message("Checking the input object")
    checkClass <- class(input)[1]
    
    if(checkClass != "genophenoComor" ){
        message("Check the input object. Remember that this
                    object must be obtained after applying the query
                    function to your input file. The input object class must
                    be:\"genophenoComor\" ")
        stop()
    }
    
    if(class(input)[1]== "genophenoComor"){
        patients <- input@tpatients
        prev  <- input@result
        
        disPrev1 <- prev[ , c( 1, 3 ) ]
        colnames ( disPrev1 ) <- c( "dis", "patients" )
        disPrev2 <- prev[ , c( 2, 4 ) ]
        colnames ( disPrev2 ) <- c( "dis", "patients" )
        
        disPrev <- rbind ( disPrev1, disPrev2)
        disPrev <- disPrev[ !duplicated( disPrev$dis ), ]
        disPrev$prevalence <- round((as.numeric(disPrev$patients)/patients)*100,2)
        disPrev$dis <- paste0( disPrev$dis, " (yes)")
        
        disPrev <- disPrev[with(disPrev, order(-prevalence)), ]
        colnames( disPrev ) <- c("phenotype", "N_patients", "Prevalence(%)")
        
    }
    
    
    prevalenceTable <- disPrev
    prevalenceTable$mutation <- paste0( input@mutation[1], ": ", input@mutation[2])
    
    edges <- data.frame( prevalenceTable[ , c("phenotype", "mutation")] )
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
            stop
            message("Check if your layout is included in the igraph layout options: 
                    layout.auto, layout.random, layout.circle, layout.sphere, 
                    layout.fruchterman.reingold, layout.kamada.kawai, layout.spring, 
                    layout.reingold.tilford, layout.fruchterman.reingold.grid, 
                    layout.lgl, layout.graphopt, layout.svd, layout.norm"
            )
        } 

    
        sizes <- c( as.numeric(prevalenceTable[ , "Prevalence(%)" ]), 30)
        names( sizes ) <- c(prevalenceTable[ , "phenotype" ], paste0( input@mutation[1], ": ", input@mutation[2]))

      

        if( verbose ) {
            message( "The network contains ", igraph::vcount( netw ), " nodes and ", igraph::ecount( netw ), " edges." )
        }
        

        

        if(interactive == FALSE){
            plot( netw, 
                  vertex.frame.color = "white",
                  layout              = lay,
                  vertex.color        = ifelse ( igraph::V( netw )$name %in% paste0( input@mutation[1], ": ", input@mutation[2]), "#ff349a", "royalblue2" ), 
                  vertex.label.dist   = 0,      #puts the name labels slightly off the dots
                  vertex.frame.color  = 'blue', #the color of the border of the dots
                  vertex.label.color  = 'black',#the color of the name labels
                  vertex.label.font   = 0,      #the font of the name labels
                  vertex.label        = igraph::V( netw )$names, #specifies the lables of the vertices. in this case the 'name' attribute is used
                  edge.color          = "darkgrey",
                  edge.width          = 1,
                  edge.arrow.size     = 0.5,
                  vertex.size         = as.numeric( sizes[ igraph::V( netw )$name ] ),
                  #vertex.size         = 0.5,
                  vertex.label.cex    = 0.8,    #specifies the size of the font of the labels
                  main                = title
            ) 
            #legend('bottomright',legend= paste0( disPrev$dis,": " ,disPrev$patients, "patients (", round(disPrev$prevalence, 3) ,"%)" ),col='black',pch=21, pt.bg='lightblue')
            
        }else if (interactive == TRUE){
            tkid <- igraph::tkplot(g, 
                                   vertex.frame.color = "white",
                                   layout              = lay,
                                   vertex.color        = ifelse ( igraph::V( netw )$name %in% input@mutation[1],"#ff349a" , "royalblue2" ), 
                                   vertex.label.dist   = 0,      #puts the name labels slightly off the dots
                                   vertex.frame.color  = 'blue', #the color of the border of the dots
                                   vertex.label.color  = 'black',#the color of the name labels
                                   vertex.label.font   = 0,      #the font of the name labels
                                   vertex.label        = igraph::V( netw )$names, #specifies the lables of the vertices. in this case the 'name' attribute is used
                                   edge.color          = "darkgrey",
                                   edge.width          = 1,
                                   edge.arrow.size     = 0.5,
                                   vertex.size         = as.numeric( sizes[ igraph::V( netw )$name ] ),
                                   #vertex.size         = 0.5,
                                   vertex.label.cex    = 0.8,    #specifies the size of the font of the labels
                                   main                      = title
            ) 
            #legend('bottomright',legend= paste0( disPrev$dis,": " ,disPrev$patients, "patients (", round(disPrev$prevalence, 3) ,"%)" ),col='black',pch=21, pt.bg='lightblue')
            
            
        }
}