#' Plot the phenotype prevalence in a barplot (when there is not any variation) and in a network 
#' if a variation is defined
#'
#'Given a \code{cupcakeResults} object a barplot or a network is obtained.
#'
#' @param input A \code{cupcakeResults} object, obtained 
#' by applying the \code{co.occurrence} function
#' @param variation Determine what is the variation value of interest for 
#' performing the co-occurrence analysis. 
#' @param layout By default \code{'layout.circle'}. It can be set 
#' to any other of the possible igraph layouts. 
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an
#' on-time log from the function.
#' @param warnings By default \code{TRUE}. Change it to \code{FALSE} to don't see
#' the warnings.
#'  
#' @return A barplot or a network
#' @examples
#' load(system.file("extdata", "RcupcakeExResult.RData", package="Rcupcake"))
#' ntwk <- prevalence.plot( 
#'               input            = cupcakeResults 
#'               )
#' @export prevalence.plot
#' 

prevalence.plot <- function( input, layout = "layout.circle", variation, verbose = FALSE ) {
    
    message("Checking the input object")
    checkClass <- class(input)[1]
    
    if(checkClass != "cupcakeResults" ){
        message("Check the input object. Remember that this
                    object must be obtained after applying the co.occurrence
                    function. The input object class must
                    be:\"cupcakeResults\" ")
        stop()
    }
    
    if(class(input)[1]== "cupcakeResults"){
        patients <- input@tpatients
        prev  <- input@result
        
        disPrev1 <- prev[ , c( 1, 3 ) ]
        colnames ( disPrev1 ) <- c( "dis", "patients" )
        disPrev2 <- prev[ , c( 2, 4 ) ]
        colnames ( disPrev2 ) <- c( "dis", "patients" )
        
        disPrev <- rbind ( disPrev1, disPrev2)
        disPrev <- disPrev[ !duplicated( disPrev$dis ), ]
        disPrev$prevalence <- round((as.numeric(disPrev$patients)/patients)*100,2)

        
        for( i in 1:nrow( disPrev ) ){
            
            if( unlist(strsplit( disPrev$dis[ i ], ":"))[2] == " " & 
                nchar( unlist(strsplit( disPrev$dis[ i ], ":"))[2] ) == 1){
                
                disPrev$dis[ i ] <- paste0( disPrev$dis[ i ], "/")
            }
           
        }
        
        
        disPrev <- disPrev[with(disPrev, order(-prevalence)), ]
        colnames( disPrev ) <- c("phenotype", "N_patients", "Prevalence(%)")
        
    }
    
    
    prevalenceTable <- disPrev
    
    if( !missing( variation ) )
    {
        prevalenceTable$variation <- paste0( input@variation[1], ": ", input@variation[2])
        
        edges <- data.frame( prevalenceTable[ , c("phenotype", "variation")] )
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
        names( sizes ) <- c(prevalenceTable[ , "phenotype" ], paste0( input@variation[1], ": ", input@variation[2]))
        
        
        
        if( verbose ) {
            message( "The network contains ", igraph::vcount( netw ), " nodes and ", igraph::ecount( netw ), " edges." )
        }
        
        
             plot( netw, 
                  vertex.frame.color = "white",
                  layout              = lay,
                  vertex.color        = ifelse ( igraph::V( netw )$name %in% paste0( input@variation[1], ": ", input@variation[2]), "#ff349a", "royalblue2" ), 
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
                  vertex.label.cex    = 0.8    #specifies the size of the font of the labels
            ) 
            #legend('bottomright',legend= paste0( disPrev$dis,": " ,disPrev$patients, "patients (", round(disPrev$prevalence, 3) ,"%)" ),col='black',pch=21, pt.bg='lightblue')
            
        
            
    }else{

        colnames( prevalenceTable )[3] <- "Prevalence"
        
        p <- ggplot2::qplot(x=phenotype, y=Prevalence, fill=phenotype,
                               data=prevalenceTable)+
            ggplot2::geom_bar(position = "dodge", 
                              stat="identity", 
                              colour = "black")

        p <- p + ggplot2::scale_fill_grey()
        p <- p + ggplot2::theme_classic( ) + ggplot2::theme( plot.margin = ggplot2::unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ), 
                                                             axis.line = ggplot2::element_line ( size = 0.7, color = "black" ), text = ggplot2::element_text ( size = 11 ) ,
                                                             axis.text.x = ggplot2::element_text ( angle = 45, size = 10, hjust = 1 ))
        p

    }
}