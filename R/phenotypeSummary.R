#' A graphical summary of the phenotype values 
#'
#' Given an object of class \code{genopheno}, a figure containing a barplot for each
#' phenotype is displayed. Each barplot shows the population percentage suffering each
#' type of the phenotypes according to the values it takes, and distinguishing between
#' those having or not a mutation. Furthermore, a data.frame with the numerical variables
#' is obtained.  
#'
#' @param input Object of \code{genopheno} class. 
#' @param nfactor By default 10. Change it into other number if you consider there is any
#' categorical variable with more than nfactor values. 
#' @param mutation Determines the mutation of interest for wchich you want to analyze the 
#' phenotype values
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
#' phenotypeSummary( input      = data2b2, 
#'                  mutation   = "CHD8",
#'                  verbose    = FALSE 
#'            )
#' @export phenotypeSummary

phenotypeSummary <- function( input, mutation, nfactor = 10, showTable = FALSE, outputFile = FALSE, path = "", verbose = FALSE ){
    
    
    message("Checking the input object")
    checkClass <- class(input)[1]
    
    if(checkClass != "genopheno"){
        message("Check the input object. Remember that this
                object must be obtained after applying the queryPheno
                function to your input file. The input object class must
                be:\"phenotype\"")
        stop()
    }
    
    if( verbose == TRUE ){
        message( "Creating a summary table with the main characteristics of population" )
    }
    
    tt <- input@iresult
    ph <- input@phenotypes
    mt <- input@mutations
    
    if ( missing( mutation ) ) {
        message("Please, enter the mutation of interest for this analysis")
        stop()
    }
    
    if( mutation %in% mt$variable ){
        mt <- mt[ mt$variable == mutation, ]        
    }else{
        message( "Your mutation of interest is not in the mutation list")
        message( "The mutations availabe for this analysis are: ")
        for( i in 1:nrow(mt)){
            message("-> ", mt$variable[i])
        }
        stop()
    }

    

    #grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(ph), nrow(mt))))
    #vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y)
    plots <- list()
    
    for( i in 1:nrow( ph )){
        
        pcolumn <- which(colnames(tt) == as.character(ph[i,1]))
        
        if( length( unique( tt[,pcolumn])) <= nfactor){
            
            message( as.character(ph$variable[i]), " phenotype is considered as a categorical variable")
            selection <- as.data.frame((round(100*summary(as.factor(tt[,pcolumn]))/length(unique(tt$patient_id)),2)))
            colnames(selection) <- ph$variable[i]
            
            for( j in 1:nrow(mt)){

                mcolumn <- which(colnames(tt) == as.character(mt[j,1]))
                mtyes <- tt[ tt[,mcolumn] =="yes",]
                mtyes <- as.data.frame((round(100*summary(as.factor(mtyes[,pcolumn]))/length(unique(mtyes$patient_id)),2)))
                colnames(mtyes) <- paste0("P_", mt$variable[j], "yes")
                
                mtno  <- tt[ tt[,mcolumn] =="no",]
                mtno <- as.data.frame((round(100*summary(as.factor(mtno[,pcolumn]))/length(unique(mtno$patient_id)),2)))
                colnames(mtno) <- paste0("P_", mt$variable[j], "no")
                mtoutput <- merge( mtyes, mtno, all = TRUE, by=0 )
                rownames(mtoutput) <- mtoutput[,1]
                mtoutput <- mtoutput[,2:3]
                
            }
            
            output <- merge( mtoutput, selection, all = TRUE, by=0 )
            output <- output[,c(1,4,2,3)]
            output[is.na(output)] <- 0
            colnames(output)[1] <- "PhenotypeValue"
            
            if( i == 1 & j == 1){
                
                resultTable              <- output
                resultTable$phenotype    <- colnames(output)[2]
                colnames(resultTable)[2] <- "P_AllPatients"
                resultTable              <- resultTable[,c(5,1:4)]
            }else{
                
                resultmidd               <- output
                resultmidd$phenotype    <- colnames(output)[2]
                colnames(resultmidd)[2] <- "P_AllPatients"
                resultmidd              <- resultmidd[,c(5,1:4)] 
                
                resultTable             <- rbind( resultTable, resultmidd )
            }
            
            output.m <- reshape2::melt(output, id.vars='PhenotypeValue')
            output.m$value <- as.numeric( as.character(output.m$value ))
            output.m$PhenotypeValue <- as.character(output.m$PhenotypeValue )
            
            
            
            p <- ggplot2::ggplot(output.m, ggplot2::aes(PhenotypeValue, value)) +   
                ggplot2::geom_bar(ggplot2::aes(fill = variable), 
                                  position = "dodge", 
                                  stat="identity", 
                                  colour = "black")
            
            p <- p + ggplot2::scale_fill_manual(values=c("grey", "#136593", "#E69F00"))
            p <- p + ggplot2::theme_classic( ) + ggplot2::theme( plot.margin = ggplot2::unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ), 
                                                                 axis.line = ggplot2::element_line ( size = 0.7, color = "black" ), text = ggplot2::element_text ( size = 14 ) ,
                                                                 axis.text.x = ggplot2::element_text ( angle = 45, size = 10, hjust = 1 ))
            
            
            #print(p, vp = vplayout(i, j))
            plots[[i]] <- p
            
        }else{
            
            message( as.character(ph$variable[i]), " phenotype is considerede as a continuous variable")
            
            for( j in 1:nrow(mt)){
                
                selection <- tt[c("patient_id", as.character(ph$check[i]), as.character(mt$check[j]))]
                mcolumn <- which(colnames(selection) == as.character(mt[j,1]))
                pcolumn <- which(colnames(selection) == as.character(ph[i,1]))
                
                
                mtyes <- selection[ selection[,mcolumn] =="yes",]
                mtno  <- selection[ selection[,mcolumn] =="no",]
                
                stats <- t.test(as.numeric(mtno[,2]), as.numeric(mtyes[,2]))
                
                bp <- ggplot2::ggplot(selection, ggplot2::aes(x=selection[,mcolumn], y= as.numeric(selection[,pcolumn]))) + 
                    ggplot2::geom_boxplot() +
                    ggplot2::stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
                    ggplot2::labs ( title = paste0( "Analysis of ", colnames(selection)[pcolumn], " vs \n", colnames(selection)[mcolumn], "\n(p-val = ", stats$p.value, " )" ) , x = "mut", y = "pheno value")
                
                bp <- bp + ggplot2::theme_classic( ) +
                    ggplot2::theme( plot.margin = ggplot2::unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ), 
                                    axis.line = ggplot2::element_line ( size = 0.7, color = "black" ), 
                                    text = ggplot2::element_text ( size = 11 ),
                                    axis.text.x = ggplot2::element_text ( angle = 45, size = 11, hjust = 1 ))
                
                #print(bp, vp = vplayout(i, j))
                plots[[i]] <- bp
            }
            
        }
        
    }
    
    multiplot(plotlist = plots, cols = 2)
    
    
    if( outputFile == TRUE){
        
        resultTable$yesno <- NA
        write.table( resultTable, file = paste0(path, "phenoSummary.txt"), 
                     col.names = TRUE, 
                     row.names = FALSE, 
                     quote     = FALSE, 
                     sep       = "\t" )
        
    }
    
    if( showTable == TRUE){
        return( resultTable )       
    }

}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}


