#' Describes the phenotypic characteristics for the whole study population then according to the 
#' status regarding one selected gene
#'
#' Given an object of class \code{genopheno}, a file with the different values for each phenotype, 
#' and the prevalence of each one in general population and according to the gene status selected
#' is generated. A figure containing a barplot for each phenotype is displayed. Each barplot shows 
#' the population percentage suffering each type of the phenotypes according to the values it takes, 
#' and distinguishing between those having or not a mutation. Furthermore, a data.frame with the 
#' numerical values is obtained.  
#'
#' @param input Object of \code{genopheno} class. 
#' @param nfactor By default 10. Change it into other number if you consider there is any
#' categorical variable with more than \code{nfactor} values. 
#' @param mutation Determines the mutation of interest for wchich you want to analyze the 
#' phenotype values. By default FALSE. 
#' @param showTable By default TRUE. Change it into FALSE in order to not visualize the table
#' with the ressults. 
#' @param showFigures By default FALSE. Change it into TRUE in order to visualize the table
#' with the ressults. 
#' @param path By default the working directory. Define the path where you want the file to 
#' be saved
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @return A file .
#' @examples
#' load(system.file("extdata", "genophenoExData.RData", package="genophenoR"))
#' phenotypeSummary( input       = genophenoExData, 
#'                   showTable   = TRUE, 
#'                   showFigures = TRUE, 
#'                  verbose      = FALSE 
#'            )
#' @export phenotypeSummary

phenotypeSummary <- function( input, mutation = FALSE, nfactor = 10, showTable = TRUE, showFigures = FALSE, path = getwd(), verbose = FALSE ){
    
    
    if( verbose == TRUE){
        message("Checking the input object")
    } 
    
    
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
    
    
    if( mutation == FALSE ){
        message("No genomic information will be taken into account")
    }else if( mutation %in% mt$variable ){
        mt <- mt[ mt$variable == mutation, ]        
    }else{
        message( "Your mutation of interest is not in the mutation list")
        message( "The mutations availabe for this analysis are: ")
        for( i in 1:nrow(mt)){
            message("-> ", mt$variable[i])
        }
        stop()
    }
    
    if( verbose == TRUE ){
        message( "A graphic will be generated for each phenotype" )
    }
    
    #grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(ph), nrow(mt))))
    #vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y)
    plots <- list()
    
    for( i in 1:nrow( ph )){
        
        pcolumn <- which(colnames(tt) == as.character(ph[i,1]))
        
        if( length( unique( tt[,pcolumn])) <= nfactor){
            
            if( verbose == TRUE ){
                message( as.character(ph$variable[i]), " phenotype is considered as a categorical variable")
            }
            
            selection <- as.data.frame(summary(as.factor(tt[,pcolumn])))
            selection$perc <- round(100*selection[,1] / length(unique(tt$patient_id)), 2)
            
            for( cont in 1:nrow(selection)){
                confint <-  binom.test( selection[cont,1], length(unique(tt$patient_id)))$conf.int
                selection$confint[cont] <- paste0("[", round(confint[[1]]*100,1),"-",round(confint[[2]]*100,1), "]")
                
            }
            
            colnames(selection)[1] <- ph$variable[i]
            
            
            if( mutation == FALSE){
                
                output <- selection[, c(1,2)]
                output$PhenotypeValue <- rownames( output )
                output <- output[, c(3, 2)]
                
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
                                                                     axis.line = ggplot2::element_line ( size = 0.7, color = "black" ), text = ggplot2::element_text ( size = 11 ) ,
                                                                     axis.text.x = ggplot2::element_text ( angle = 45, size = 10, hjust = 1 ))
                
                
                #print(p, vp = vplayout(i, j))
                plots[[i]] <- p
                
                if( i == 1 ){
                    resultTable                <- selection
                    resultTable$phenotypeValue <- rownames(selection)
                    resultTable$phenotype      <- colnames(selection)[1]
                    colnames(resultTable)[2]   <- "P_AllPatients"
                    resultShowTable            <- resultTable[,c(5,4,2,3)]
                    
                    
                }else{
                    
                    resultmidd                <- selection
                    resultmidd$phenotypeValue <- rownames(selection)
                    resultmidd$phenotype      <- colnames(selection)[1]
                    colnames(resultmidd)[2] <- "P_AllPatients"
                    output                  <- resultmidd[,c(5,4,2,3)]
                    
                    resultShowTable         <- rbind( resultShowTable, output )
                    
                }
                
            }
            
            else{
                
                for( j in 1:nrow(mt)){
                    
                    mcolumn <- which(colnames(tt) == as.character(mt[j,1]))
                    mtyes <- tt[ tt[,mcolumn] =="yes",]
                    
                    mtyesTable <- as.data.frame(summary(as.factor(mtyes[,pcolumn])))
                    mtyesTable$perc <- round(100*mtyesTable[,1] / length(unique(mtyes$patient_id)), 2)
                    
                    for( cont in 1:nrow(mtyesTable)){
                        confint <-  binom.test( mtyesTable[cont,1], length(unique(mtyes$patient_id)))$conf.int
                        mtyesTable$confint[cont] <- paste0("[", round(confint[[1]]*100,1),"-",round(confint[[2]]*100,1), "]")
                        
                    }
                    
                    colnames(mtyesTable) <- c( paste0("P_", mt$variable[j], "yes"), "P_yes", "CI_yes")
                    
                    
                    
                    mtno  <- tt[ tt[,mcolumn] =="no",]
                    
                    mtnoTable <- as.data.frame(summary(as.factor(mtno[,pcolumn])))
                    mtnoTable$perc <- round(100*mtnoTable[,1] / length(unique(mtno$patient_id)), 2)
                    
                    for( cont in 1:nrow(mtnoTable)){
                        confint <-  binom.test( mtnoTable[cont,1], length(unique(mtno$patient_id)))$conf.int
                        mtnoTable$confint[cont] <- paste0("[", round(confint[[1]]*100,1),"-",round(confint[[2]]*100,1), "]")
                        
                    }
                    
                    colnames(mtnoTable) <- c( paste0("P_", mt$variable[j], "no"), "P_no", "CI_no")
                    
                    mtoutput <- merge( mtyesTable, mtnoTable, all = TRUE, by=0 )
                    rownames(mtoutput) <- mtoutput[,1]
                    mtoutput <- mtoutput[,2:7]
                    
                }
                
                output <- merge( mtoutput, selection, all = TRUE, by=0 )
                output <- output[,c(1,8,9,10,2:7)]
                output[is.na(output)] <- 0
                colnames(output)[1] <- "PhenotypeValue"
                
                if( i == 1 & j == 1){
                    
                    resultTable              <- output
                    resultTable$phenotype    <- colnames(output)[2]
                    colnames(resultTable)[2] <- "P_AllPatients"
                    resultTable              <- resultTable[,c(11,1,3,6,9)]
                    
                    resultShowTable          <- output
                    resultShowTable[,2]      <- colnames(resultShowTable)[2]
                    colnames(resultShowTable)[2] <- "phenotype"
                    
                }else{
                    
                    resultmidd               <- output
                    resultmidd$phenotype    <- colnames(output)[2]
                    colnames(resultmidd)[2] <- "P_AllPatients"
                    resultmidd              <- resultmidd[,c(11,1,3,6,9)] 
                    
                    
                    output[,2]      <- colnames(output)[2]
                    colnames(output)[2] <- "phenotype"
                    resultShowTable         <- rbind( resultShowTable, output )
                    
                }
                
                output4plot <- output[,c(1, 3, 6, 9)]
                output.m <- reshape2::melt(output4plot, id.vars='PhenotypeValue')
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
                
            }
        }else if( length( unique( tt[,pcolumn])) > nfactor){
            
            if( verbose == TRUE ){
                
                message( as.character(ph$variable[i]), " phenotype is considerede as a continuous variable")        
            }
            
            
            
            if( mutation == FALSE){
                message("No plot is available for this variable")        
                
            } else{
                
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
    }
    
    
    #multiplot(plotlist = plots, cols = 2)
    
    if( showFigures == TRUE ){
        for(i in 1:length(plots)){
            print( plots[[i]] ) 
        }
    }
        

    resultShowTable$yesno <- NA
    
    write.table( resultShowTable, file = paste0(path, "/phenoSummary.txt"), 
                 col.names = TRUE, 
                 row.names = FALSE, 
                 quote     = FALSE, 
                 sep       = "\t" )
    
    
    if( showTable == TRUE){
        return( resultShowTable )       
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


