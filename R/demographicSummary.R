#' Describes the demographic characteristics (sex, age) of the population under study
#'
#' Given an object of class \code{genopheno}, and the characters used to 
#' specify the gender, a graphic containing 3 plots, one of the age distribution, 
#' another one of the age distribution and the third one with the relation 
#' between age and gender distribution, is obtained. 
#'
#' @param input Object of \code{genopheno} class. 
#' @param maleCode Characters(s) used to determine the male condition of a patient. 
#' Depending on the database it can be determined, for example, as \code{Male}, .
#' \code{MALE}, \code{M}, with digits as \code{0} or \code{1}. 
#' @param femaleCode Characters(s) used to determine the female condition of a patient. 
#' Depending on the database it can be determined, for example, as \code{Female}, .
#' \code{FEMALE}, \code{F}, with digits as \code{0} or \code{1}. 
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @param warnings By default \code{TRUE}. Change it to \code{FALSE} to don't see
#' the warnings.
#' @return A multiple graph containing a barplot with age distribution,  
#' a boxplot representing age distribution by gender and a pie chart representing 
#' gender distribution.
#' @examples
#' load(system.file("extdata", "genophenoExData.RData", package="genophenoR"))
#' demographicSummary( input = genophenoExData, 
#'                      maleCode   = "MALE", 
#'                      femaleCode = "FEMALE"
#'            )
#' @export demographicSummary



demographicSummary <- function( input , maleCode, femaleCode, verbose = FALSE, warnings = TRUE) {
    
    
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
    
    
    if ( missing( maleCode ) | missing( femaleCode ) ) {
        message("'maleCode' or 'femlaeCode' argument is missing. 
                Please, enter the 'maleCode' and 'femaleCode' argument.")
        stop()        
    }
    

    if( verbose == TRUE ){
        message( "Creating a summary table with the main characteristics of population" )
    }
    
    tt <- input@iresult
    
    male<- tt[ tt$Gender == maleCode, ]
    female<- tt[ tt$Gender == femaleCode, ]
    
    ### sex distribution
    
    if( verbose == TRUE){
        message("Generating the sex distribution plot")
    } 
    
    slices<-c(length ( unique( female$patient_id ) ), length ( unique( male$patient_id ) ) )
    lbl1 <- paste0(round((length (unique( female$patient_id ) ) ) /length ( unique( tt$patient_id ) ) *100, 2 ), "%")
    lbl2 <- paste0(round((length (unique( male$patient_id ) ) ) /length ( unique( tt$patient_id ) ) *100, 2 ), "%")
    
    grp1 <- paste("Female (",lbl1, ")" )    
    grp2 <- paste("Male (",lbl2, ")" )
    
    
    df <- data.frame(
        group = c( grp1, grp2 ),
        value = c(slices[1], slices[2])
    )
    
    bp2<- ggplot2::ggplot(df, ggplot2::aes(x="", y=value, fill=group))+
        ggplot2::geom_bar(width = 1, stat = "identity")
    
    
    blank_theme <- ggplot2::theme_minimal()+
        ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            panel.grid   = ggplot2::element_blank(),
            axis.ticks   = ggplot2::element_blank(),
            plot.title   = ggplot2::element_text(size=14, face="bold")
        )
    
    pie <- bp2 + ggplot2::coord_polar("y", start=0)
    pie <- pie + ggplot2::scale_fill_grey(start = 0.4, end = 0.8, na.value = "red") +  
        blank_theme + 
        ggplot2::theme(axis.text.x = ggplot2::element_blank())
    ##
    
    
    tt$Age    <- as.numeric( tt$Age )
    ttNotNA <- tt[! is.na( tt$Age), ]
    
    if( nrow( tt ) != nrow( ttNotNA ) ){
        message(paste0("From the initial ", nrow(tt), " patients, there are \n",
                nrow( ttNotNA ), " for which age information is available"))
    
    }
    

    ### age distribution
    
    if( verbose == TRUE){
        message("Generating the age distribution plot")
    } 
    
    p <-ggplot2::ggplot( ttNotNA ) +
        ggplot2::geom_bar( ggplot2::aes(Age) ) +
        ggplot2::labs ( title = "Age distribution" , x = "age", y = "# of patients") +
        ggplot2::scale_x_continuous(breaks = seq(min(ttNotNA$Age), max(ttNotNA$Age), by= 3),labels = seq(min(ttNotNA$Age), max(ttNotNA$Age), by=3))
    
    
    p <- p + ggplot2::theme_classic( ) + ggplot2::theme( plot.margin = ggplot2::unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ), 
                                                         axis.line = ggplot2::element_line ( size = 0.7, color = "black" ), text = ggplot2::element_text ( size = 14 ) ,
                                                         axis.text.x = ggplot2::element_text ( angle = 45, size = 9, hjust = 1 ))
    
    #Boxplot
    
    if( verbose == TRUE){
        message("Generating the plot showing age distribution according to gender")
    } 
    
    maleNotNA<- ttNotNA[ ttNotNA$Gender == maleCode, ]
    femaleNotNA<- ttNotNA[ ttNotNA$Gender == femaleCode, ]
    
    male1<-maleNotNA[c("patient_id", "Age", "Gender")]
    female1<-femaleNotNA[c("patient_id", "Age", "Gender")]
    
    tot<-rbind(male1, female1)
    tot$Age <- as.numeric( tot$Age )
    stats <- t.test(as.numeric(female$Age), as.numeric(male$Age))
    
    bp <- ggplot2::ggplot(tot, ggplot2::aes(x=Gender, y=Age)) + 
        ggplot2::geom_boxplot() +
        ggplot2::stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
        ggplot2::labs ( title = paste0("Age distribution by Gender\n (T-test p-val:", round(stats$p.value, 3), ")") , x = "gender", y = "age")
    
    bp <- bp + ggplot2::theme_classic( ) +
        ggplot2::theme( plot.margin = ggplot2::unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ), 
                        axis.line = ggplot2::element_line ( size = 0.7, color = "black" ), 
                        text = ggplot2::element_text ( size = 11 ),
                        axis.text.x = ggplot2::element_text ( angle = 45, size = 11, hjust = 1 ))
    
    
    
    #plot them together
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))
    vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y)
    print(p, vp = vplayout(1, 1:2))
    print(bp, vp = vplayout(2, 1))
    print(pie, vp = vplayout(2, 2))
}



