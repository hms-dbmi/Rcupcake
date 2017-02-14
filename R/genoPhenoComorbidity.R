#' Comorbidity Analysis \code{genophenoComor}
#'
#' Given an object of type \code{genopheno}, a comorbidity analysis is perform, 
#' for the subset of population under specific conditions of age and gender. It 
#' generates a \code{cgpAnalysis} object.
#'
#' @param input  A \code{genopheno} object, obtained with the queryPheno function. 
#' @param pth Determines the path where the required input file with 
#' the yes/no phenotype data is located.
#' @param aggregate By default TRUE. Change it to FALSE if you want to 
#' analyze the comorbidity taking into all the values of each phenotype.
#' @param ageRange Determines what is the age range of interest for
#' performing the comorbidity analysis. By default it is set from 0 to 100 
#' years old. 
#' @param gender Determine what is the gender of interest for 
#' performing the comorbidity analysis. By default \code{ALL}. Change it to the 
#' gender of interest for your comorbidity analysis.
#' @param mutation Determine what is the mutation value of interest for 
#' performing the comorbidity analysis. By default \code{c("ALL", "ALL")}. Change it to the 
#' value of interest for your comorbidity analysis. For example, \code{c("CHD8", "yes")}
#' @param nfactor By default 10. Change it into other number if you consider there is any
#' categorical variable with more than nfactor values. 
#' @param score The comorbidity score is a measure based on  the observed comorbidities
#' and the expected ones, based on the occurrence of each disease.
#' @param fdr A Fisher exact test for each pair of diseases is performed to assess 
#' the null hypothesis of independence between the two diseases. The Benjamini-Hochberg 
#' false discovery rate method (FDR) is applied to correct for multiple testing.
#' @param oddsRatio The odds ratio represents the increased chance that someone 
#' suffering disease X will have the comorbid disorder Y.
#' @param rr The relative risk refers to the fraction between the number of 
#' patients diagnosed with both diseases and random expectation based on disease 
#' prevalence.
#' @param phi The Pearsons correlation for binary variables (Phi) measures the 
#' robustness of the comorbidity association.
#' @param cores By default \code{1}. To run parallel computations on machines 
#' with multiple cores or CPUs, the cores argument can be changed. 
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @param warnings By default \code{TRUE}. Change it to \code{FALSE} to don't see
#' the warnings.
#' @return An object of class \code{cgpAnalysis}
#' @examples
#' load(system.file("extdata", "genopheno.RData", package="genophenoR"))
#' ex1 <- genoPhenoComorbidity( 
#'               input         = queryExample,
#'               pth           = system.file("extdata", package="genophenoR"),
#'               aggregate     = TRUE, 
#'               ageRange      = c(0,16),
#'               gender        = "MALE", 
#'               mutation      = c("CHD8", "ALL")
#'               )
#' @export genoPhenoComorbidity

genoPhenoComorbidity <- function ( input, pth, ageRange=c(0,100), aggregate = TRUE, gender="ALL", mutation=c("ALL", "ALL"), nfactor = 10, score, fdr, oddsRatio, rr, phi, cores = 1, verbose = FALSE, warnings = TRUE ){
    
    message("Checking the input object")
    checkClass <- class(input)[1]
    
    if(checkClass != "genopheno"){
        message("Check the input object. Remember that this
                object must be obtained after applying the queryPheno
                function to your input file. The input object class must
                be:\"genopheno\"")
        stop()
    }
    
    data <- input@iresult
    
    message( "Staring the comorbidity analysis" )
    message( "Loading the phenotype data file" )
 
    codes <- read.delim ( file.path(pth, "phenoValues.txt"),
                          header=TRUE, 
                          sep="\t", 
                          colClasses="character" ) 
    
    if( aggregate == TRUE ){

        message( "Checking the phenotype data file" )
        checkPheno <- as.data.frame( summary( as.factor( codes$yesno ) ) )
        good       <-  c("no", "yes")
        
        if( nrow( checkPheno) != 2 | 
            ! tolower( rownames( checkPheno)[1] ) %in% good |
            ! tolower( rownames( checkPheno)[2] ) %in% good){
            message("The yesno column in the phenoValues file is not filled correctly. Please, revise it,\nand check that the only possible values for this column are: yes and no.")
            stop()
        }
        
        message( "Aggregating the phenotypes values as yes/no" )
        
        for( i in 1:nrow(input@phenotypes)){
            
            pcolumn <- which(colnames(data) == as.character(input@phenotypes[i,1]))
            
            if( length( unique( data[,pcolumn])) > nfactor){
                message( colnames(data)[pcolumn], " phenotype is considered as a continuous variable. It will not be taken in to account for the comorbidity analysis")
            }else{
                codesSelection <- codes[ codes$phenotype == as.character(input@phenotypes[i,3]),c(2,6)]
                
                for( j in 1:nrow(codesSelection)){
                    data[ ,pcolumn][ data[ ,pcolumn] == codesSelection$PhenotypeValue[j]] <- codesSelection$yesno[j]
                }
                
                mt <- input@mutations
                
                if( mutation[1] %in% mt$variable ){
                    mt <- mt[ mt$variable == mutation[1], ]        
                }else{
                    message( "Your mutation of interest is not in the mutation list")
                    message( "The mutations availabe for this analysis are: ")
                    for( i in 1:nrow(mt)){
                        message("-> ", mt$variable[i])
                    }
                    stop()
                }
                
                mt <- mt[ mt$variable == mutation[1], ]  
                
                
                subset <- data[c("patient_id", "Gender", "Age", as.character(mt$check[1]), as.character(input@phenotypes[i,1]))]
                subcolumn <- which(colnames(subset) == as.character(input@phenotypes[i,1]))
                subset[,subcolumn] <- tolower(subset[,subcolumn])
                subset <- subset[ subset[as.character(input@phenotypes[i,1])] == "yes", ]
                subset[as.character(input@phenotypes[i,1])] <- input@phenotypes[i,3]
                colnames(subset)[subcolumn] <- "phenotype"
                
                if( i == 1){
                    qresult <- subset
                }else{ 
                    qresult <- rbind( qresult, subset )
                }
                
            }

            
            
        }
        
    }
    else if( aggregate == FALSE ){
        message( "For each phenotypes, all the possible values will be used")
        
        for( i in 1:nrow(input@phenotypes)){
            
            pcolumn <- which(colnames(data) == as.character(input@phenotypes[i,1]))
            
            if( length( unique( data[,pcolumn])) > nfactor){
                message( colnames(data)[pcolumn], " phenotype is considered as a continuous variable. It will not be taken in to account for the comorbidity analysis")
            }else{
                data[ ,pcolumn] <- paste0( input@phenotypes[i,3], ": " ,data[ ,pcolumn] )
                
                mt <- input@mutations
                
                if( mutation[1] %in% mt$variable ){
                    mt <- mt[ mt$variable == mutation[1], ]        
                }else{
                    message( "Your mutation of interest is not in the mutation list")
                    message( "The mutations availabe for this analysis are: ")
                    for( i in 1:nrow(mt)){
                        message("-> ", mt$variable[i])
                    }
                    stop()
                }
                    
                subset <- data[c("patient_id", "Gender", "Age", as.character(mt$check[1]), as.character(input@phenotypes[i,1]))]
                subcolumn <- which(colnames(subset) == as.character(input@phenotypes[i,1]))
                colnames(subset)[subcolumn] <- "phenotype"
                
                if( i == 1){
                    qresult <- subset
                }else{ 
                    qresult <- rbind( qresult, subset )
                }
            }
            
            
        }

    }
  
    

    if ( !missing( ageRange ) ) {
        
        naCheck <- qresult[! is.na( qresult$Age), ]
        
        if( nrow(naCheck) != nrow( qresult)){
            message("There is not age information for all the patients.")
            noAge <- nrow( qresult ) - nrow( naCheck )
            message("The ", noAge, " patients without age data will be removed")
            qresult  <- qresult[! is.na( qresult$Age), ]
            
            
        }
        
        qresult$Age <- as.numeric(qresult$Age)
        qresult <- qresult[ qresult$Age >= ageRange[ 1 ] & qresult$Age <= ageRange[ 2 ], ]
    }
    
    if ( !missing( gender ) ) {
        if(gender!="ALL"){
            qresult <- qresult[ qresult$Gender == gender, ]
        }
    }
    
    totPatients <- length( unique( qresult$patient_id ) )
    
    
    if ( !missing( mutation ) ) {
        if(mutation[2] !="ALL"){
            ncolumn <- which(colnames(qresult) == as.character(mt$check[1]))
            qresult <- qresult[ qresult[,ncolumn] == mutation[2], ]
        }
    }
    
    
    if( length( unique( qresult$phenotype)) < 2 ){
        message(paste0("Your patients subset only contains 1 phenotype: ", unique( qresult$phenotype)))
        message("Comorbidity analysis cannot be performed")
        stop()
    }
    
    else{
        ##active patients
        activePatients <- unique( qresult$patient_id )
        ##
        
        phenoPairs <- function ( pt ){
            pp <- qresult[ qresult$patient_id == pt, ]
            phenosC <- unique( pp$phenotype )
            phenos.f <- as.character( unique(pp$phenotype) )
            phenosC.c <- unique(do.call(c, apply(expand.grid(phenos.f, phenosC), 1, combn, m=2, simplify=FALSE)))
            phenos.f <- phenosC.c[sapply(phenosC.c, function(x) x[1] != x[2])]
        }
        
        message( "Generating the genophenoComor object" )
        finalCP  <- parallel::mclapply( activePatients, phenoPairs, mc.preschedule = TRUE, mc.cores = cores )
        finalCP <- finalCP[ sapply(finalCP, function(x) { length(x) != 0 }) ]
        
        f <- function( j ){ t( data.frame( j ) ) }
        unnest <-  do.call( f, list( j = finalCP  ) )
        unnest <- unnest[!duplicated(unnest), ]
        unnest <- lapply(1:nrow(unnest), function(ii) unnest[ii, ])
        
        
        
        resultado <- parallel::mclapply( unnest, tableData, mc.preschedule = TRUE, mc.cores = cores, data = qresult, lenActPa=totPatients)
        resultad2 <- do.call("rbind", resultado )
        resultad2 <- as.data.frame( resultad2, stringsAsFactors=FALSE )
        
        
        
        colnames(resultad2) <- c( "phenotypeA", "phenotypeB", "patientsPhenoA", "patientsPhenoB", "patientsPhenoAB", "patientsPhenoAnotB", "patientsPhenoABnotA", "patientsNotAnotBpheno", "fisher", "oddsRatio", "relativeRisk", "phi" )
        
        
        resultad2$expect <-  as.numeric( resultad2$patientsPhenoA ) * as.numeric( resultad2$patientsPhenoB ) / totPatients
        resultad2$score  <- log2( ( as.numeric( resultad2$patientsPhenoAB ) + 1 ) / ( resultad2$expect + 1) )
        resultad2        <- resultad2[ with( resultad2, order( resultad2$fisher ) ), ]
        resultad2$fdr    <- p.adjust( as.numeric( resultad2$fisher ), method = "fdr", n = nrow( resultad2 ) )
        
        resultad2$pair   <- NA
        for(cont in 1:nrow(resultad2)){
            pairDis <- sort(c(resultad2$phenotypeA[cont], resultad2$phenotypeA[cont]))
            resultad2$pair[cont] <- paste(pairDis[1], pairDis[2], sep="*")
        }
        
        resultad2 <- resultad2[!duplicated(resultad2$pair),]
        resultad2 <- resultad2[,c(1:15)]
        
        if ( !missing( score ) ) {
            resultad2 <- resultad2[ resultad2$score > score, ]
        }
        if ( !missing( fdr ) ) {
            resultad2 <- resultad2[ resultad2$fdr < fdr, ]
        }
        if ( !missing( oddsRatio ) ) {
            resultad2 <- resultad2[ resultad2$fdr > oddsRatio, ]
        }
        if ( !missing( rr ) ) {
            resultad2 <- resultad2[ resultad2$fdr > rr, ]
        }
        if ( !missing( phi ) ) {
            resultad2 <- resultad2[ resultad2$fdr > phi, ]        
        }
        
        resultad2$fisher <- round(as.numeric(resultad2$fisher), 3)
        resultad2$oddsRatio <- round(as.numeric(resultad2$oddsRatio), 3)
        resultad2$relativeRisk <- round(as.numeric(resultad2$relativeRisk), 3)
        resultad2$phi <- round(as.numeric(resultad2$phi), 3)
        resultad2$expect <- round(as.numeric(resultad2$expect), 3)
        resultad2$score <- round(as.numeric(resultad2$score), 3)
        resultad2$fdr <- round(as.numeric(resultad2$fdr), 3)
        
        if( nrow( resultad2 ) == 0 ){
            warning("None of the disease pairs has pass the filters") 
        }
        
        genophenoComor <- new( "genophenoComor", 
                               ageMin    = ageRange[ 1 ], 
                               ageMax    = ageRange[ 2 ], 
                               gender    = gender, 
                               patients  = totPatients,
                               tpatients = length(activePatients),
                               prevalence= (length(activePatients)/totPatients)*100,
                               minimumOR = round(min(as.numeric(resultad2$oddsRatio)), digits = 3),
                               minimumRR = round(min(as.numeric(resultad2$relativeRisk)), digits = 3),
                               minimumPhi= round(min(as.numeric(resultad2$phi)), digits = 3),
                               dispairs  = nrow( resultad2 ),
                               result    = resultad2 
        )
        return( genophenoComor )
    
            
    }
    
}

tableData <- function ( pairCode, data, lenActPa ) {
    
    code1 <- pairCode[[ 1 ]]
    code2 <- pairCode[[ 2 ]]
    
    dis1 <- data[ data$phenotype == code1, ]
    dis2 <- data[ data$phenotype == code2, ]
    
    dis12 <- dis2[ dis2$patient_id %in% dis1$patient_id, ]
    
    disAcode <- code1
    disBcode <- code2
    disA     <- length( unique ( dis1$patient_id ) )
    disB     <- length( unique ( dis2$patient_id ) )
    AB       <- length( unique ( dis12$patient_id ) )
    AnotB    <- disA - AB
    BnotA    <- disB - AB
    notAB    <- lenActPa - AB - AnotB - BnotA
    
    mm <- matrix( c( AB, AnotB, BnotA, notAB), nrow = 2 )
    
    tryCatch( {ff <- fisher.test( mm )}, error=function(msg) {
        message(msg)
        message("code1:", code1, " - code2:", code2)
    })
    
    relativeRisk <- as.numeric(AB*lenActPa)/as.numeric(disA* disB)
    den <- as.numeric(disA*disB)*as.numeric(lenActPa-disA)*as.numeric(lenActPa-disB)
    num <- as.numeric(AB*lenActPa)-as.numeric(disA*disB)
    phi <- ((num)/sqrt(den))
    
    c( disAcode, disBcode, disA, disB, AB, AnotB, BnotA, notAB, ff$p.value, ff$estimate, relativeRisk, phi )    
    
}









