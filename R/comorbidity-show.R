setMethod( "show",
    signature = "comorbidity",
    definition = function( object ) {
        cat( "Object of class 'comorbidity'\n" )
        cat( " . Search:                   ", object@search, "\n" )
        cat( " . Only comorbidities between index diseases:", object@intraCode, "\n" )
        cat( " . Aggregate the disease codes:", object@aggregated, "\n" )
        cat( " . N. Input Index Diseases:  ",   object@tDiseases, "\n" )
        cat( " . N. Index Diseases Present:", object@indexDis, "\n" )
        cat( " . N. Concomintant Diseases: ",  object@nfDisease, "\n" )
        cat( " . N. Patients:              ", length( unique ( object@qresult$patient_id ) ), "\n" )
    }
)

setMethod( "show",
           signature = "phenotype",
           definition = function( object ) {
               cat( "Object of class 'phenotype'\n" )
               cat( " . N. Mutations:   ", nrow ( object@mutations ) , "\n" )
               cat( " . N. Phenotypes:  ", nrow ( object@phenotypes ), "\n" )
               cat( " . N. Patients:    ", length( unique ( object@iresult$patient_id ) ), "\n" )
           }
)

setMethod( "show",
           signature = "cAnalysis",
           definition = function( object ) {
               cat( "Object of class 'cAnalysis'\n" )
               cat( " . Age Min :", object@ageMin, "\n" )
               cat( " . Age Max :", object@ageMax, "\n" )
               cat( " . Gender  :", object@gender, "\n" )
               cat( " . Patients in the age and gender interval:", object@patients, "\n" )
               cat( " . Patients suffering the index disease(s):", object@tpatients, "\n" )
               cat( " . Disease Prevalence:", round(as.numeric(object@prevalence), 3), "\n")
               cat( " . Minimum odds ratio:", object@minimumOR, "\n")
               cat( " . Minimum relative risk:", object@minimumRR, "\n")
               cat( " . Minimum phi value:", object@minimumPhi, "\n")
               cat( " . Number of comorbidities:", object@dispairs, "\n")
           }
)


setMethod( "show",
           signature = "molecularComorbidity",
           definition = function( object ) {
               cat( "Object of class 'molecularComorbidity'\n" )
               cat( " . Search:                     ", object@search, "\n" )
               cat( " . Aggregate the disease codes:", object@aggregated, "\n" )
               cat( " . N. Input Diseases:          ", object@indexDis, "\n" )
               cat( " . N. Index Diseases Present:  ", object@nfDisease, "\n" )
               cat( " . N. Genes   :                ", object@nGenes, "\n" )
           }
)

setMethod( "show",
           signature = "molecularcAnalysis",
           definition = function( object ) {
               cat( "Object of class 'molecularcAnalysis'\n" )
               cat( " . Overlap Min :", object@ovlpMin, "\n" )
               cat( " . Overlap Max :", object@ovlpMax, "\n" )
               cat( " . Jaccard Min :", object@jaccardMin, "\n" )
               cat( " . Jaccard Max :", object@jaccardMax, "\n" )
               cat( " . P-value     :", object@pValue, "\n" )
               cat( " . Number of comorbidities:", object@dispairs, "\n")
           }
)