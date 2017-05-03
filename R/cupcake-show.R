setMethod( "show",
           signature = "cupcakeData",
           definition = function( object ) {
               cat( "Object of class 'cupcakeData'\n" )
               cat( " . N. Variations:   ", nrow ( object@variations ) , "\n" )
               cat( " . N. Phenotypes:  ", nrow ( object@phenotypes ), "\n" )
               cat( " . N. Patients:    ", length( unique ( object@iresult$patient_id ) ), "\n" )
           }
)

setMethod( "show",
           signature = "cupcakeResults",
           definition = function( object ) {
               cat( "Object of class 'cupcakeResults'\n" )
               cat( " . Age Min :", object@ageMin, "\n" )
               cat( " . Age Max :", object@ageMax, "\n" )
               cat( " . Gender  :", object@gender, "\n" )
               cat( " . Variation :", object@variation, "\n" )
               cat( " . Patients in the age and gender interval:", object@patients, "\n" )
               cat( " . Patients with vairation selected:", object@tpatients, "\n" )
               cat( " . Prevalence:", round(as.numeric(object@prevalence), 3), "\n")
               cat( " . Odds ratio range:", object@ORrange, "\n")
               cat( " . Relative risk range:", object@RRrange, "\n")
               cat( " . Phi range:", object@PHIrange, "\n")
               cat( " . Number of comorbidities:", object@dispairs, "\n")
           }
)