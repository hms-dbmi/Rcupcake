setMethod( "show",
           signature = "genopheno",
           definition = function( object ) {
               cat( "Object of class 'genopheno'\n" )
               cat( " . N. Mutations:   ", nrow ( object@mutations ) , "\n" )
               cat( " . N. Phenotypes:  ", nrow ( object@phenotypes ), "\n" )
               cat( " . N. Patients:    ", length( unique ( object@iresult$patient_id ) ), "\n" )
           }
)

setMethod( "show",
           signature = "genophenoComor",
           definition = function( object ) {
               cat( "Object of class 'genophenoComor'\n" )
               cat( " . Age Min :", object@ageMin, "\n" )
               cat( " . Age Max :", object@ageMax, "\n" )
               cat( " . Gender  :", object@gender, "\n" )
               cat( " . Patients in the age and gender interval:", object@patients, "\n" )
               cat( " . Patients with mutation selected:", object@tpatients, "\n" )
               cat( " . Prevalence:", round(as.numeric(object@prevalence), 3), "\n")
               cat( " . Minimum odds ratio:", object@minimumOR, "\n")
               cat( " . Minimum relative risk:", object@minimumRR, "\n")
               cat( " . Minimum phi value:", object@minimumPhi, "\n")
               cat( " . Number of comorbidities:", object@dispairs, "\n")
           }
)