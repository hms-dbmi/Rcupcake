setClass( "cupcakeData",
          representation =
              representation( 
                  nVariations  = "numeric",    # number of variations
                  nPhenotype   = "numeric",    # number of phenotypes
                  nPatient     = "numeric",    # number of patients
                  iresult      = "data.frame", # result
                  variations   = "data.frame", # variations
                  phenotypes   = "data.frame"  # phenotypes
              ),
          prototype = 
              prototype( nVariations = numeric(),
                         nPhenotype  = numeric(),
                         nPatient    = numeric(),
                         iresult     = data.frame(),
                         variations  = data.frame(),
                         phenotypes  = data.frame()
              )
)


setClass( "cupcakeResults",
          representation =
              representation( ageMin    = "numeric",    # single or list
                              ageMax    = "numeric",    # max age
                              gender    = "character",  # gender
                              variation = "character",  # variation
                              patients  = "numeric",    # subsetPatients
                              tpatients = "numeric",    # totalPatients
                              prevalence= "numeric",    # prevalence respet to the total population
                              ORrange   = "character",  # OR range 
                              RRrange   = "character",  # relative risk range 
                              PHIrange  = "character",  # phi range
                              dispairs  = "numeric",    # number of pairs
                              result    = "data.frame"  # result
              ),
          prototype = 
              prototype( ageMin    = numeric(),
                         ageMax    = numeric(),
                         gender    = character(),
                         variation = character(),
                         patients  = numeric(),
                         tpatients = numeric(),
                         prevalence= numeric(),
                         ORrange   = character(),
                         RRrange   = character(),
                         PHIrange  = character(),
                         dispairs  = numeric(),
                         result    = data.frame()
              )
)