setClass( "genopheno",
          representation =
              representation( 
                  nMutations   = "numeric",    # number of mutations
                  nPhenotype   = "numeric",    # number of phenotypes
                  nPatient     = "numeric",    # number of patients
                  iresult      = "data.frame",  # result
                  mutations    = "data.frame", # mutations
                  phenotypes   = "data.frame" # phenotypes
              ),
          prototype = 
              prototype( nMutations  = numeric(),
                         nPhenotype  = numeric(),
                         nPatient    = numeric(),
                         iresult     = data.frame(),
                         mutations     = data.frame(),
                         phenotypes     = data.frame()
              )
)


setClass( "genophenoComor",
          representation =
              representation( ageMin    = "numeric",  # single or list
                              ageMax    = "numeric",    # max age
                              gender    = "character",  # gender
                              mutation  = "character",  #mutation
                              patients  = "numeric",    # subsetPatients
                              tpatients = "numeric",    # totalPatients
                              prevalence= "numeric",    # prevalence respet to the total population
                              minimumOR = "numeric",    # minimum value of the OR
                              minimumRR = "numeric",    # minimum value of the relative risk
                              minimumPhi = "numeric",    # minimum value of the phi value
                              dispairs  = "numeric",    # number of pairs
                              result    = "data.frame"  # result
              ),
          prototype = 
              prototype( ageMin    = numeric(),
                         ageMax    = numeric(),
                         gender    = character(),
                         mutation  = character(),
                         patients  = numeric(),
                         tpatients = numeric(),
                         prevalence= numeric(),
                         minimumOR = numeric(),
                         minimumRR = numeric(),
                         minimumPhi= numeric(),
                         dispairs  = numeric(),
                         result    = data.frame()
              )
)