# library(RCurl)
# library(jsonlite)
# 
# apiKey <- "f8i07tqmjink3l2umpj3v2fhdu"
# IRCT_REST_BASE_URL <- "https://nhanes.hms.harvard.edu/"
# IRCT_CL_SERVICE_URL <- paste(IRCT_REST_BASE_URL,"rest/v1/",sep="")
# IRCT_RESOURCE_BASE_URL <- paste(IRCT_CL_SERVICE_URL,"resourceService/",sep="")
# IRCT_PATH_RESOURCE_URL <-
#     paste(IRCT_RESOURCE_BASE_URL, "path", sep = "")
# IRCT_LIST_RESOURCE_URL <-
#     paste(IRCT_RESOURCE_BASE_URL, "resources", sep = "")
# 
# startSession <-
#     httr::content(httr::GET(
#         paste0(
#             IRCT_REST_BASE_URL,
#             "/rest/v1/securityService/startSession?key=",
#             apiKey
#         )
#     ))
# startSession
# 
# getchildren <- function(children, fieldname) {
#     
#     nexturl <- paste(IRCT_PATH_RESOURCE_URL, fieldname, sep = "")
#     newchildren <- httr::content(httr::GET(nexturl))
#     if (length(newchildren) > 0) {
#         for (i in 1:length(newchildren)) {
#             res = tryCatch({
#                 newchild <- newchildren[[i]]$pui
#                 children <- c(children, newchild)
#                 getchildren(children, 
#                     gsub("\\#","%23", gsub("\\?", "%3F", gsub("[)]","%29", gsub("[(]","%28", URLencode(newchild)))))
#                 )
#             }, error = function(errorCondition) {
#                 message("ERROR: There is something wrong with the children")
#                 message(newchildren)
#             })
#         }
#     } else {
#         #message("Ending loop. No more children.")
#     }
#     return(children)
# }
# 
# 
# myquery <- function(myfields, myvector) {
#     
#     # Create list of paths from the 'vector' list
#     # which contains all available paths for the resource
#     pathList <- grep(myfields, vector, value=TRUE)
#     
#     # Get all the fields available from the pathlist
#     querySELECT<- c()
#     for (i in 1:length(pathList)) {
#         message( pathList[i])
#         
#         #Get the fields for this particluar path
#         nurlstr <- paste(IRCT_PATH_RESOURCE_URL, pathList[i], sep = "")
#         nurl <- gsub("\\#","%23", gsub("\\?", "%3F", gsub("[)]","%29", gsub("[(]","%28", URLencode(nurlstr)))))
#         
#         pathFields <- httr::content(httr::GET(nurl))
# 
#         # Generate {field} object for SELECT portion of the query
#         for (j in 1:length(pathFields)) {
#             message(pathFields[j])
#             
#             myField <- list(
#                 list(
#                     field=list(
#                         pui = unbox(pathFields[[j]]$pui), 
#                         dataType = unbox(pathFields[[j]]$dataType$name)
#                     ), 
#                     alias=unbox(
#                         pathFields[[j]]$displayName
#                     )
#                 )
#             )
#             querySELECT <- c(querySELECT, (myField))
#         }
#     }
#     
#     # Generate WHERE portion of the query, from the
#     # first path selected.
#     queryWHERE <- c()
#     # Assuming STRING variable, but not sure
#     mylist = list(pui = unbox(pathList[1]), datatype = unbox("STRING"))
#     queryWHERE <- list(field=mylist, predicate=unbox("CONTAINS"), fields=list(ENCOUNTER=unbox("NO")))
#     querySTRING <- list(select=querySELECT, where=list(queryWHERE))
#     
#     return(toJSON(querySTRING, pretty=TRUE))
# }
# 
# vector1 <- getchildren(c(),'/nhanes/Demo/laboratory/laboratory/pcbs/')
# vector2 <- getchildren(c(),'/nhanes/Demo/demographics/demographics/')
# vector  <- c( vector1, vector2 )
# # Generate query, based on selected criteria
# query_as_a_sting <- myquery(myfields = 'AGE|PCB153', myvector = vector)
# query_as_a_sting2 <- myquery('AGE|SEX|pcbs', vector)
