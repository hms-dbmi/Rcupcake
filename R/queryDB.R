createQuery <- function( pathFile ){
    
    input <- read.delim( pathFile, header = FALSE, sep= ",")
    input$V1 <- gsub( "\\\\", "/", input$V1)
    
    
    for( i in 1:nrow( input )){
        
        name<- unlist(strsplit(input$V1[i], "/"))
        name<- name[length(name)]
        name<- unlist( strsplit(name, "_"))
        name<- paste(name[2], name[3], sep = "_")
         
        if( i == 1){
            qq <- paste0( '{
                      "select": [
                          {
                          "field": {
                          "pui": "/ssc/Demo/SFARI_Simplex_Collection_v15', input$V1[i],
                          '",
                            "dataType": "STRING"
                            },
                            "alias": "P.', name, '"
                        },'
                          )
        }else if( i != nrow(input)){
            qmore <- paste0( '{
                          "field": {
                          "pui": "/ssc/Demo/SFARI_Simplex_Collection_v15', input$V1[i],
                          '",
                            "dataType": "STRING"
                            },
                            "alias": "P.', name, '"
                        },'
            )
            qq<- paste0( qq, qmore )
        }else{
            qmore <- paste0( '{
                          "field": {
                          "pui": "/ssc/Demo/SFARI_Simplex_Collection_v15', input$V1[i],
                             '",
                            "dataType": "STRING"
                            },
                            "alias": "P.', name, '"
                            },
                              {
                                        "field": {
                                                         "pui": "/ssc/Demo/SFARI_Simplex_Collection_v15/SFARI_Simplex_Collection_v15/Clinical/Demographics/SEX_CD/FEMALE",
                                                         "dataType": "STRING"
                                    },
                                                         "alias": "Gender"
                                },
                          {
                                    "field": {
                                      "pui": "/ssc/Demo/SFARI_Simplex_Collection_v15/SFARI_Simplex_Collection_v15/Clinical/Demographics/AGE_IN_YEARS_NUM/",
                                      "dataType": "STRING"
                                    },
                                    "alias": "Age"
                                  },
                            {
                                        "field": {
                                "pui": "/ssc/Demo/SFARI_Simplex_Collection_v15/SFARI_Simplex_Collection_v15/SSC_wigler_mutations/CHD8/yes",
                                "dataType": "STRING"
                                    },
                                "alias": "M.CHD8"
                            },
                            {
                                        "field": {
                                "pui": "/ssc/Demo/SFARI_Simplex_Collection_v15/SFARI_Simplex_Collection_v15/SSC_wigler_mutations/CHD8/no",
                                "dataType": "STRING"
                                    },
                                "alias": "M.CHD8"
                                }
                            ],
                             
                             "where": [
                             {
                             "field": {
                             "pui": "/ssc/Demo/SFARI_Simplex_Collection_v15/SFARI_Simplex_Collection_v15/Clinical/Demographics/AGE_IN_YEARS_NUM/",
                             "dataType": "STRING"
                             },
                             "predicate": "CONTAINS",
                             "fields": {
                             "ENOUNTER": "NO"
                             }
                             }
                             ]
                     }'

            )
            qq<- paste0( qq, qmore )
        }
        
  
    }
    write.table( qq, file= "C:/Users/AG440/Desktop/queryPrueba.txt")
    return( qq )
 
}
 

    