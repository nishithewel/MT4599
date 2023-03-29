remove_duplicate <- function(matrix){
    col_nums <- which(duplicated(as.list(as.data.frame(matrix))))
    return(matrix[,-col_nums])
}



#test for duplicated
test_remove_duplicate <- function(){
    test <- matrix(c(rep(1,15),seq(1,15)),nrow=5)
    # which(duplicated(as.list(as.data.frame(test))))
    #remove the duplicates
    test[,-which(duplicated(as.list(as.data.frame(test))))]
    test <- remove_duplicate(test)
    # test
    
}

# this is wrong as it doesnt 
get_interactions <- function(df){
    interaction_fields <- list()
    for (pillar in c("Environmental", "Governance", "Social")){
        # print(pillar)
        pillar_fields <- field_df %>% 
            filter(
                # partial_match_filter(colnames(df),field_df$`RDP API Field Name` ) 
                # str_detect(`RDP API Field Name`, paste(colnames(df), collapse = "|"))
                `RDP API Field Name` %in% colnames(df)
                   &
                       Pillar == pillar & 
                       #check if title is not a controversy score
                       !str_detect(Title, "Controv")
            ) %>%
            pull(`RDP API Field Name`)
        
        interaction_fields[[pillar]] <-  combn(pillar_fields, 2, FUN = paste, 
                                               collapse = '*')
    }
    return(interaction_fields)
}

# interactions <- get_interactions(complete_data)


partial_match_filter <- function(full_vec, partial_vec) {
    # Use grepl() to create a logical vector indicating which elements of full_vec 
    # partially match any element of partial_vec
    match_logical <- grepl(paste(partial_vec, collapse = "|"), full_vec)
    
    # Subset full_vec based on the logical vector
    filtered_vec <- full_vec[match_logical]
    
    # Return the filtered vector
    return(match_logical)
}

# partial_match_filter(colnames(complete_data), field_df$`RDP API Field Name`
#                      )
