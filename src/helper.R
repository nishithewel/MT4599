library(readxl)

remove_duplicate <- function(matrix) {
    col_nums <- which(duplicated(as.list(as.data.frame(matrix))))
    return(matrix[, -col_nums])
}



# test for duplicated
test_remove_duplicate <- function() {
    test <- matrix(c(rep(1, 15), seq(1, 15)), nrow = 5)
    # which(duplicated(as.list(as.data.frame(test))))
    # remove the duplicates
    test[, -which(duplicated(as.list(as.data.frame(test))))]
    test <- remove_duplicate(test)
    # test
}

# this is wrong as it doesnt
get_interactions <- function(df) {
    interaction_fields <- list()
    for (pillar in c("Environmental", "Governance", "Social")) {
        # print(pillar)
        pillar_fields <- field_df %>%
            filter(
                # partial_match_filter(colnames(df),field_df$`RDP API Field Name` )
                # str_detect(`RDP API Field Name`, paste(colnames(df), collapse = "|"))
                `RDP API Field Name` %in% colnames(df) &
                    Pillar == pillar &
                    # check if title is not a controversy score
                    !str_detect(Title, "Controv")
            ) %>%
            pull(`RDP API Field Name`)

        interaction_fields[[pillar]] <- combn(pillar_fields, 2,
            FUN = paste,
            collapse = "*"
        )
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

generate_interaction_formula <- function(df, num_interaction = "all") {
    # Get the names of the columns
    col_names <- colnames(df)

    # Create a list of all possible combinations of column names
    col_combinations <- combn(col_names, 2,
        FUN = paste,
        collapse = "*"
    )
    if (num_interaction != "all") {
        interactions <- col_combinations[1:num_interaction]
    } else {
        interactions <- col_combinations
    }

    # # Create a list of formulas for each column combination
    # formula_list <- lapply(col_combinations, function(cols) {
    #     paste(cols[[1]], "*", cols[[2]])
    # })
    #
    # # Combine all formulas into a single formula with "+" separators
    # interaction_formula <- paste(formula_list, collapse = "+")
    #
    # # Add the main effect terms to the formula
    # main_effect_terms <- paste(col_names, collapse = "+")
    # interaction_formula <- paste(main_effect_terms, interaction_formula, sep = "+")

    # Convert the formula string to an R formula object

    comb_str <- paste(interactions, sep = " ", collapse = " + ")
    # interaction_formula <- as.formula(comb_str)

    return(comb_str)
}

# create df of col names

create_within_interactions <- function(df, map_df, limit = F) {
    # creats within pillar interactions
    # returns a string with all the interactions with * format
    col_df <- data.frame(
        col_names = colnames(df)
    )
    temp <- col_df %>%
        mutate(
            TR.name = str_extract(col_names, "TR.+")
        ) %>%
        left_join(map_df, by = join_by(TR.name == `RDP API Field Name`))

    interactions <- list()
    for (pillar in c("Environmental", "Governance", "Social")) {
        colbypillar <- temp %>%
            filter(
                Pillar == pillar
            ) %>%
            pull(col_names)

        col_combinations <- combn(colbypillar, 2,
            FUN = paste,
            collapse = "*"
        )
        interactions[[pillar]] <- col_combinations
    }
    if (limit != F) {
        comb_str <- paste(unlist(interactions)[1:limit], sep = " ", collapse = " + ")
    } else {
        comb_str <- paste(unlist(interactions), sep = " ", collapse = " + ")
    }
    # interaction_formula <- as.formula(comb_str)

    return(comb_str)
}

create_linear_predictor <- function(df) {
    # case 1: No interactions
    xmatrix_no <- model.matrix(y ~ .,
        data = df
    )[, -1]


    # case 2: Withi pillar interactions
    field_df <- read_xlsx("../docs/Refinitiv ESG docs/esg_data_guide.xlsx",
        sheet = "Field Description"
    )
    f2 <- paste("y ~", create_within_interactions(df = df, map_df = field_df))

    xmatrix_within <- model.matrix(formula(f2),
        data = df
    )[, -1]

    # Case 3: All interactions
    xmatrix_all <- model.matrix(y ~ .^2,
        data = df
    )[, -1]

    return(
        list(
            x_no_interaction = xmatrix_no,
            x_pillar_interaction = xmatrix_within,
            x_all_interaction = xmatrix_all
        )
    )
}
