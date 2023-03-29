library(tidyverse)
library(readxl)
library(naniar)
library(mice)

source("helper.R")

set.seed(20005828)

main <- function() {
  # load dataframes

  # contains mapping for categories
  field_df <- read_xlsx("../docs/Refinitiv ESG docs/esg_data_guide.xlsx",
    sheet = "Field Description"
  )

  df <- read_xlsx("data/FTSE100.xlsx") %>%
    # remove index
    dplyr::select(-1) %>%
    # select a single year
    filter(row_number() %in% seq(1, nrow(.), 2))

  df <- rename_cols(df, field_df)

  df <- df %>% 
    select(-which(colnames(.) %in% grep("Score", colnames(.), value = TRUE)))  %>%
    select(-Instrument)
  
  # extract cols with the word "Controv" in the column name
  controv_cols <- df %>%
    select(which(colnames(.) %in% grep("Controv", colnames(.), value = TRUE)))

  # remove columns where the column name contains a space
  df <- df %>%
    select(-which(colnames(.) %in% grep(" ", colnames(.), value = TRUE))) %>%
    # remove columns with the word "Score" and "Controv" in them
    select(-which(colnames(.) %in% grep("Controv", colnames(.), value = TRUE))) %>%
    # remove columns with a 100% missing value using dplyr
    select(-which(colSums(is.na(.)) == nrow(.)))



  # convert columns to datatype
  numeric_cols <- map_dtype_field(field_df, df, c("Float","Money","Integer") )
  date_cols <- map_dtype_field(field_df, df, "Date")
  str_cols <- map_dtype_field(field_df, df, "String")
  bool_cols <- map_dtype_field(field_df, df, "Boolean")


  df <- df %>%
    mutate(across(str_cols, as.factor)) %>%
    mutate(across(bool_cols, as.factor)) %>%
    mutate(across(date_cols, as.Date)) %>%
    mutate(across(numeric_cols, as.numeric)) %>%

  #drop the date cols
    select(-date_cols)



  # str_cols <- df %>% select_if(is.character)

  # str_to_fct <- names(str_cols[str_cols %>%
  #   map(unique) %>%
  #   map(length) < 6])

  # drop factor columns with more than 2 levels
  df <- df %>%
    select(-which(sapply(., is.factor) & sapply(., nlevels) > 2))

  # df <- df %>%

  #     mutate(across(str_to_fct, as.factor)) %>%
  #     mutate(across(bool_cols, as.factor))


  # create missing indicator columns
  df <- create_missing_indicator(df)

  # drop columns with less than 2 factor levels only for factor columns
  df <- df %>%
    select(-which(sapply(., is.factor) & sapply(., nlevels) < 2)) %>%
    # drop columns with only one unique value for numeric columns %>%
    select(-which(sapply(., is.numeric) & sapply(., length) == 2)) %>%
    # remove columns with no data

    select(-which(colSums(is.na(.)) == nrow(.)))
  
  
  # remove duplicate columns by value
  df <- df[, !duplicated(t(df))]




  # columns with data imbalances
  imbalanced_cols <- calc_low_freq_cols(df, threshold = 0.15)
  # drop the imbalanced columns
  df <- df %>%
    select(-imbalanced_cols)


  # run the mice function, returns (list) of imputed dataframes
  m <-  2
  imputed_dfs <- run_mice(df,m = m)
  
  


  # create response variable
  # sum the controv_cols row-wise
  controv_cols <- controv_cols %>%
    mutate(controv_sum = rowSums(. ,na.rm = T ))
   # imputed_df$y <- controv_cols$controv_sum

  # save the imputed dataframes to a csv
  for (i in 1:m) {
    
    csv <- imputed_dfs[[i]]
    imputed_dfs[[i]]$y  <- controv_cols$controv_sum
    write.csv(imputed_dfs[[i]], file = paste0("data/imputed_df", i, ".csv"))
    
    # imputed_dfs[[i]] <- csv
  }
  save(imputed_dfs, file="data.Rda")
  # save(y, file="data.Rda")
}


calc_low_freq_cols <- function(df, threshold = 0.05) {
  # Calculate the proportion of each class in each column


  #
  factor_cols <- df %>% select_if(is.factor)

  class_prop <- apply(factor_cols, 2, function(x) prop.table(table(x)))
  #
  t <- class_prop < threshold
  # # Find the columns where the proportion of any class is below the threshold
  cols_to_drop <- which(colSums(t) > 0)
  #
  # # Remove the identified columns from the dataframe
  #   df_clean <- df[, -cols_to_drop]

  return(names(cols_to_drop))
}
#
test_calc_low_freq <- function() {
  # imbalanced factor df
  df <- data.frame(
    x = as.factor(c(rep("a", 10), rep("b", 1), rep("c", 5))),
    y = c(rep("a", 5), rep("b", 5), rep("c", 6)),
    z = as.factor(c(rep("a", 1), rep("b", 10), rep("c", 5)))
  )
  cols <- calc_low_freq_cols(df, threshold = 0.05)
  print(cols)
}
# test_calc_low_freq()

run_mice <- function(df, m) {
  # subset cols with missing values
  df_missing <- df %>%
    select(-which(colSums(is.na(.)) == 0))

  # subset cols with no missing values
  df_no_missing <- df %>%
    select(-which(colSums(is.na(.)) > 0))

  imp <- mice(df_missing, maxit = 0)
  predM <- imp$predictorMatrix
  meth <- imp$method
  
  # causes issies with a constant y
  meth["TR.AnalyticNomCommInvolvment"] <- ""
  meth["TR.BoardMemberLTCompIncentives"] <- ""
  # meth["TR.USLGBTEqualityIndex"] <-  ""

  meth <- lapply(meth, function(x) ifelse(x == "pmm", "lasso.norm", x))
  # meth <- lapply(meth, function(x) ifelse(x == "pmm","lasso.select.norm", x))
  meth <- lapply(meth, function(x) ifelse(x == "logreg", "lasso.logreg", x))

  # centre and scale df_missing
  
  
  df_missing_scaled <- scale_numeric_cols(df_missing)

  # m <- 2

  imp2 <- mice(df_missing_scaled,
    # maxit = iter,  test#change to 5
    m = m,
    predictorMatrix = predM,
    method = meth,
    print = T,
    ridge = 1e-5,
    nfolds = 3
  )

  # implement
  imp_dfs <- list(rep(NA, m))
  # predict the missing values
  for (i in 1:m) {
    df_imp <- mice::complete(imp2, i)

    # unscale the imputed values
    df_imp <- unscale_numeric_df(df = df_missing, scale_df = df_imp)

    # combine the imputed and non missing values
    df_imp <- cbind(df_imp, df_no_missing)
    
    df_imp <- df_imp %>% select_if(~!all(is.na(.)))
    imp_dfs[[i]] <- df_imp
  }

  return(imp_dfs)
}

# scale_numeric_cols <- function(df) {
#   num_cols <- df %>% select_if(is.numeric) %>% colnames()
#   df[num_cols] <- scale(df[num_cols])
#   return(df)
# }



create_missing_indicator <- function(df, threshold = 0.25,
                                     u_thresh = 1) {
  missing_prop <- colMeans(is.na(df))
  df_indicator <- df %>%
    # dropped 40 cols
    # if missing p , p 0.25< p < 1 then we consider reported the statistic
    mutate(
      across(names(missing_prop[missing_prop < u_thresh & missing_prop > threshold]),
        .fns = ~ !is.na(.x), # checks not na
        .names = "reported_{.col}"
      ),
      # "unused" retains only the columns not used in
      .keep = "unused"
    )




  return(df_indicator)
}


rename_cols <- function(df, field_df) {
  # rename columns to match the RDP API
  new_cols <- c()
  for (col in colnames(df)) {
    mapping <- ifelse(col %in% field_df$Title,
      subset(field_df, Title == col)$`RDP API Field Name`,
      col
    )
    new_cols <- append(new_cols, mapping)
  }
  colnames(df) <- new_cols

  return(df)
}


# maps the cols to the appropriate datetype
map_dtype_field <- function(map_df, df, dtypes) {
  cols <- map_df %>%
    filter(`RDP API Field Name` %in% colnames(df) &
      # check if df type is bool
      `Data type (dt)` %in% dtypes) %>%
    pull(`RDP API Field Name`)
  return(cols)
}

map_dtype_field <- function(map_df, data, dtypes) {
  cols <- map_df %>%
    filter(`RDP API Field Name` %in% colnames(data) &
      # check if data type is bool
      `Data type (dt)` %in% dtypes) %>%
    pull(`RDP API Field Name`)
  return(cols)
}

# cols to scale


# convert string cols with less than 6 unique values to factor
str_to_fct <- function(x) {
  if (is.character(x) & length(unique(x)) < 6) {
    return(as.factor(x))
  } else {
    return(x)
  }
}

convert_dtype <- function(df, field_df) {
  numeric_cols <- map_dtype_field(field_df, df, c("Float", "Money", "Integer"))
  date_cols <- map_dtype_field(field_df, df, "Date")
  str_cols <- map_dtype_field(field_df, df, "String")
  bool_cols <- map_dtype_field(field_df, df, "Boolean")


  df %>%
    mutate(across(str_cols, as.character)) %>%
    mutate(across(bool_cols, as.factor)) %>%
    mutate(across(date_cols, as.Date)) %>%
    mutate(across(numeric_cols, as.numeric))


  return(df)
}


scale_numeric_cols <- function(df) {
  num_cols <- df %>% select_if(is.numeric) %>% colnames()
  df[num_cols] <- scale(df[num_cols])
  attr(df[num_cols], "scaled:center") <- NULL
  attr(df[num_cols], "scaled:scale") <- NULL
  df[num_cols] <- lapply(df[num_cols], unname)
  return(df)
}



unscale_numeric_df <- function(df, scale_df) {
  num_cols <- df %>% select_if(is.numeric) %>% colnames()
  
  #note scaled df is imputed
  
  sd <- apply(df[num_cols], 2, sd, na.rm = T)
  mean <- colMeans(df[num_cols], na.rm = T)
  
  scale_df[num_cols] <- scale_df[num_cols] * sd + mean
  
  unscaled_df <- scale_df
  return(unscaled_df)
}


convert_str_to_factor <- function(df) {
  # if str cols have less than 6 unique values, convert to factor
  str_cols <- df %>%
    select_if(is.character) %>%
    colnames()
  for (col in str_cols) {
    if (length(unique(df[[col]])) < 6) {
      df[[col]] <- as.factor(df[[col]])
    }
  }
}


main()
