library(tidyverse)
library(readxl)
library(naniar)
library(mice)

# contains mapping for categories
field_df <- read_xlsx("docs/Refinitiv ESG docs/esg_data_guide.xlsx", 
                      sheet = "Field Description")

# create interaction terms
data <- read_xlsx("src/data/FTSE100.xlsx") %>%
    select(-1)  #remove index


#create remapping of column names from Name to TR. API st
new_cols <- c()
for (col in colnames(data)){
    mapping <- ifelse(col %in% field_df$Title, subset(field_df, Title == col)$`RDP API Field Name`, col)
    new_cols <- append(new_cols, mapping)
}


colnames(data) <- new_cols


#pick the odd years
df <- data %>%
    filter(row_number() %in% seq(1,nrow(data),2))





# create response 

#contreversy cols
controv.cols <- colnames(df)[str_detect(colnames(df), regex("^TR.Controv"))]

#remove the scores as its not data?
score.cols <- colnames(df)[str_detect(colnames(df), regex("Score"))]


unique(field_df$`Data type (dt)`)

unique(x$`CO2 Estimation Method`)

#check missing vals

# maps the cols to the appropriate datetype
map_dtype_field <- function(map_df, data, dtypes ){
    cols <- map_df %>% 
        filter(`RDP API Field Name` %in% colnames(data) & 
                   #check if data type is bool
                   `Data type (dt)` %in% dtypes) %>%
        pull(`RDP API Field Name`)
    return(cols)
}

#cols to scale
scale_cols <- map_dtype_field(field_df, df, c("Float","Money","Integer") )
date_cols <- map_dtype_field(field_df, df, "Date" )
str_cols <- map_dtype_field(field_df, df, "String" )
bool_cols <- map_dtype_field(field_df, df, "Boolean" )
# 


#turn the ISO400 + board structure to factors

str_to_fct <- c("TR.ISO14000", "TR.BoardStructureType")

# cols_with_spaces <- colnames(df)[grepl("",colnames(df))]
cols_with_spaces <- colnames(df)[str_detect(colnames(df), regex("\\s+|&"))]

#deal with missing values



#create the x matrix
x <- df %>% 
    
    mutate(across( str_to_fct, as.factor)) %>%
    mutate(across(bool_cols, as.factor)) %>%
    # mutate(	
    #     TR.CO2EstimationMethod = recode( TR.CO2EstimationMethod, "Reported" = 1,
    #                                     "Median" = 0)) %>%
    # mutate(across(scale_cols, scale)) %>%
    select(!all_of(c(controv.cols, #removes the response
                     #removes the score cols
                     score.cols, 
                     cols_with_spaces
                     ))) %>%
    select(!Instrument) 


    
# colnames(x)
    


p_missing <- unlist(lapply(x, function(x) sum(is.na(x))))/nrow(x)
sort(p_missing[p_missing > 0], decreasing = TRUE)

#get names 
# names(p_missing) 
length(p_missing[p_missing > 0.25])


missing_df <- x %>% summarize_all(funs(sum(is.na(.)) / length(.))) %>% 
    pivot_longer(everything(),
                 names_to = "col",
                 values_to = "missingperc")

miss_cols <- missing_df %>% 
    filter(
    missingperc > 0.5  #col with more than 50% missing
) %>%
    pull(col)
#drop cols with no values



### missing values drop cols with 100% missng values
x <- x %>% select(!all_of(names(p_missing[p_missing == 1]))) %>% 
#dropped 40 cols
# if missing p , p 0.25< p < 1 then we consider reported the statistic
  mutate(across(names(p_missing[p_missing < 1 & p_missing > 0.25]), 
                  .fns =  ~!is.na(.x), #checks not na
                  .names = "reported_{.col}",
                 .keep = "unused")   # "unused" retains only the columns not used in
  )

colnames(x)    
#time to use mice

# imp <- mice(x[colnames(x)[130:135]], maxit = 0)

set.seed(20000)

n = 100
imp <- mice(x[1:n], maxit = 0,method = "cart")
    

predM <- imp$predictorMatrix
meth <- imp$method    


imp2 <- mice(x[1:n], maxit = 5,
             predictorMatrix = predM, 
             method = "cart", print =  T)
# 
# x <- x %>%
#     mutate(across(miss_cols, 
#            .fns = ~!is.na(.x), #checks not na
#            
#            .names = "reported_{.col}"),
#            .keep = "unused")
# colnames(temp)
#            
#use 


    #        %>%
    # rename_with()
    #        
    #        
    #                .fns = list(reported = ~is.na),
    #                .names = "{fn}_{col}" ) )

#deal with missing values
# turn columns with more thatn 50% missing values to presence or not
# i.e. reported or not if continuous







y <- rowSums(df[,controv.cols], na.rm = TRUE)
# df["total_controv"] <-  y

#map title in df to title in field df
# create the * interaction terms
interaction_fields <- list()
for (pillar in c("Environmental", "Governance", "Social")){
    # print(pillar)
    pillar_fields <- field_df %>% 
        filter(`RDP API Field Name` %in% colnames(x) &
                   Pillar == pillar & 
                   #check if title is not a controversy score
                   !str_detect(Title, "Controv")
        ) %>%
        pull(`RDP API Field Name`)
    
    interaction_fields[[pillar]] <-  combn(pillar_fields, 2, FUN = paste, 
                                           collapse = '*')
}


interaction_fields$Environmental[1:10]
# replace with whole string

linear_pred <- paste(interaction_fields$Environmental[1:4], sep = " " , collapse = " + ")


f <- paste("total_controv","~ ", linear_pred )
glmFit <- glm(f , data = df, family = quasipoisson)

xmatrix <- model.matrix(glmFit)

xmatrix <- xmatrix[, 2:ncol(xmatrix)]

head(xmatrix)






missing.values <- df %>%
    gather(key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    group_by(key) %>%
    mutate(total = n()) %>%
    group_by(key, total, isna) %>%
    summarise(num.isna = n()) %>%
    mutate(pct = num.isna / total * 100)



missing.values %>% filter(
    isna == "TRUE",
    pct > 50
)

levels <-
    (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>% 
    ggplot() +
    geom_bar(aes(x = reorder(key, desc(pct)), 
                 y = pct, fill=isna), 
             stat = 'identity', alpha=0.8) +
    scale_x_discrete(limits = levels) +
    scale_fill_manual(name = "", 
                      values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
    coord_flip() +
    labs(title = "Percentage of missing values", x =
             'Variable', y = "% of missing values")

percentage.plot
#create xmatrix for lasso



#create a 


#fit lasso

#fit spike and slab

# fit xgboost

#design simulation


hist(df$`ESG Controversies Score`
     )


