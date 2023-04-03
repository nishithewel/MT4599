library(tidyverse)
library(readxl)
library(naniar)
library(mice)

source("helper.R")
# contains mapping for categories
field_df <- read_xlsx("docs/Refinitiv ESG docs/esg_data_guide.xlsx",
  sheet = "Field Description"
)

# create interaction terms
df <- read_xlsx("src/data/FTSE100.xlsx") %>%
  # remove index
  dplyr::select(-1) %>%
  # select a single year
  filter(row_number() %in% seq(1, nrow(.), 2))

# write a function to load excel data to a dateframe in r


# #create remapping of column names from Name to TR. API st





map_columns <- function(df) {
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

df <- map_columns(df)

# contreversy cols
controv.cols <- colnames(df)[str_detect(colnames(df), regex("^TR.Controv"))]

# remove the scores as its not df?
score.cols <- colnames(df)[str_detect(colnames(df), regex("Score"))]

# maps the cols to the appropriate datetype
map_dtype_field <- function(map_df, df, dtypes) {
  cols <- map_df %>%
    filter(`RDP API Field Name` %in% colnames(df) &
      # check if df type is bool
      `Data type (dt)` %in% dtypes) %>%
    pull(`RDP API Field Name`)
  return(cols)
}

# cols to scale
scale_cols <- map_dtype_field(field_df, df, c("Float", "Money", "Integer"))
date_cols <- map_dtype_field(field_df, df, "Date")
str_cols <- map_dtype_field(field_df, df, "String")
bool_cols <- map_dtype_field(field_df, df, "Boolean")
#


# turn the ISO400 + board structure to factors
str_cols <- df %>% select_if(is.character)

str_to_fct <- names(str_cols[str_cols %>%
  map(unique) %>%
  map(length) < 6])

# x# cols_with_spaces <- colnames(df)[grepl("",colnames(df))]
cols_with_spaces <- colnames(df)[str_detect(colnames(df), regex("\\s+|&"))]

# create the x matrix
x <- df %>%
  mutate(across(str_to_fct, as.factor)) %>%
  mutate(across(bool_cols, as.factor)) %>%
  # this makes the pmm run but is strictly not proper, since the nas are blockingn
  mutate(across(
    scale_cols,
    ~ as.numeric(scale(.)) # consider caret preprocess in this step
  )) %>%
  dplyr::select(!all_of(c(
    # controv.cols, #removes the response
    # removes the score cols
    score.cols,
    # remove date cols
    date_cols,
    # this messes with mice so we remove, an issue with the mapping
    cols_with_spaces
  ))) %>%
  dplyr::select(!Instrument)

# sum(is.na(temp))/(nrow(temp)*ncol(temp))

#write a function to remove columns with 75% missing values
remove_cols <-







p_missing <- unlist(lapply(x, function(x) sum(is.na(x)))) / nrow(x)
sort(p_missing[p_missing > 0], decreasing = TRUE)

# get names
# names(p_missing)
length(p_missing[p_missing > 0.25])

fully_missing <- all_of(names(p_missing[p_missing == 1]))

fully_missing

### missing values drop cols with 100% missng values
x <- x %>% dplyr::select(!fully_missing) %>%
  # dropped 40 cols
  # if missing p , p 0.25< p < 1 then we consider reported the statistic
  mutate(across(names(p_missing[p_missing < 1 & p_missing > 0.25]),
    .fns = ~ !is.na(.x), # checks not na
    .names = "reported_{.col}"
  ),
  # "unused" retains only the columns not used in
  .keep = "unused"
  )
# %>%
# select(!all_of(names(p_missing[p_missing < 1 & p_missing > 0.25])))

colnames(x)[!colnames(x) %in% colnames(df)]

# colnames(x)

# get the num of leves of facts
fct_cols <- x %>%
  dplyr::select_if(is.factor) %>%
  map(levels) %>%
  map(length)


x <- x %>%
  # drop cols with less than 2 factors
  dplyr::select(!names(fct_cols[fct_cols < 2])) %>%
  # drop constant cols
  dplyr::select(!where(~ any(length(unique(.)) == 2 & NA %in% unique(.))))
## Need to drop constant columns


drop_rows <- which.max(rowSums(is.na(x)))
x <- x %>%
  filter(
    !row_number() == drop_rows
  )


# run a check
sum(x %>%
  dplyr::select_if(is.factor) %>%
  map(levels) %>% map(length) < 2) == 0

# x %>%  select_if(is.character) %>%  map(unique) %>% map(length)



# x %>% select(!where(~any(length(unique(.)) == 2 & NA %in% unique(.) )))


# consider dividing the df by removing cols with no-missing values

# drop rows based on missingness

x_no_missing <- x %>%
  select(where(~ !any(is.na(.))))
# filter(
#   !row_number() == which.max(rowSums(is.na(x)))
# )

x_missing <- x %>%
  select(where(~ any(is.na(.))))



set.seed(20000)



# cols with large class imbalances
imbalanced_cols <- names(x_missing %>% select_if(is.factor) %>%
  # gets the counts of each level
  map(table) %>%
  # if the relative frequency is less than 5% we choose
  keep(\(x) any(x / 105 < 0.1)))

md.pattern(x %>% select(imbalanced_cols) %>% select(!TR.CSRReportingExternalAudit))

imbalanced_cols

which.max(rowSums(is.na(x)))

hist(rowSums(is.na(x)))

imp_df <- x_missing %>%
  # filter(
  #   !row_number() == which.max(rowSums(is.na(x)))
  #        ) %>%

  # this row has a lot of missing values
  # drop cols with less than 5% balance
  select(!all_of(c(imbalanced_cols)))

imp <- mice(imp_df, maxit = 0)

md.pattern(imp_df)

predM <- imp$predictorMatrix
meth <- imp$method

# causes issies with a constant y
meth["TR.AnalyticNomCommInvolvment"] <- ""
meth["TR.BoardMemberLTCompIncentives"] <- ""
# meth["TR.BlankCheck"] <- ""
# ,"TR.BoardMemberLTCompIncentives"
meth

# addreess the collinearity problems


# should we centre the variables before applying lasso? Yes
meth <- lapply(meth, function(x) ifelse(x == "pmm", "lasso.norm", x))
# meth <- lapply(meth, function(x) ifelse(x == "pmm","lasso.select.norm", x))
meth <- lapply(meth, function(x) ifelse(x == "logreg", "lasso.logreg", x))
meth

iter <- 2
imp2 <- mice(imp_df,
  # maxit = iter,  test#change to 5
  predictorMatrix = predM,
  method = meth,
  print = T,
  ridge = 1e-5
)

# x_long <- mice::complete(imp2, action= "long", include = TRUE)

# mice(imp_df,method = meth,print = T)
# diagnotics
sum(is.na(imp_df)) / sum(!is.na(imp_df))
# sum(is.na(x_long))/sum(!is.na(x_long))

md.pattern(imp_df)
md.pattern(complete(imp2))
# theres one row with a lott of missing values i.e 48 missing values

plot(imp2)
# vis_miss(x_long[1:105,])


p_missing <- unlist(lapply(x_long, function(x) sum(is.na(x)))) / nrow(x_long)
sort(p_missing[p_missing > 0], decreasing = TRUE)

# make.post(x_missing)
which.max(rowSums(is.na(complete(imp2, 4))))



# write all mice df to
imp_x <- mice::cbind(imp2, x_no_missing) %>%
  # ok becasue glmnet does work with the with.mids
  mice::complete(3)

# md.pattern(test)
hist(rowSums(is.na(imp_x)))

# we can drop more rows



# i.e. reported or not if continuous

create_response <- function() {
  y <- rowSums(df[, controv.cols] %>%
    # bit janky fix
    filter(!row_number() == drop_rows),
  na.rm = TRUE
  )
  # dichotamising step
  y <- ifelse(y > 0, 1, 0)
  return(y)
}

y <- create_response()
# df["total_controv"] <-  y

# map title in df to title in field df
# create the * interaction terms



# replace with whole string




# remove rows with missing values
complete_data <- data[complete.cases(data), ]

# our model do not work with missing data
data <- cbind(y, imp_x)
# removes rows with missing values
complete_data <- data[complete.cases(data), ]


# remove cols with duplicate values in data
complete_data <- complete_data[, apply(complete_data, 2, function(x) length(unique(x)) > 1)]


# interations <-
interactions <- get_interactions(complete_data)
linear_pred <- paste(unlist(interactions), sep = " ", collapse = " + ")
# linear_pred


f <- paste("y", "~ ", linear_pred)


# ssgam_f <- paste("y","~ ", comple )
# glmFit <- glm(f , data = complete_data, family = quasipoisson)


# drop the intercept
# xmatrix <- model.matrix(glmFit)[, -1]

xmatrix <- model.matrix(formula(f), data = complete_data)[, -1]
dim(xmatrix)





xmatrix <- remove_duplicate(xmatrix)

y <- as.factor(complete_data$y)
# xmatrix <- xmatrix

head(xmatrix)


#
#
#
#
# missing.values <- df %>%
#     gather(key = "key", value = "val") %>%
#     mutate(isna = is.na(val)) %>%
#     group_by(key) %>%
#     mutate(total = n()) %>%
#     group_by(key, total, isna) %>%
#     summarise(num.isna = n()) %>%
#     mutate(pct = num.isna / total * 100)
#
#
#
# missing.values %>% filter(
#     isna == "TRUE",
#     pct > 50
# )
#
# levels <-
#     (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key
#


# percentage.plot
# create xmatrix for lasso



# create a


# fit lasso
library(glmnet) ###
lasso <- glmnet(
  x = xmatrix,
  y = as.factor(y),
  family = "binomial"
)


cvlasso <- cv.glmnet(xmatrix,
  as.factor(y),
  family = "binomial",
  # type.measure = "class"
  type.measure = "auc",
  alpha = 1, # this indicates the lasso part
  nfolds = 5,
  # becasume we scaled earlier
  standardize = T,
  # should set up the lambda values,
  keep = TRUE
)
cvlasso$lambda.min


par(mfrow = c(1, 2))
plot(lasso, xvar = "lambda")

abline(v = log(cvlasso$lambda.min), lwd = 4, lty = 2)

plot(cvlasso)

abline(v = log(cvlasso$lambda.min), lwd = 4, lty = 2)


summary(cvlasso)

# cvlasso$lambda.min

outcome <- coef(cvlasso, s = "lambda.1se")
# view non-zero coefs
outcome[outcome[, 1] != 0, ]



cnf <- confusion.glmnet(cvlasso, newx = xmatrix, newy = y)
print(cnf)


# ROC curves
rocs <- roc.glmnet(cvlasso$fit.preval, newy = y)
best <- cvlasso$index["min", ]
plot(rocs[[best]], type = "l")
invisible(sapply(rocs, lines, col = "grey"))
lines(rocs[[best]], lwd = 2, col = "red")
title("ROC for LASSO model")

idmin <- match(cvlasso$lambda., cvlasso$lambda)
auc <- cvlasso[["cvm"]][idmin]
auc



library(selectiveInference)
# inference step
# estimate sigm
# estimateSigma(xmatrix, )
# get teh beta
lambda <- cvlasso$lambda.min

n <- nrow(xmatrix)

beta <- coef(cvlasso, s = lambda / n)

# run inference
# maybe we drop the categorical variables here?
fixedLassoInf(xmatrix,
  as.numeric(y),
  family = "binomial",
  beta = beta,
  lambda = lambda
)
# Error in checkargs.xy(x, y) : x cannot have duplicate columns


library(spikeSlabGAM)
# lp <- paste(unlist(colnames(x_no_missing)),sep = " ", collapse = " + ")
#
# f1 <- paste("y ~", lp)
#
# f1


mcmc <- list(
  nChains = 4, chainLength = 5`000, burnin = 500,
  thin = 5
)

m <- spikeSlabGAM(
  formula = formula(f),
  data = complete_data,
  family = "binomial",
  mcmc = mcmc
)

summary(m)


# fit xgboost

# design simulation
# set up matrix of std normal vars
# generate random coefficient list
# randomly set some to zero i.e sparsity
# set some close to zero and see if model "picks out the right" ones

# see error accross



# inference on lasso
# library(selectiveIn)



# missing_df <- x %>% summarize_all(funs(sum(is.na(.)) / length(.))) %>%
#     pivot_longer(everything(),
#                  names_to = "col",
#                  values_to = "missingperc")
#
# miss_cols <- missing_df %>%
#     filter(
#     missingperc > 0.5  #col with more than 50% missing
# ) %>%
#     pull(col)
# #drop cols with no values
#

# hist(df$`ESG Controversies Score`
#      )