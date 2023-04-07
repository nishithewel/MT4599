# create plot of multicollinearity
# see https://www.rpubs.com/Steven_Surya/correlation-network

# see https://r-graph-gallery.com/250-correlation-network-with-igraph.html

library(ggplot2)
library(gridExtra)
plot_MC <- function() {

}


load(".Rdata")

source("https://www.dropbox.com/s/ht5k403adce4h6f/corr_graph.R?dl=1")
library(igraph)


# corr_graph(complete_df %>% select_if(is.numeric))

df <- xmats[[1]]
# replace _ with space on column names
colnames(df) <- gsub("_", " ", colnames(df))



corr_graph(df,r.cut.off = 0.95,
# layout = "layout_in_circle"
            vertex.cex = 1.5,
           scale.vertex = T,
           border = "dark grey", omit.rest = T,
           output = "png", filename = "corr_graph",       
           label.cex = 0.9,         
## 3      domains = NULL, 
            res = 150, height = 6, width = 16

           )

# create plot of mcmc convergence


# create plot of lasso convergence
plot(spike_models[[1]][["model"]])



#plot var imps for each random forest model



# p[[2]]


plot_var_imptree <- function(models) {
    #returns list of plots
    p <- list()
    for (i in seq_along(models)) {
        tree_imp <- varImp(models[[i]][["model"]], scale = F)

        # change col names of plot
        rownames(tree_imp$importance) <- colnames(xmats[[i]])[as.numeric(row.names(tree_imp$importance))]
        # change datafram rownames
        p[[i]] <- plot(tree_imp, top = 10, main = paste("Variable Importance for RRF Case ", i))
    }
    return(p)
}

plot_var_imptree(tree_models)
g <-grid.arrange(grobs = p, nrow = 3)
ggsave(
    filename = "plots/tree_var_imps.png",
    plot = g, width = 210, height = 297, units = "mm"
)


#get rf 
library(randomForest)
plot_var_impspike <- function(models) {
    # returns list of plots
    p <- list()
    for (i in seq_along(models)) {
        # tree_imp <- varImpPlot(models[[i]][["model"]]$rf.obj)
        rf <- spike_models[[i]][["model"]]$rf.obj


        #extract number from rownames string


        # regex to extract number from string
        # col_nums <- str_extract(rownames(rf$importance), "[0-9]+")

        # change col names of plot


        rownames(rf$importance) <- colnames(xmats[[i]])[as.numeric(col_nums)]
        # change datafram rownames
        p[[i]] <- plot(tree_imp, top = 10, main = paste("Variable Importance for RRF Case ", i))
    }
    return(p)
}
plot_var_impspike(spike_models)

rf <- spike_models[[2]][["model"]]$rf.obj
varImpPlot(rf,n.var = 10)
caret::plot(varImp(rf))
plot(rf)
# create overlaid roc curve

# plot response distribution

# get imputef df first

df %>%
    mutate(cond = y > 0) %>%
    ggplot(aes(x = y, fill = cond)) + geom_histogram(bins = 40) +
 #change plot labels
    labs(x = "", y = "Counts of Controversies",
     title = "Distribution of Controversy Counts") +
     # change legend labels
     scale_fill_discrete(
        name = "Is # Controversies > 0?"
        #  labels = c("No Controversy", "Controversy")
     ) + theme(legend.position = "bottom")

#save to file
ggsave(
    filename = "plots/response_dist.png"
)



#plots for lasso


plot_lasso <- function(models) {
    #returns list of plots
    p <- list()
    for (i in seq_along(models)) {
        # imp <- varImp(models[[i]][["model"]], scale = F)
        mod <- models[[i]][[1]]$glmnet.fit
        # change col names of plot
        # rownames(tree_imp$importance) <- colnames(xmats[[i]])[as.numeric(row.names(tree_imp$importance))]
        # change datafram rownames
        plot(mod, xvar = "lambda"
        )

    }
    return(p)
}

load(".Rdata")

p.lasso <- plot_lasso(lasso_models)
names(lasso_models[[1]][[1]])

plot(roc.glmnet)
g <-grid.arrange(grobs = p.lasso, nrow = 3)
ggsave(
    filename = "plots/lasso_path.png",
    plot = g, width = 210, height = 297, units = "mm"
)


