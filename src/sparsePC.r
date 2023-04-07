# Generated from function body. Editing this file has no effect.
fixedSparsePC <- function(
    x = NULL, y = NULL, n.rep = 10, n.iter1 = 150, n.iter2 = 100,
    n.prcmp = 5, max.genes = 100, ntree = 1000, nodesize = 1,
    verbose = TRUE, ...) {
    permute.rows <- function(x) {
        dd <- dim(x)
        n <- dd[1]
        p <- dd[2]
        mm <- runif(length(x)) + rep(seq(n) * 10, rep(p, n))
        matrix(t(x)[order(mm)], n, p, byrow = TRUE)
    }
    balanced.folds <- function(y, nfolds = min(
                                   min(table(y)),
                                   10
                               )) {
        totals <- table(y)
        fmax <- max(totals)
        nfolds <- min(nfolds, fmax)
        nfolds <- max(nfolds, 2)
        folds <- as.list(seq(nfolds))
        yids <- split(seq(y), y)
        bigmat <- matrix(NA, ceiling(fmax / nfolds) * nfolds, length(totals))
        for (i in seq(totals)) {
            if (length(yids[[i]]) > 1) {
                bigmat[seq(totals[i]), i] <- sample(yids[[i]])
            }
            if (length(yids[[i]]) == 1) {
                bigmat[seq(totals[i]), i] <- yids[[i]]
            }
        }
        smallmat <- matrix(bigmat, nrow = nfolds)
        smallmat <- permute.rows(t(smallmat))
        res <- vector("list", nfolds)
        for (j in 1:nfolds) {
            jj <- !is.na(smallmat[, j])
            res[[j]] <- smallmat[jj, j]
        }
        return(res)
    }
    class.error <- function(y, ytest, pred) {
        cl <- sort(unique(y))
        err <- rep(NA, length(cl))
        for (k in 1:length(cl)) {
            cl.pt <- (ytest == cl[k])
            if (sum(cl.pt) > 0) {
                err[k] <- mean(ytest[cl.pt] != pred[cl.pt])
            }
        }
        err
    }
    get.pc <- function(X, Y, n.prcmp) {
        cat("Getting prcmp...\n")
        nclass <- length(unique(Y))
        n <- nrow(X)
        p <- ncol(X)
        o.r <- order(Y)
        Y <- Y[o.r]
        Y.freq <- tapply(Y, Y, length)
        X <- X[o.r, ]
        # X.mean <- as.double(c(apply(X, 2, mean.center, center = TRUE)))
        # X.sd <- as.double(c(apply(X, 2, sd.center, center = TRUE)))
        ss.X <- X
        pc.out <- svd(ss.X)
        U <- pc.out$u
        D <- pc.out$d
        UD <- t(t(U) * D)
        imp <- apply(UD, 2, function(p) {
            rf.out <- randomForest::randomForest(cbind(Y), p,
                importance = TRUE
            )
            rf.out$importance[1, 1]
        })
        o.r <- order(imp, decreasing = TRUE)
        tot.var <- cumsum(D[o.r] / sum(D))
        cat("total variation for top prcmp(s):", round(100 *
            tot.var[n.prcmp]), "%", "\n")
        Y.prcmp <- as.matrix(UD[, o.r[1:n.prcmp]])
        Y.prcmp <- as.matrix(apply(Y.prcmp, 2, function(p) {
            p.mean <- rep(tapply(p, Y, mean), Y.freq)
            p.sd <- rep(tapply(p, Y, sd), Y.freq)
            rnorm(n, p.mean, 0.1 * p.sd)
        }))
        colnames(ss.X) <- paste("X.", 1:p, sep = "")
        return(list(Y.prcmp = Y.prcmp, ss.X = ss.X, tot.var = tot.var))
    }
    gene.signature <- NULL
    X <- as.matrix(x)
    Y <- y
    if (!is.factor(y)) {
        Y <- factor(y)
    }
    n.genes <- ncol(X)
    n.data <- nrow(X)
    if (any(is.na(Y))) {
        stop("Missing values not allowed in y")
    }
    if (n.data != length(Y)) {
        stop("number of rows of x should match length of y")
    }
    n.class <- length(unique(Y))
    dim.results <- pred.results <- rep(0, n.rep)
    pred.class.results <- matrix(NA, n.rep, n.class)
    X <- scale(X, center = TRUE, scale = TRUE)
    for (k in 1:n.rep) {
        if (verbose) {
            cat("\n ---> Monte Carlo Replication:", k, "\n")
        }
        if (n.rep > 1) {
            cv.sample <- balanced.folds(Y, 3)
            train.pt <- c(cv.sample[[1]], cv.sample[[2]])
            test.pt <- cv.sample[[3]]
        } else {
            train.pt <- test.pt <- 1:n.data
        }
        n.prcmp <- min(n.prcmp, length(train.pt))
        pc.out <- get.pc(X[train.pt, ], Y[train.pt], n.prcmp = n.prcmp)
        sig.genes <- NULL
        signal <- rep(0, n.genes)
        for (p in 1:n.prcmp) {
            if (verbose) {
                cat("fitting principal component:", p, "(", round(100 *
                    pc.out$tot.var[p]), "%)", "\n")
            }
            ss.out <- spikeslab(
                x = pc.out$ss.X, y = pc.out$Y.prcmp[
                    ,
                    p
                ], n.iter1 = n.iter1, n.iter2 = n.iter2, max.var = max.genes,
                bigp.smalln.factor = max(1, round(max.genes / nrow(pc.out$ss.X))),
                bigp.smalln = (nrow(pc.out$ss.X) < ncol(pc.out$ss.X))
            )
            sig.genes.p <- as.double(which(abs(ss.out$gnet) >
                .Machine$double.eps))
            if (length(sig.genes.p) > 0) {
                sig.genes <- c(sig.genes, sig.genes.p)
                signal[sig.genes.p] <- signal[sig.genes.p] +
                    abs(ss.out$gnet)[sig.genes.p]
            }
        }
        if (length(sig.genes) == 0) {
            sig.genes <- as.double(which(signal == max(signal,
                na.rm = TRUE
            )))[1]
        }
        sig.genes <- sort(unique(sig.genes))
        P <- min(max.genes, length(sig.genes))
        o.r <- order(signal[sig.genes], decreasing = TRUE)
        sig.genes.k <- sig.genes[o.r][1:P]
        rf.data.x <- as.matrix(X[, sig.genes.k])
        colnames(rf.data.x) <- paste("x.", 1:length(sig.genes.k))
        Y.train <- Y[train.pt]
        Y.test <- Y[test.pt]
        rf.out <- randomForest::randomForest(
            x = as.matrix(rf.data.x[train.pt, ]), y = Y.train, importance = TRUE, ntree = ntree,
            nodesize = nodesize
        )
        gene.signature <- c(gene.signature, sig.genes.k)
        dim.results[k] <- length(sig.genes.k)
        if (n.rep > 1) {
            rf.pred <- predict(rf.out, newdata = as.matrix(rf.data.x[test.pt, ]))
            pred.results[k] <- mean(as.character(Y.test) != rf.pred)
            pred.class.results[k, ] <- class.error(
                as.character(Y),
                as.character(Y.test), rf.pred
            )
        }
        if (verbose & (n.rep > 1)) {
            cat(
                "\n", "PE:", round(pred.results[k], 3), "dimension:",
                dim.results[k], "\n"
            )
        }
    }
    gene.signature.all <- gene.signature
    gene.signature.freq <- tapply(
        gene.signature, gene.signature,
        length
    )
    gene.signature <- as.double(names(gene.signature.freq)[rev(order(gene.signature.freq))][1:mean(dim.results)])
    if (verbose) {
        cat("growing the forest classifier...\n")
    }
    rf.data.x <- as.matrix(X[, gene.signature])
    colnames(rf.data.x) <- paste("x.", gene.signature)
    rf.out <- randomForest::randomForest(
        x = rf.data.x, y = Y,
        importance = TRUE, ntree = ntree, nodesize = nodesize
    )
    cat("\n\n")
    cat("-----------------------------------------------------------\n")
    cat("no. prcmps           :", n.prcmp, "\n")
    cat("no. genes            :", n.genes, "\n")
    cat("max genes            :", max.genes, "\n")
    cat("no. samples          :", nrow(X), "\n")
    cat("no. classes          :", n.class, "\n")
    cat(
        "class freq           :", as.double(tapply(Y, Y, length)),
        "\n"
    )
    cat("class names          :", levels(Y), "\n")
    cat("replicates           :", n.rep, "\n")
    cat(
        "model size           :", round(mean(dim.results), 4),
        "+/-", round(sd(dim.results), 4), "\n"
    )
    if (n.rep > 1) {
        cat("misclass             :", round(
            mean(100 * pred.results),
            4
        ), "+/-", round(sd(100 * pred.results), 4), "\n")
        for (j in 1:n.class) {
            cat(paste("  class #         ", j, ":"), round(mean(100 *
                pred.class.results[, j], na.rm = TRUE), 4), "\n")
        }
    }
    cat("\n")
    cat("Gene Signature:\n")
    print(gene.signature)
    cat("-----------------------------------------------------------\n")
    invisible(list(
        gene.signature = gene.signature, gene.signature.all = gene.signature.all,
        rf.object = rf.out
    ))
}
