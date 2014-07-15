##### LDA methods on Coe objects

#' Linear Discriminant Analysis on Coe objects
#' 
#' Performs a LDA on Coe objects. Relies on \link{lda} in MASS.
#' @aliases LDA
#' @rdname LDA
#' @param x a \link{Coe}, or a PCA object
#' @param fac the grouping factor (names of one of the $fac column or column id)
#' @param retain the number of PC axis to retain for LDA.PCA
#' @param ... additional arguments to feed \link{lda}
#' @return a 'LDA' object on which to apply \link{plot.LDA}, which is a list with components:
#' \itemize{
#'  \item \code{x} any \link{Coe} object (or a matrix)
#'  \item \code{fac} grouping factor used
#'  \item \code{removed} ids of columns in the original matrix that have been removed since constant (if any)
#'  \item \code{mod} the raw lda mod from \link{lda}
#'  \item \code{mod.pred} the predicted model using x and mod
#'  \item \code{CV.fac} cross-validated classification
#'  \item \code{CV.tab} cross-validation tabke
#'  \item \code{CV.correct} proportion of correctly classified individuals
#'  \item \code{LDs} unstandardized LD scores see Claude (2008)
#'  \item \code{mshape} mean values of coefficients in the original matrix
#'  \item \code{method} inherited from the Coe object (if any)
#' }
#' @seealso \link{plot.LDA}, \link{plotCV}
#' @keywords Multivariate
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 24)
#' bot.l <- LDA(bot.f, 'type')
#' bot.l
#' plot(bot.l)
#' bot.f$fac$plop <- factor(rep(letters[1:4], each=10))
#' bot.l <- LDA(bot.f, 'plop')
#' bot.l
#' plot(bot.l)
#' @export
LDA <- function(x, fac, retain, ...) {
    UseMethod("LDA")
}

#' @rdname LDA
#' @export
LDA.Coe <- function(x, fac, retain, ...) {
    Coe <- x
    if (missing(fac)) 
        stop(" * no fac provided")
    fac <- Coe$fac[, fac]
    X <- as.matrix(Coe$coe)
    if (!missing(retain) & length(Coe$method) == 1 & Coe$method[1] == 
        "eFourier") {
        X <- X[, coeff.sel(retain = retain, nb.h = ncol(X)/4, 
            cph = 4)]
    }
    remove <- which(apply(X, 2, sd) < 1e-10)
    if (length(remove) != 0) {
        cat(" * variables", colnames(X)[remove], "are removed since they are constant.\n")
        X <- X[, -remove]
    } else {
        remove <- NULL
    }
    # now we calculate two lda models with MASS::lda one with
    mod <- lda(X, grouping = fac, tol = 1e-08, ...)
    mod.pred <- predict(mod, X)
    # leave-one-out cross validation
    CV.fac <- lda(X, grouping = fac, tol = 1e-08, CV = TRUE, 
        ...)$class
    # we build a nice table from it
    CV.tab <- table(fac, CV.fac)
    names(dimnames(CV.tab)) <- c("actual", "classified")
    CV.correct <- sum(diag(CV.tab))/sum(CV.tab)
    # we calculate unstandardized LDs
    n <- nrow(X)
    lm.mod <- lm(X ~ fac)
    dfw <- n - nlevels(fac)
    SSw <- var(lm.mod$residuals) * (n - 1)
    VCVw <- SSw/dfw
    LDs <- VCVw %*% mod$scaling
    # we build the list with all the components that may be
    # useful elsewhere including the lda output, the CV
    # prediction and unstandardized LDs for shape reconstruction
    LDA <- list(x = X, fac = fac, removed = remove, mod = mod, 
        mod.pred = mod.pred, CV.fac = CV.fac, CV.tab = CV.tab, 
        CV.correct = CV.correct, LDs = LDs, mshape = apply(Coe$coe, 
            2, mean), method = Coe$method)
    class(LDA) <- c("LDA", class(LDA))
    return(LDA)
}

#' @rdname LDA
#' @export
LDA.default <- function(x, fac, retain, ...) {
    X <- x
    if (!is.matrix(X)) 
        X <- as.matrix(X)
    if (missing(fac)) 
        stop(" * no fac provided")
    # now we calculate two lda models with MASS::lda one with
    mod <- lda(X, grouping = fac)
    mod.pred <- predict(mod, X)
    # leave-one-out cross validation
    CV.fac <- lda(X, grouping = fac, tol = 1e-08, CV = TRUE, 
        ...)$class
    # we build a nice table from it
    CV.tab <- table(fac, CV.fac)
    names(dimnames(CV.tab)) <- c("actual", "classified")
    CV.correct <- sum(diag(CV.tab))/sum(CV.tab)
    # we calculate unstandardized LDs
    n <- nrow(X)
    lm.mod <- lm(X ~ fac)
    dfw <- n - nlevels(fac)
    SSw <- var(lm.mod$residuals) * (n - 1)
    VCVw <- SSw/dfw
    LDs <- VCVw %*% mod$scaling
    # we build the list to be returned
    LDA <- list(x = X, fac = fac, removed = remove, mod = mod, 
        mod.pred = mod.pred, CV.fac = CV.fac, CV.tab = CV.tab, 
        CV.correct = CV.correct, LDs = LDs, mshape = NULL, method = "other")
    class(LDA) <- c("LDA", class(LDA))
    return(LDA)
}

#' @rdname LDA
#' @export
LDA.PCA <- function(x, fac, retain = 5, ...) {
    PCA <- x
    if (missing(fac)) 
        stop(" * no fac provided")
    fac <- PCA$fac[, fac]
    if (missing(retain)) {
        cat(" * the first", retain, "PC axes are used.")
    }
    X <- PCA$x[, 1:retain]
    if (is.matrix(X)) {
        remove <- which(apply(X, 2, sd) < 1e-10)
        if (length(remove) != 0) {
            cat(" * variables", colnames(X)[remove], "are removed since they are constant.\n")
            X <- X[, -remove]
        }
    } else {
        remove <- NULL
    }
    X <- as.matrix(X)
    # now we calculate two lda models with MASS::lda one with
    mod <- lda(X, grouping = fac, tol = 1e-08, ...)
    mod.pred <- predict(mod, X)
    # leave-one-out cross validation
    CV.fac <- lda(X, grouping = fac, tol = 1e-08, CV = TRUE, 
        ...)$class
    # we build a nice table from it
    CV.tab <- table(fac, CV.fac)
    names(dimnames(CV.tab)) <- c("actual", "classified")
    CV.correct <- sum(diag(CV.tab))/sum(CV.tab)
    # we calculate unstandardized LDs (wrong here for use in
    # shape reconstruction, would need one more step (PCA2shp?)
    # but not sure how useful it is)
    n <- nrow(X)
    lm.mod <- lm(X ~ fac)
    dfw <- n - nlevels(fac)
    SSw <- var(lm.mod$residuals) * (n - 1)
    VCVw <- SSw/dfw
    LDs <- VCVw %*% mod$scaling
    # 
    LDA <- list(x = X, fac = fac, removed = remove, mod = mod, 
        mod.pred = mod.pred, CV.fac = CV.fac, CV.tab = CV.tab, 
        CV.correct = CV.correct, LDs = LDs, mshape = NULL, method = "LDAPCA")  # may be interesting to add LDA on PCA here?
    class(LDA) <- c("LDA", class(LDA))
    return(LDA)
}

#' @export
print.LDA <- function(x, ...) {
    cat("Leave-one-out cross-validation: (", signif(x$CV.correct * 
        100, 3), "% - ", sum(diag(x$CV.tab)), "/", sum(x$CV.tab), 
        "): \n", sep = "")
    print(x$CV.tab)
}

##### end LDA 
