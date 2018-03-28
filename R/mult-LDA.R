.restore_LDs <- function(x){
  ids_drop <- c(x$ids_collinear, x$ids_constant)
  # nothing dropped case
  if (length(ids_drop)==0)
    return(x$LDs)
  ids_keep <- setdiff(1:ncol(x$x), ids_drop)
  # transpose unnormalized LDs so that columns are coe, rows LDs
  x_keep <- x$LDs %>% t
  # # build x_drop matrix from mean dropped coe
  # x_drop <- apply(x$x[, ids_drop], 2, mean) %>%
  #   list %>% rep(nrow(x_keep)) %>% do.call("rbind", .)
  x_drop <- matrix(0, nrow(x_keep), length(ids_drop))
  # dimnames it
  if (!is.null(names(ids_drop)))
    colnames(x_drop) <- names(ids_drop)
  rownames(x_drop) <- rownames(x_keep)

  # reinsert columns
  res <- reinsert_columns(x_keep, x_drop, ids_keep, ids_drop)

  # restore dimnames (overchecking but possibly safer)
  if (!is.null(colnames(x$x)))
    rownames(res) <- colnames(x$x)
  if (!is.null(rownames(x_keep)))
    colnames(res) <- rownames(x_keep)
  res
}

# LDA methods on Coe -------------

#' Linear Discriminant Analysis on Coe objects
#'
#' Calculates a LDA on [Coe] on top of [MASS::lda].
#'
#' @aliases LDA
#' @rdname LDA
#' @param x a  PCA object
#' @param fac the grouping factor (names of one of the $fac column or column id)
#' @param retain the proportion of the total variance to retain (if retain<1) using \link{scree}, or the number of PC axis (if retain>1).
#' @param ... additional arguments to feed \link{lda}
#' @note For LDA.PCA, retain can be passed as a vector (eg: 1:5, and retain=1, retain=2, ...,
#' retain=5) will be tried, or as "best" (same as before but retain=1:number_of_pc_axes is used).
#' @note Silent message and progress bars (if any) with `options("verbose"=FALSE)`.
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
#'  \item \code{CV.ce} class error
#'  \item \code{LDs} unstandardized LD scores see Claude (2008)
#'  \item \code{mshape} mean values of coefficients in the original matrix
#'  \item \code{method} inherited from the Coe object (if any)
#' }
#' @family multivariate
#' @examples
#' bot.f <- efourier(bot, 24)
#' bot.p <- PCA(bot.f)
#' LDA(bot.p, 'type', retain=0.99) # retains 0.99 of the total variance
#' LDA(bot.p, 'type', retain=5) # retain 5 axis
#' bot.l <- LDA(bot.p, 'type', retain=0.99)
#' bot.l
#' plot(bot.l)
#' bot.f$fac$plop <- factor(rep(letters[1:4], each=10))
#' bot.l <- LDA(PCA(bot.f), 'plop')
#' bot.l
#' plot(bot.l)
#' @export
LDA <- function(x, fac, retain, ...) {
  UseMethod("LDA")
}

#' @rdname LDA
#' @export
LDA.default <- function(x, fac, retain, ...) {
  x0 <- x
  # some checks
  if (!is.matrix(x))
    x <- as.matrix(x)
  if (missing(fac))
    stop("no fac provided")
  # dispatch fac
  f <- fac_dispatcher(x, fac)

  # handles constant and collinear variables
  ids_constant  <- which_constant(x)
  if (length(ids_constant)>0){
    ids_collinear <- which_collinear(x[, -ids_constant])
  } else {
    ids_collinear <- which_collinear(x)
  }

  # message about them, if verbose
  if (.is_verbose()){
    if (length(ids_constant)>0){
      if (is.null(colnames(x))){
        message("removed", length(ids_constant), "collinear columns")
      } else {
        message("removed these collinear columns:", paste(colnames(x)[ids_constant], collapse=", "))
      }
    }
    if (length(ids_collinear)>0){
      if (is.null(colnames(x))) {
        message("removed", length(ids_collinear), "collinear columns")
      } else {
        message("removed these collinear columns:", paste(colnames(x)[ids_collinear], collapse=", "))
      }
    }
  }

  # drop concerned columns
  ids_drop <- c(ids_constant, ids_collinear)
  if (length(ids_drop)>0)
    x <- x[, -c(ids_constant, ids_collinear)]

  # now we calculate two lda models with MASS::lda,
  #   1st - with  CV
  mod      <- MASS::lda(x = x, grouping=f, ...)
  mod.pred <- predict(mod, x)
  #   2nd - with leave-one-out cross validation
  CV.fac   <- MASS::lda(x=x, grouping=f,
                        tol = 1e-05, CV = TRUE, ...)$class

  # build a nice table from it
  CV.tab <- table(f, CV.fac)
  names(dimnames(CV.tab)) <- c("actual", "classified")
  CV.correct <- sum(diag(CV.tab))/sum(CV.tab)

  # return to unstandardized LDs
  n <- nrow(x)
  lm.mod <- lm(x ~ f)
  dfw <- n - nlevels(f)
  SSw <- var(lm.mod$residuals) * (n - 1)
  VCVw <- SSw/dfw
  LDs <- VCVw %*% mod$scaling

  # calculate class error
  tab <- CV.tab
  ce <- sapply(seq_along(1:nrow(tab)),
               function(i) 1-(sum(tab[i, -i])/sum(tab[i, ])))
  names(ce) <- rownames(tab)

  # we build the list to be returned
  res <- list(x = x0, fac = f,
              mod = mod,
              mod.pred = mod.pred,
              CV.fac = CV.fac,
              CV.tab = CV.tab,
              CV.correct = CV.correct,
              CV.ce = ce,
              LDs = LDs,
              ids_constant=ids_constant,
              ids_collinear=ids_collinear)

  if (is.list(x0) && !is.null(x0$mshape))
    res$mshape <- x0$mshape

  class(res) <- unique(c("LDA", class(res)))
  return(res)
}

#' @export
LDA.Coe <- function(x, fac, retain, ...) {
  # since important warnings are handled
  # and replace by messages
  # and that if something bad happens,
  # MASS:lda would stop anyway
  res <- suppressWarnings(
    LDA(x$coe, fac_dispatcher(x, fac))
  )
  if (!is.null(x$method))
    res$method <- x$method
  if (!is.null(x$cuts))
    res$cuts <- x$cuts
  # if (!is.null(x$mshape))
  #   res$mshape <- x$mshape

  res$mshape <- apply(x$coe, 2, mean)

  res
}

#' @rdname LDA
#' @export
LDA.PCA <- function(x, fac, retain = 0.99, ...) {

  if (length(retain)==1 && retain < 1)
    retain <- scree_min(x, retain)
  if (length(retain)==1 && retain == "best")
    retain <- 1:ncol(x$x)

  # select best case
  if (length(retain)>1){
    discri <- numeric(length(retain))
    names(discri) <- paste0("PC1:", retain)
    for (i in seq_along(discri)){
      discri[i] <- LDA(x=x, fac=fac, retain = retain[i])$CV.correct
    }
    return(discri)
  }

  PCA <- x
  f0 <- fac
  #fac handling
  fac <- fac_dispatcher(x, fac)

  if (.is_verbose()) message(retain, " PC retained")
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
  mod <- MASS::lda(X, grouping=fac, tol = 1e-08, ...)
  mod.pred <- predict(mod, X)
  # leave-one-out cross validation
  CV.fac <- MASS::lda(X, grouping = fac, tol = 1e-08, CV = TRUE, ...)$class
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
  # class error
  tab <- CV.tab
  ce <- numeric(nrow(tab))
  for (i in 1:nrow(tab)) ce[i] <- 1-(sum(tab[i, -i])/sum(tab[i, ]))
  names(ce) <- rownames(tab)

  LDA <- list(x = X, fac = fac, f0 = f0, removed = remove, mod = mod,
              mod.pred = mod.pred, CV.fac = CV.fac, CV.tab = CV.tab,
              CV.correct = CV.correct, CV.ce = ce, LDs = LDs,
              mshape = NULL, method = "LDAPCA")  # may be interesting to add LDA on PCA here?
  class(LDA) <- c("LDA", class(LDA))
  return(LDA)
}

#' @export
print.LDA <- function(x, ...) {

  cat(" * Cross-validation table ($CV.tab):\n")
  print(x$CV.tab)

  cat("\n * Class correctness ($CV.ce):\n")
  print(x$CV.ce)

  cat("\n * Leave-one-out cross-validation ($CV.correct): (",
      signif(x$CV.correct * 100, 3), "% - ",
      sum(diag(x$CV.tab)), "/", sum(x$CV.tab),
      "): \n", sep = "")

}

# LDA metrics -------

#' Calculate classification metrics on a confusion matrix
#'
#' In some cases, the class correctness or the proportion of correctly classified
#' individuals is not enough, so here are more detailed metrics when working on classification.
#'
#' @param x a \code{table} or an \link{LDA} object
#'
#' @return  a list with the following components is returned:
#' \enumerate{
#'   \item \code{accuracy}  the fraction of instances that are correctly classified
#'   \item \code{macro_prf} data.frame containing \code{precision}
#'   (the fraction of correct predictions for a certain class);
#'   \code{recall}, the fraction of instances of a class that were correctly predicted;
#'   \code{f1} the harmonic mean (or a weighted average) of precision and recall.
#'   \item \code{macro_avg}, just the average of the three \code{macro_prf} indices
#'   \item \code{ova} a list of one-vs-all confusion matrices for each class
#'   \item \code{ova_sum} a single of all ova matrices
#'   \item \code{kappa} measure of agreement between the predictions and the actual labels
#' }
#' @seealso  The pages below are of great interest to understand these metrics. The code
#' used is partley derived from the Revolution Analytics blog post (with their authorization). Thanks to them!
#' \enumerate{
#' \item \url{https://en.wikipedia.org/wiki/Precision_and_recall}
#' \item \url{http://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html}
#' \item \url{http://www.r-bloggers.com/is-your-classification-model-making-lucky-guesses/}
#' }
#' @family multivariate
#' @examples
#' # some morphometrics on 'hearts'
#' hearts %>% fgProcrustes(tol=1) %>%
#' coo_slide(ldk=1) %>% efourier(norm=FALSE) %>% PCA() %>%
#' # now the LDA and its summary
#' LDA(~aut) %>% classification_metrics()
#' @export
classification_metrics <- function(x){
  UseMethod("classification_metrics")
}

#' @export
classification_metrics.table <- function(x){
  tab <- x
  # check that a table is passed
  .check(is.table(tab),
         "only defined on 'table's")

  n <- sum(tab)           # nb of instances
  nc <- nrow(tab)         # nb of classes
  diag <- diag(tab)       # nb of correctly classified instances per class
  rowsums <- rowSums(tab) # nb of instances per class
  colsums <- colSums(tab) # nb of predictions per class
  p <- rowsums / n        # distribution of instances over the actual classes
  q <- colsums / n        # distribution of instances over the predicted classes

  # overall accuracy
  accuracy <- sum(diag) / n
  # precision, recall, F1
  precision <- diag / colsums
  recall <- diag / rowsums
  f1 <- 2 * precision * recall / (precision + recall)
  macro_prf <- dplyr::data_frame(precision, recall, f1)

  # macro precision, recall, f1
  macro_avg <- dplyr::data_frame(avg_precision=mean(precision),
                                 avg_recall=mean(recall),
                                 avg_f1=mean(f1))

  # one vs all
  ova = lapply(1 : nc,
               function(i){
                 v = c(tab[i,i],
                       rowsums[i] - tab[i,i],
                       colsums[i] - tab[i,i],
                       n-rowsums[i] - colsums[i] + tab[i,i]);
                 return(matrix(v, nrow = 2, byrow = T))})
  # nice names
  names(ova) <- colnames(tab)
  for (i in seq_along(ova)){
    dimnames(ova[[i]]) <- list(actual=c(rownames(tab)[i], "others"),
                               classified=c(colnames(tab)[i], "others"))
  }

  # we add all these matrices
  ova_sum <- Reduce("+", ova)
  dimnames(ova_sum) %<>% lapply(`[<-`, 1, "relevant")
  # micro <- dplyr::data_frame(accuracy=sum(diag(ova_sum)) / sum(ova_sum),
  # prf=(diag(ova_sum) / apply(ova_sum, 1, sum))[1])

  expAccuracy = sum(p*q)
  kappa = (accuracy - expAccuracy) / (1 - expAccuracy)

  list(accuracy=accuracy,
       macro_prf=macro_prf,
       macro_avg=macro_avg,
       ova = ova,
       ova_sum = ova_sum,
       kappa=kappa
  )
}

#' @export
classification_metrics.LDA <- function(x){
  classification_metrics(x$CV.tab)
}

# classify --------
# #' Classify using LDA
# #'
# #' @param x a Coe
# #' @param fac a standalone factor, or the name or id of the $fac column to use.If it contains
# #' NAs, they will also be removed first from the x object
# #' @param ref at least two level names from `$fac` to use as a training subset of x
# #' @param unk same as above for one level name to classify
# #'
# #' @return a list with components:
# #' \itemize{
# #' \item \code{$N_ref} the number of elements in the training set
# #' \item \code{$N_unk} the number of elements in the unknown set
# #' \item \code{$counts} counts of classification of 'unk' in each class of 'ref'
# #' \item \code{$pc} same thing as percentages
# #' \item \code{$probs} same thing as posterior probabilities
# #' \item \code{$probs} same thing as posterior but as a data.frame
# #' }
# #'
# #' @examples
# #' table(olea$var)
# #' x <- opoly(olea, 5)
# #' classify(x, fac="var", ref=c("Aglan","Cypre"), unk="PicMa")
# #' @export
# #' classify <- function(x, fac, ref, unk){
# #'   UseMethod("classify")
# #' }
# #' @export
# #' classify.default <- function(x, fac, ref, unk){
# #'   stop("method only available for objects of class 'Coe'")
# #' }
# #'
# #' @export
# #' classify.Coe <- function(x, fac, ref, unk){
# #'   # so that we can directly pass a fac
# #'   if (!is.factor(fac)){
# #'     fac <- x$fac[, fac]
# #'   }
# #'   # fac <- fac_dispatcher(x, fac)
# #'   # if any NAs, we remove them
# #'   if (any(is.na(fac))) {
# #'     x  <- x %>% slice(which(!is.na(fac)))
# #'     fac <- fac %>% na.omit() %>% factor()
# #'   }
# #'   # we filter for levels of interest
# #'   all_id  <- (fac %in% c(ref, unk))
# #'   # cat(all_id)
# #'   x <- slice(x, all_id)
# #'   fac <- fac[all_id]
# #'   # calculate a PCA using all taxa
# #'   P0 <- PCA(x)
# #'   # calculate an LDA using all but the unknown taxa
# #'   ref_id <- fac != unk
# #'   L0 <- P0 %>%
# #'     slice(ref_id) %>%
# #'     LDA(fac[ref_id], retain=0.99)
# #'   # extract and prepare scores of the unknown taxa
# #'   unk_id <- fac == unk
# #'   P1_all <- P0 %>%
# #'     slice(unk_id)
# #'   P1 <- P1_all$x[, 1:ncol(L0$x)]
# #'
# #'   # classify using the MASS::lda
# #'   pred <- predict(L0$mod, P1)
# #'   # prepare the results as a list
# #'   counts <- table(pred$class)
# #'   N_unk  <- sum(counts)
# #'   pc     <- round((counts / sum(counts))*100, 2)
# #'   probs  <- pred$posterior
# #' #
# #' #   probs_fac <- cbind(pred$posterior, select(P1_all$fac, Site, Period)) %>%
# #' #     group_by(Site, Period) %>%
# #' #     summarise_each(funs(mean))
# #' #   probs_fac <- dplyr::bind_cols(dplyr::select(probs_fac, 1:2), round(select(probs_fac, 3)))
# #'   probs_fac <- NULL
# #'
# #'   return(list(N_ref=nrow(L0$x),
# #'               N_ref_tab=table(L0$fac),
# #'               N_unk=N_unk,
# #'               counts=counts,
# #'               pc=pc,
# #'               probs=probs,
# #'               probs_fac=probs_fac))
# #' }

# reLDA -----------

#' "Redo" a LDA on new data
#'
#' Basically a wrapper around \link{predict.lda} from the package MASS. Uses a LDA model
#' to classify new data.
#' @param newdata to use, a \link{PCA} or any \link{Coe} object
#' @param LDA a \link{LDA} object
#' @return a list with components (from ?predict.lda ).
#' \itemize{
#' \item class factor of classification
#' \item posterior posterior probabilities for the classes
#' \item x the scores of test cases
#' \item res data.frame of the results
#' \item CV.tab a confusion matrix of the results
#' \item CV.correct proportion of the diagonal of CV.tab
#' \item newdata the data used to calculate passed to predict.lda
#' }
#' @note Uses the same number of PC axis as the LDA object provided. You should probably use \link{rePCA} in
#' conjunction with reLDA to get 'homologous' scores.
#' @examples
#' # We select the first 10 individuals in bot,
#' # for whisky and beer bottles. It will be our referential.
#' bot1   <- slice(bot, c(1:10, 21:30))
#' # Same thing for the other 10 individuals.
#' # It will be our unknown dataset on which we want
#' # to calculate classes.
#' bot2   <- slice(bot, c(11:20, 31:40))
#'
#' # We calculate efourier on these two datasets
#' bot1.f <- efourier(bot1, 8)
#' bot2.f <- efourier(bot2, 8)
#'
#' # Here we obtain our LDA model: first, a PCA, then a LDA
#' bot1.p <- PCA(bot1.f)
#' bot1.l <- LDA(bot1.p, "type")
#'
#' # we redo the same PCA since we worked with scores
#' bot2.p <- rePCA(bot1.p, bot2.f)
#'
#' # we finally "predict" with the model obtained before
#' bot2.l <- reLDA(bot2.p, bot1.l)
#' bot2.l
#'
#' @rdname reLDA
#' @export
reLDA <- function(newdata, LDA){
  UseMethod("reLDA")
}

#' @rdname reLDA
#' @export
reLDA.default <- function(newdata, LDA){
  stop("method only defined for LDA objects")
}

#' @rdname reLDA
#' @export
reLDA.PCA <- function(newdata, LDA){
  #   if (missing(newdata) | !any(class(newdata) == "PCA"))
  #     stop(" * a PCA object must be provided")
  mod <- LDA$mod
  nc <- ncol(LDA$x)
  reLDA <- predict(mod, newdata$x[, 1:nc])
  #   return(reLDA)
  #   if (length(LDA$f0)==1) {
  #     actual <- newdata$fac[, LDA$f0]
  #     if (!is.null(actual)) {
  #       reLDA$res <- dplyr::data_frame(actual=actual, classified=reLDA$class)
  #       reLDA$CV.tab <- table(reLDA$res)
  #       reLDA$CV.correct <- sum(diag(reLDA$CV.tab)) / sum(reLDA$CV.tab)
  #     }
  #   }
  reLDA$newdata <- newdata$x[, 1:nc]
  #   class(reLDA) <- "LDA"
  return(reLDA)
}

#' @rdname reLDA
#' @export
reLDA.Coe <- function(newdata, LDA){
  #   if (missing(newdata) | !any(class(newdata) == "PCA"))
  #     stop(" * a PCA object must be provided")
  mod <- LDA$mod
  if (!identical(colnames(LDA$x),  colnames(newdata$coe)))
    stop("LDA and newdata object do not have the same structure")
  return(predict(mod, newdata$coe))
}



