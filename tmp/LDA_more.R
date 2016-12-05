library(Momocs)
oll <- olea %>% opoly() %>% PCA %>% LDA(~var)
oll$CV.tab %>% summary

#' Calculate summary statistics on a confusion matrix
#'
#' @details
#' \itemize{
#' \item \bold{accuracy}: fraction of instances that are correctly classified
#' \item \bold{precision} (or positive predictive value): fraction of correct predictions for a certain class
#' \item \bold{recall} (or sensitivity): fraction of instances of a class that were correctly predicted
#' \item \bold{f1}: harmonic mean (or a weighted average) of precision and recall,
#' ie \code{2 * precision * recall / (precision + recall) }
#' }
#' Quoting \href{https://en.wikipedia.org/wiki/Precision_and_recall}{Wikipedia}:
#' "high precision means that an algorithm returned substantially
#' more relevant results than irrelevant ones, while high recall means that an
#' algorithm returned most of the relevant results."
#' \itemize{
#' \item \bold{ova} (one versus all): a list with as many binary confusion matrices as there are levels
#' \item \bold{ova_accuracy}: same as above but on ova
#' \item \bold{ova_prf} (precision, recall, f1): as ova is symmetric, these three statistics are equal
#' }
#'
summary_tab <- function(tab){
  # check that a table is passed
  Momocs:::.check(is.table(tab),
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
  macro_prf <- data.frame(precision, recall, f1)

  # macro precision, recall, f1
  macro_avg <- data.frame(avg_precision=mean(precision),
                          avg_recall=mean(recall),
                          avg_f1=mean(f1))

  # one vs all
  ova = lapply(1 : nc,
               function(i){
                 v = c(cm[i,i],
                       rowsums[i] - cm[i,i],
                       colsums[i] - cm[i,i],
                       n-rowsums[i] - colsums[i] + cm[i,i]);
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
  micro <- data.frame(accuracy=sum(diag(ova_sum)) / sum(ova_sum),
                      prf=(diag(ova_sum) / apply(ova_sum, 1, sum))[1])

  expAccuracy = sum(p*q)
  kappa = (accuracy - expAccuracy) / (1 - expAccuracy)

  list(accuracy=accuracy,
       macro_prf=macro_prf,
       macro_avg=macro_avg,
       ova = ova,
       ova_sum = ova_sum,
       micro=micro,
       kappa=kappa
  )
}


oll$CV.tab %>% summary_tab() %>% print
