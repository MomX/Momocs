# ready -------

#' Tests the accuracy of LDAs as a function of the number of classes
#'
#' Useful to test the relation between the number of classes and the LDA accuracy.
#'
#' @param OutCoe object
#' @param fac column from the \code{$fac} to use
#' @param nbOfClasses numeric vector of number of classes to test; or a
#' numeric vector of length two with proportions; or "range" (default value) to
#' test all along the range.
#' @param replicates_per_nbOfClasses numeric number of replicates for each value of \code{nbOfClasses}
#' @param plot logical whether to plot. If TRUE the results are returned invisibly.
#' @param ... arguments to feed \link{LDA}
#'
#' @details For each value in \code{nbOfClasses}, this function will sample this number
#' of categories withing the \code{fac} column, then calculates the LDA accuracy (after a PCA).
#' This is done \code{replicates_per_nbOfClasses} times per values of \code{nbOfClasses}.
#' @return a `data.frame` is returned with all tests (invisibly if `plot = TRUE`).
#' @family LDA
#' @examples
#' x <- hearts %>% fgProcrustes() %>% coo_slide(ldk=1) %>%
#'      efourier(nb.h=6, norm=F)
#' y <- LDA_accuracy_vs_nbOfClasses(x, "aut", "range") # all levels from binary to 8
#' y # results are here
#' # LDA_accuracy_vs_nbOfClasses(x, "aut", 2:6) # you can specify explicitely
#' # LDA_accuracy_vs_nbOfClasses(x, "aut", c(0.5, 1)) # proportion
#' @export
LDA_accuracy_vs_nbOfClasses <- function(x,
                                        fac,
                                        nbOfClasses="range",
                                        replicates_per_nbOfClasses=100,
                                        plot=TRUE,
                                        ...){
  # checks a Coe is provided
  .check(is_Coe(x),
         "only works on Coe objects")

  # levels of fac, where to sample
  levs <- levels(factor(x$fac[, fac]))

  # if 'range' is provided
  if (length(nbOfClasses)==1 && nbOfClasses=="range")
    nbOfClasses <- 2:length(levs)
  # if 'proportion' is provided
  if (length(nbOfClasses)==2 && any(nbOfClasses<1)){
    nbOfClasses <- round(nbOfClasses * length(levs))
    nbOfClasses <- nbOfClasses[1]:nbOfClasses[2]
    if (nbOfClasses[1]<2)
      nbOfClasses[1] <- 2
  }
  # check that nbOfClasses vector makes sense
  .check(max(nbOfClasses)<=length(levs),
         "none nbOfClasses cannot be higher than the number of levels")
  .check(min(nbOfClasses)>=2,
         "none nbOfClasses cannot be lower than 2")
  # to store the results
  res <- list()
  # loop along all nbOfClasses, and sample some levels, filter the Coe,
  # PCA and LDA. accuracy only is saved.
  for (i in seq_along(nbOfClasses)){
    # no idea why below doest work
    # res1 <- replicate(replicates_per_nbOfClasses, {
    #   levs1 <- sample(levs, size = nbOfClasses[i], replace=FALSE)
    #   x %>%
    #     subset(x$fac[, fac] %in% levs1) %>%
    #     PCA %>%
    #     LDA(fac, verbose=FALSE, ...) %$%
    #     CV.correct })
    # good old while
    k<-0
    while (k < replicates_per_nbOfClasses){
      levs1 <- sample(levs, size = nbOfClasses[i], replace=FALSE)
      res <- c(res,
               x %>%
                 subset(x$fac[, fac] %in% levs1) %>%
                 PCA %>%
                 LDA(fac, verbose=FALSE, ...) %$%
                 CV.correct)
      k <- k+1
      cat(".")
    }
  }
  # combines results into a data.frame
  df <- dplyr::data_frame(nbOfClasses=rep(nbOfClasses, each=replicates_per_nbOfClasses),
                          accuracy=unlist(res))
  if (plot) {
    gg <- ggplot(df, aes(y=accuracy, x=factor(nbOfClasses))) +
      geom_boxplot() + labs(y="Accuracy", x="Number of classes")
    print(gg)
    invisible(df)
  } else {
    return(df)
  }
}

#######


#' Calculates the LDA accuracy expected when reshuffling groups
#'
#' Given a \code{Coe} object and a factor to use with \code{LDA}, one
#' obtains an accuracy (ie \code{$CV.correct} on the resulting object).$
#' To judge how good it is, it may be useful to compare it to the accuracies
#' obtained using permutations. This function shuffles the factor of interest,
#' and calculate the distribution of the accuracy under this null hypothesis.
#' It may take long to calculate but may hel provide a more realistic information than
#' the rough comparison of obtained accuracy with \code{1/number_of_classes}.
LDA_null_accuracy <- function(x, fac, replicates=10, ...){
  obs <- x %>% PCA %>% LDA(fac, verbose=FALSE, ...) %$% CV.correct

  f0 <- .fac_dispatcher(x, fac)
  null <- vector("numeric", replicates)

  for (i in 1:replicates){
    null[i] <- x %>% PCA %>% LDA(factor(sample(f0)), verbose=FALSE, ...) %$% CV.correct
    cat(".")
  }
  list(null=null, obs=obs)
}

a <- x %>% LDA_null_accuracy("var", replicates = 5)
