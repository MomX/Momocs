### Todo : most of it will move to Momit/Momacs


# txt -----------------------
#' Import coordinates from a .txt file
#'
#' A wrapper around \link{read.table}
#' that can be used to import outline/landmark coordinates.
#'
#' Columns are not named in the \code{.txt} files. You can tune this using the \code{...} argument.
#' Define the \link{read.table} arguments that allow to import a single file, and then
#' pass them to this function, ie if your \code{.txt} file
#' has a header (eg ('x', 'y')), do not forget \code{header=TRUE}.
#' @param txt.paths a vector of paths corresponding to the .txt files to import. If not
#' provided (or \code{NULL}), switches to the automatic version, just as in \link{import_jpg}.
#' See Details there.
#' @param ... arguments to be passed to \link{read.table}, eg. 'skip', 'dec', etc.
#'
#' @note Note this function will be deprecated from Momocs
#' when \code{Momacs} and \code{Momit}
#' will be fully operationnal.
#'
#' @note Silent message and progress bars (if any) with `options("verbose"=FALSE)`.
#' @return a list of matrix(ces) of (x; y) coordinates that can be passed to
#' \link{Out}, \link{Opn} and \link{Ldk}.
#' @family import functions
#' @export
import_txt <- function(txt.paths = .lf.auto(), ...) {
  # if (is.null(txt.paths)) {
  #   txt.paths <- .lf.auto()
  # }
  if (.is_verbose())
    cat(" * Extracting ", length(txt.paths), "..txt coordinates...\n")
  if (length(txt.paths) > 10 & .is_verbose()) {
    pb <- progress::progress_bar$new(total = length(txt.paths))
    t <- TRUE
  } else {
    t <- FALSE
  }
  res <- list()
  for (i in seq(along = txt.paths)) {
    coo <- utils::read.table(txt.paths[i], ...)
    res[[i]] <- as.matrix(coo)
    if (t)
      pb$tick()
  }
  # names(res) <- substr(txt.paths, start=1,
  # stop=nchar(txt.paths)-4)
  names(res) <- txt.paths %>% .trim.ext() %>% .trim.path()
  return(res)
}

# outlines ------------------
#' Extract outlines coordinates from an image silhouette
#'
#' Provided with an image 'mask' (i.e. black pixels on a white background),
#' and a point form where to start the algorithm, returns the (x; y) coordinates of its outline.
#'
#' Used internally by \link{import_jpg1} but may be useful for other purposes.
#' @param img a matrix of a binary image mask.
#' @param x numeric the (x; y) coordinates of a starting point within the shape.
#' @return a matrix the (x; y) coordinates of the outline points.
#'
#' @note Note this function will be deprecated from Momocs
#' when \code{Momacs} and \code{Momit}
#' will be fully operationnal.
#'
#' @references
#' \itemize{
#' \item The original algorithm is due to: Pavlidis, T. (1982). \emph{Algorithms
#' for graphics and image processing}. Computer science press.
#' \item is detailed in: Rohlf, F. J. (1990). An overview of image processing and
#' analysis techniques for morphometrics. In \emph{Proceedings of the Michigan Morphometrics Workshop}. Special Publication No. 2 (pp. 47-60). University of Michigan Museum of Zoology: Ann Arbor.
#' \item and translated in R by: Claude, J. (2008). \emph{Morphometrics with R}. (p. 316). Springer.
#' }
#'
#' @note If you have an image with more than a single shape, then
#' you may want to try `imager::highlight` function. Momocs may use this at some point.
#' @family import functions
#' @export
import_Conte <- function(img, x) {
  while (abs(img[x[1], x[2]] - img[x[1] - 1, x[2]]) < 0.1) {
    x[1] <- x[1] + 1
  }
  # while (abs(img[x[1], x[2]] - img[x[1], x[2]-1]) < 0.1) {
  # x[2] <- x[2] -1 }
  a <- 1
  M <- matrix(c(0, -1, -1, -1, 0, 1, 1, 1, 1, 1, 0, -1, -1,
                -1, 0, 1), nrow = 2, ncol = 8, byrow = TRUE)
  M <- cbind(M[, 8], M, M[, 1])
  X <- 0
  Y <- 0
  x1 <- x[1]
  x2 <- x[2]
  SS <- NA
  S <- 6
  while ((any(c(X[a], Y[a]) != c(x1, x2)) | length(X) < 3)) {
    if (abs(img[x[1] + M[1, S + 1], x[2] + M[2, S + 1]] -
            img[x[1], x[2]]) < 0.1) {
      a <- a + 1
      X[a] <- x[1]
      Y[a] <- x[2]
      x <- x + M[, S + 1]
      SS[a] <- S + 1
      S <- (S + 7)%%8
    } else if (abs(img[x[1] + M[1, S + 2], x[2] + M[2, S +
                                                    2]] - img[x[1], x[2]]) < 0.1) {
      a <- a + 1
      X[a] <- x[1]
      Y[a] <- x[2]
      x <- x + M[, S + 2]
      SS[a] <- S + 2
      S <- (S + 7)%%8
    } else if (abs(img[x[1] + M[1, S + 3], x[2] + M[2, S +
                                                    3]] - img[x[1], x[2]]) < 0.1) {
      a <- a + 1
      X[a] <- x[1]
      Y[a] <- x[2]
      x <- x + M[, S + 3]
      SS[a] <- S + 3
      S <- (S + 7)%%8
    } else {
      S <- (S + 1)%%8
    }
  }
  return(cbind((Y[-1]), ((dim(img)[1] - X))[-1]))
}

#' Extract outline coordinates from a single .jpg file
#'
#' Used to import outline coordinates from .jpg files. This function is used for
#' single images and is wrapped by \link{import_jpg}. It relies itself on \link{import_Conte}
#' @param jpg.path vector of paths corresponding to the .jpg files to import, such as
#' those obtained with \link{list.files}.
#' @param auto.notcentered logical if TRUE random locations will be used until
#' one of them is (assumed) to be within the shape (because it corresponds to a black pixel) and only if
#' the middle point is not black;
#' if FALSE a \link{locator} will be called, and you will have to click on a
#' point within the shape.
#' @param fun.notcentered NULL by default but can accept a function that, when passed with an imagematrix and returns
#' a numeric of length two that corresponds to a starting point on the imagematrix for the Conte
#' algorithm. A \code{while} instruction wraps it, so the function may be wrong in proposing this
#' starting position. See the examples below for a quick example.
#' @param threshold the threshold value use to binarize the images. Above, pixels
#' are turned to 1, below to 0.
#' @param ... arguments to be passed to \link{read.table}, eg. 'skip', 'dec', etc.
#'
#' @note Note this function will be deprecated from Momocs
#' when \code{Momacs} and \code{Momit}
#' will be fully operationnal.
#'
#'
#' @details jpegs can be provided either as RVB or as 8-bit greylevels or monochrome.
#' The function binarizes pixels values using the 'threshold' argument. It will try to start to
#' apply the \link{import_Conte} algorithm from the center of
#' the image and 'looking' downwards for the first black/white 'frontier' in
#' the pixels. This point will be the first of the outlines. The latter may be useful
#' if you align manually the images and if you want to retain this information
#' in the consequent morphometric analyses.
#'
#' If the point at the center of the
#' image is not within the shape, i.e. is 'white' you have two choices defined by
#' the 'auto.notcentered' argument. If it's TRUE, some random starting points
#' will be tried until on of them is 'black' and within the shape; if FALSE
#' you will be asked to click on a point within the shape.
#'
#' If some pixels on the borders are not white, this functions adds a 2-pixel
#' border of white pixels; otherwise \link{import_Conte} would fail and return an error.
#'
#' Finally, remember that if the images are not in your working directory,
#' \link{list.files} must be called with the argument \code{full.names=TRUE}!
#'
#' Note that the use of the \code{fun.notcentered} argument will probably leads to serious headaches
#' and will probably imply the dissection of these functions: \link{import_Conte}, \link{img_plot} and
#' \code{import_jpg} itself
#' @seealso \link{import_jpg}, \link{import_Conte}, \link{import_txt}, \link{lf_structure}.
#' See also Momocs' vignettes for data import.
#' @return a matrix of (x; y) coordinates that can be passed to Out
#' @family import functions
#' @export
import_jpg1 <- function(jpg.path,
                        auto.notcentered = TRUE,
                        fun.notcentered = NULL,
                        threshold = 0.5) {
  img <- jpeg::readJPEG(jpg.path)
  # if a RVB is provided by the way, apply (img, 1:2, mean) is
  # much slower
  if (!is.matrix(img)) {
    img <- (img[, , 1] + img[, , 2] + img[, , 3])/3
  }
  # we binarize using threshold
  img[img > threshold] <- 1
  img[img <= threshold] <- 0
  # we test for images with black pixels on their border which
  # causes Conte to fail. IF it is the case, we add a 1 pixel
  # white border around
  borders <- c(img[1, ], img[nrow(img), ], img[, 1], img[,
                                                         ncol(img)])
  if (any(borders != 1)) {
    .mat.buffer <- function(m, buff.size, buff.fill = 1) {
      nr <- nrow(m)
      c.buff <- matrix(buff.fill, nrow = nr, ncol = buff.size)
      m <- cbind(c.buff, m, c.buff)
      nc <- ncol(m)
      r.buff <- matrix(buff.fill, nrow = buff.size, ncol = nc)
      m <- rbind(r.buff, m, r.buff)
      return(m)
    }

    img <- .mat.buffer(img, buff.size = 2, buff.fill = 1)
  }
  # img <- x[dim(x)[1]:1,] #Conte/readJPEG, etc.  we initialize
  # value with the middle or (1,1)
  if (is.null(fun.notcentered)) {
    x <- round(dim(img)/2)
  } else {
    x <- c(1, 1)
  }
  # while we dont start a black pixel (ie img[x1, x2]==0) to
  # start Conte, we search for it k is the number of tries
  # tries_max <- 50 tries <- 1 while (img[x[1], x[2]] != 0 &
  # tries < tries_max) {
  while (img[x[1], x[2]] != 0) {
    # etiher with a smart function, if provided
    if (!is.null(fun.notcentered)) {
      x <- fun.notcentered(img)
    }
    # either randomly picking points
    if (auto.notcentered) {
      while (img[x[1], x[2]] != 0) {
        x[1] <- sample(dim(img)[1], 1)
        x[2] <- sample(dim(img)[2], 1)
      }
    }
    # or manually by plotting the image and begging for a click
    if (is.null(fun.notcentered) & !auto.notcentered) {
      img_plot(img)
      while (img[x[1], x[2]] != 0) {
        message("Click a point within the shape")
        xy <- unlist(locator(1))
        x <- round(c(nrow(img) - xy[2], xy[1]))
        if (x[1] > dim(img)[1]) {
          x[1] <- dim(img)[1]
        }
        if (x[1] < 1) {
          x[1] <- 1
        }
        if (x[2] > dim(img)[2]) {
          x[2] <- dim(img)[2]
        }
        if (x[2] < 1) {
          x[2] <- 1
        }
      }
      # cat(xy, '\t', x, '\n') here, for tests
    }
    # tries <- tries + 1 cat(tries)
  }
  # if (tries >= tries_max) { return(NA) } Then we start
  # import_Conte
  out <- import_Conte(img, x)
  return(out)
}

#' Extract outline coordinates from multiple .jpg files
#'
#' This function is used to import outline coordinates and is built around
#' \link{import_jpg1}.
#' @param jpg.paths a vector of paths corresponding to the .jpg files to import. If not
#' provided (or \code{NULL}), switches to the automatic version. See Details below.
#' @param auto.notcentered logical if TRUE random locations will be used until.
#' one of them is (assumed) to be within the shape (because of a black pixel);
#' if FALSE a \link{locator} will be called, and you will have to click on a
#' point within the shape.
#' @param fun.notcentered NULL by default. Is your shapes are not centered and if a random pick of
#' a black pixel is not satisfactory. See \link{import_jpg1} help and examples.
#' @param threshold the threshold value use to binarize the images. Above, pixels
#' are turned to 1, below to 0.
#'
#' @note Note this function will be deprecated from Momocs
#' when \code{Momacs} and \code{Momit}
#' will be fully operationnal.
#'
#' @note Silent message and progress bars (if any) with `options("verbose"=FALSE)`.
#'
#' @details see \link{import_jpg1} for important informations about how the outlines are extracted,
#' and \link{import_Conte} for the algorithm itself.
#'
#' If \code{jpg.paths} is not provided (or \code{NULL}), you will have to select any \code{.jpg}
#' file in the folder that contains all your files. All the outlines should be imported then.
#' @return a list of matrices of (x; y) coordinates that can be passed to \link{Out}
#' @examples
#' \dontrun{
#'
# # if your images are in the folder '/foo/jpgs/'
#' lf <- list.files('/foo/jpegs', full.names=TRUE)
#' coo <- import_jpg(lf)
#' Out(coo)
#'
# # 'automatic' version
#' coo <- import_jpg()
#' }
#' @family import functions
#' @export
import_jpg <- function(jpg.paths = .lf.auto(), auto.notcentered = TRUE,
                       fun.notcentered = NULL, threshold = 0.5) {
  # if not provided
  # if (is.null(jpg.paths)) {
  #   jpg.paths <- .lf.auto()
  # }
  begin <- Sys.time()
  message("Extracting ", length(jpg.paths), ".jpg outlines...")
  if (length(jpg.paths) > 10 & .is_verbose()) {
    pb <- progress::progress_bar$new(total = length(jpg.paths))
    t <- TRUE
  } else {
    t <- FALSE
  }
  # for a futurer safer import jpg.names <-
  # .trim.path(.trim.ext(jpgs.paths))
  res <- list()
  n <- length(jpg.paths)
  for (i in seq(along = jpg.paths)) {
    if (.is_verbose()) {
      cat("[", i, "/", n, "] ", .trim.path(jpg.paths[i]))
    }
    coo_i <- import_jpg1(jpg.paths[i], auto.notcentered = auto.notcentered,
                         fun.notcentered = fun.notcentered, threshold = threshold)
    res[[i]] <- coo_i
    # if (export){coo_export(coo_i, jpg.paths[i])}
    if (.is_verbose()) {
      cat("\n")
    } else {
      if (t)
        pb$tick()
    }
  }
  names(res) <- jpg.paths %>% .trim.ext() %>% .trim.path()
  if (.is_verbose()) {
    end <- Sys.time()
    time <- end - begin
    message("Done in ", round(as.numeric(time), 1), " ", units(time))
  }
  return(res)
}

# StereoMorph ---------------
#' Import files creates by StereoMorph into Momocs
#'
#' Helps to read \code{.txt} files created by StereoMorph into (x; y) coordinates
#' or Momocs objects. Can be applied to 'curves' or 'ldk' text files.
#' @param path toward a single file or a folder containing \code{.txt} files produced by StereoMorph
#' @param names to feed \link{lf_structure}
#'
#' @note Note this function will be deprecated from Momocs
#' when \code{Momacs} and \code{Momit}
#' will be fully operationnal.
#'
#'
#' @details *1 functions import a single \code{.txt} file. Their counterpart (no '1')
#' work when path indicates the folder, i.e. 'curves' or 'ldk'. They then return a list
#' of \link{Opn} or \link{Ldk} objects, respectively. Please do not hesitate to contact me
#' should you have a particular case or need something.
#' @rdname import_StereoMorph
#' @family import functions
#' @export
import_StereoMorph_curve1 <- function(path){
  # tem fix
  stop("deprecated due to major update in StereoMorph export file format. You can try the solution by Stas Malavin before this being fixed in Momocs: https://github.com/stas-malavin/Momocs/blob/master/R/importSM.r")

  # we split the loci contained in the first column
  df <- utils::read.table(path, header=FALSE, stringsAsFactors = FALSE) %>%
    select(locus=1, x=2, y=3) %>%
    mutate(name=substr(locus, 1, nchar(locus)-4)) %>%
    select(name, x, y)
  return(split(df[, c("x", "y")], as.factor(df$name)))
}

#' @rdname import_StereoMorph
#' @family import functions
#' @export
import_StereoMorph_curve <- function(path, names){
  # tem fix
  stop("deprecated due to major update in StereoMorph export file format. You can try the solution by Stas Malavin before this being fixed in Momocs: https://github.com/stas-malavin/Momocs/blob/master/R/importSM.r")

  # we extract filenames and import them
  lf <- list.files(path, full.names=TRUE)
  res <- lapply(lf, import_StereoMorph_curve1)
  names(res) <- lf %>% .trim.ext() %>% .trim.path()
  # so that we have a list of loci that include filenames that we turn into Opns
  res <- .rollup_list(res)
  res <- lapply(res, Opn)
  # if names are provided we apply them
  if (!missing(names)) {
    for (i in seq_along(res))
      res[[i]]$fac <- lf_structure(names(res[[i]]), names)
  }
  return(res)
}

#' @rdname import_StereoMorph
#' @family import functions
#' @export
import_StereoMorph_ldk1 <- function(path){
  # tem fix
  stop("deprecated due to major update in StereoMorph export file format. You can try the solution by Stas Malavin before this being fixed in Momocs: https://github.com/stas-malavin/Momocs/blob/master/R/importSM.r")

  # a cousin of import_txt
  utils::read.table(path, header=FALSE,
             row.names=1, col.names=c("l", "x", "y"), stringsAsFactors = FALSE)
}

#' @rdname import_StereoMorph
#' @family import functions
#' @export
import_StereoMorph_ldk <- function(path, names){
  # tem fix
  stop("deprecated due to major update in StereoMorph export file format. You can try the solution by Stas Malavin before this being fixed in Momocs: https://github.com/stas-malavin/Momocs/blob/master/R/importSM.r")

  # we extract filenames and import them
  lf <- list.files(path, full.names=TRUE)
  res <- lapply(lf, import_StereoMorph_ldk1)
  names(res) <- lf %>% .trim.ext() %>% .trim.path()
  # so that we have a list of loci that include filenames that we turn into Opns
  res <- Ldk(res)
  # if names are provided we apply them
  if (!missing(names)) {
    res$fac <- lf_structure(names(res), names)
  }
  return(res)
}

# tps -----------------------
#' Import a tps file
#'
#' And returns a list of coordinates, curves, scale
#' @param tps.path lines, typically from \link{readLines}, describing a single shape in tps-like format.
#' You will need to manually build your \code{Coo} object from it: eg \code{Out(coo=your_list$coo)}.
#' @param curves \code{logical} whether to read curves, if any
#' @param tps lines for a single tps file
#' \code{tps2coo} is used in \link{import_tps} and may be useful for data import. When provided
#' with lines (eg after \link{readLines}) from a tps-like description (with "LM", "CURVES", etc.) returns a list of
#' coordinates, curves, etc.
#'
#' @note Note this function will be deprecated from Momocs
#' when \code{Momacs} and \code{Momit}
#' will be fully operationnal.
#'
#' @return a list with components:
#' \code{coo} a matrix of coordinates; \code{cur} a list of matrices; \code{scale} the scale as a numeric.
#' @rdname import_tps
#' @family import functions
#' @export
import_tps <- function(tps.path, curves=TRUE){
  # we import the tps, line by line
  tps <- readLines(tps.path)
  # we remove empty lines, if any
  tps <- tps[nchar(tps) != 0]
  # we detect the position of 'LM'
  LM.pos <- grep("LM", tps)
  # we extract shape names, if IMAGE is present, use it; else ID, else we create them
  if (length(grep("IMAGE", tps))>0){
    shp.names <- .trim.ext(gsub("IMAGE=", "", grep("IMAGE", tps, value=TRUE)))
  } else {
    if(length(grep("ID", tps))>0){
      shp.names <- gsub("ID=", "",    grep("ID", tps, value=TRUE))
    } else {
      shp.names <-  paste0("id", 1:length(LM.pos))
    }
  }
  # we prepare the blocks of shapes
  shp.pos <- data.frame(start = LM.pos, end = c(LM.pos[-1]-1, length(tps)))
  res <- vector("list", nrow(shp.pos))
  for (i in seq_along(res)){
    res[[i]] <- tps[shp.pos$start[i]:shp.pos$end[i]] %>% tps2coo(curves=curves)
  }
  names(res) <- shp.names
  # if only landmarks, we return a list of matrices
  classes <- sapply(res, class) %>% unique()
  if (!curves)
    return(res)
  # curves=FALSE case
  # also we check a bit for case where curves=TRUE but some curves are missing
  classes <- sapply(res, class)
  # in such a case, lists are expected to be returned by tps2coo
  if (any(classes!="list")){
    stop("these shapes do not have curves:", shp.names[which(classes != "list")])
  }
  return(.rollup_list(res))
}

#' @rdname import_tps
#' @family import functions
#' @export
tps2coo <- function(tps, curves=TRUE){
  scale  <- NULL
  cur <- NULL
  # we read the nb of landmarks
  # some tps files (for outlines?) have a special
  # format with no LM= but POINTS=
  gPOINTS <- grep("POINTS=", tps)
  if (length(gPOINTS)>0){
    coo.nb <- tps[gPOINTS] %>% gsub("POINTS=", "", .) %>% as.numeric()
  } else {
    coo.nb <- as.numeric(gsub("LM=", "", tps[1]))
  }
  # we read "SCALE=", if any
  scale  <- as.numeric(gsub("SCALE=", "", grep("SCALE=", tps, value=TRUE)))
  # removes the first line "LM=" and, if any, "IMAGE=", "ID=" and "SCALE=" lines
  rm.ids <- c(1, grep("IMAGE|ID|SCALE|OUTLINES|POINTS", tps))
  tps <- tps[-rm.ids]
  # this function turns lines of coordinates into a shp
  lines2shp <- function(l) {
    l %>% strsplit(" ") %>% unlist() %>%
      as.numeric() %>%
      matrix(nrow=length(l), byrow=TRUE)
  }
  # here we extract coos and check a bit
  coo <- tps[1:coo.nb] %>% lines2shp()
  .check(nrow(coo)==coo.nb,
         "the number of landmarks seems to differ from LM=")
  # no curve case
  if (!curves)
    return(list(coo=coo, cur=cur, scale=scale))
  # curve case
  # we check that curves are really present, if not we return coo
  if (length(grep("CURVE", tps))==0)
    return(list(coo=coo, cur=cur, scale=scale))
  # we get rid of landmarks and "CURVES" lines
  tps <- tps[-c(1:coo.nb, coo.nb+1)]
  # we extract POINTS ids and deduce curves ids
  cur.start <- grep("POINTS", tps)+1
  cur.end   <- c(cur.start[-1]-2, length(tps))
  # we create a list to store curves coordinates
  cur <- vector("list", length(cur.start))
  for (i in seq_along(cur.start)){
    cur[[i]] <- tps[cur.start[i]:cur.end[i]] %>% lines2shp
  }
  return(list(coo=coo, cur=cur, scale=scale))
}

# other formats -------------

#' Convert (x; y) coordinates to chaincoded coordinates
#'
#' Useful to convert (x; y) coordinates to chain-coded coordinates.
#' @param coo (x; y) coordinates passed as a matrix
#' @param chc chain coordinates
#' @seealso \link{chc2pix}
#'
#' @note Note this function will be deprecated from Momocs
#' when \code{Momacs} and \code{Momit}
#' will be fully operationnal.
#'
#' @references Kuhl, F. P., & Giardina, C. R. (1982).
#' Elliptic Fourier features of a closed contour.
#' \emph{Computer Graphics and Image Processing}, 18(3), 236-258.
#' @examples
#' pix2chc(shapes[1]) %T>% print %>% # from pix to chc
#' chc2pix()                         # and back
#' @rdname babel
#' @family import functions
#' @export
pix2chc <- function(coo) {
  if (is.list(coo)) {
    coo <- l2m(coo)
  }
  if (is.matrix(coo) & ncol(coo) != 2) {
    stop("a 2-col matrix must be provided")
  }
  coo_d <- apply(coo, 2, diff)
  if (!all(coo_d %in% -1:1)) {
    stop("matrix must contain only entire pixels indices")
  }
  if (any(apply(coo_d, 1, function(x) all(x == rep(0, 2))))) {
    stop("at least two successive coordinates do not code for a displacement")
  }
  m <- as.matrix(expand.grid(-1:1, -1:1))[-5, ]
  g <- c(5, 6, 7, 4, 0, 3, 2, 1)
  chc <- g[apply(coo_d, 1, function(x) which(x[1] == m[, 1] &
                                               x[2] == m[, 2]))]  #dirty
  return(chc)
}

#' @rdname babel
#' @family import functions
#' @export
chc2pix <- function(chc) {
  if (!all(chc %in% 0:7)) {
    stop("chc string must only contain integers between 0 and 7")
  }
  m <- matrix(c(1, 0, 1, 1, 0, 1, -1, 1, -1, 0, -1, -1, 0,
                -1, 1, -1), ncol = 2, byrow = TRUE)
  pix <- apply(m[chc + 1, ], 2, cumsum)
  return(pix)
}

# #' Converts chain-coded coordinates to Out object
# #'
# #' For Shape/ChainCoder files, a wrapper to convert chain-coded coordinates to
# #' \code{\link{Out}} objects.
# #' @param chc a path to the chc file
# #' @param skip numeric how many informations before the first chain-coded information
# #' @param names an (optional) vector of (skip) names for the \code{fac} created.
# #' Somehow similar to \code{names} in \link{lf_structure}
# #'
# #' @note Note this function will be deprecated from Momocs
# #' when \code{Momacs} and \code{Momit}
# #' will be fully operationnal.
# #'
# #' @details
# #' Files from Shape/ChainCoder comes this way:
# #' \preformatted{
# #' Name_1 fac1 fac2 fac3 [...] 6 6 6 6 6 6 6 6 7 6 [...] -1
# #' Name_2 fac1 fac2 fac3 [...] 6 6 6 6 5 5 7 6 7 6 [...] -1
# #' }
# #' This function does the following:
# #' \enumerate{
# #' \item take everything before the first chain-coded
# #' coordinate (here a "6") and transform it into a \code{data.frame}, later used
# #' as a \code{fac}
# #' \item convert all the chain-coded coordinates into (x; y) coordinates
# #' using \link{chc2pix}
# #' (and removes) the "-1" that mark the end of coordinates
# #' \item returns an \code{\link{Out}} object with the corresponding \code{fac}
# #' and with outlines named after the first \code{fac} column
# #' (here with Name_1, Name_2, etc.)
# #' }
# #' This function needs to know how many information (space-separated there is
# #' before) the first coordinate. On the example above, would be 4 id [...] was empty.
# #'
# #' @note Note this function will be deprecated from Momocs
# #' when \code{Momacs} and \code{Momit}
# #' will be fully operationnal.
# #'
# #' @seealso \link{pix2chc}
# #' @references Kuhl, F. P., & Giardina, C. R. (1982).
# #' Elliptic Fourier features of a closed contour.
# #' \emph{Computer Graphics and Image Processing}, 18(3), 236-258.
# #' @examples
# #' \dontrun{
# #' # if the file above was called 'coded.chc' in the 'data' folder:
# #' chc2Out("data/coded.chc", skip=4)
# #' }
# #' @family babel functions
# #' @export
# #' chc2Out <- function(chc, skip, names){
# #'   # read the file and break spaces
# #'   chc <- readLines(chc)
# #'   chc <- strsplit(chc, " ")
# #'   # retrieve the first columns and create a fac
# #'   df <- sapply(chc, function(x) x[1:skip])
# #'   fac <- data.frame(t(df))
# #'   # nice fac names
# #'   if (!missing(names)){
# #'     if (length(names)!=skip) {
# #'       cat(" * names and skip length differ\n")
# #'       names <- paste0("col", 1:skip)
# #'     }
# #'   } else {
# #'     names <- paste0("col", 1:skip)
# #'   }
# #'   colnames(fac) <- names
# #'   # remove these columns from the chc
# #'   chc <- lapply(chc, function(x) x[-(1:skip)])
# #'   # loop over the list: remove the (last) -1, pass it to chc2pix
# #'   coo <- lapply(chc, function(x) chc2pix(as.numeric(x[-length(x)])))
# #'   # prepare and return an Out
# #'   names(coo) <- fac[, 1]
# #'   Out(coo, fac=fac)}
# #'
# #' Imports .nef to Coe objects
# #'
# #' Useful to convert .nef files into Coe objects.
# #' It returns a matrix of coefficients that can be passed to \link{Coe}.
# #' @param nef.path the path to the .nef file
# #' @note nef2Coe cannot really deduces some components of the \code{OutCoe} constructor.
# #' Most of the time working around \code{x <- nef2Coe(); OutCoe(x, method="efourier", norm=TRUE/FALSE)}
# #' shoudl do the job. Overall, I'm not very familiar to other morphometric formats.
# #' So if you have troubles importing your datasets, contact me, I can help. Or if you fix something,
# #' please let met know!
# #'
# #' Note this function will be deprecated from Momocs
# #' when \code{Momacs} and \code{Momit}
# #' will be fully operationnal.
# #'
# #' @family babel functions
# #' @examples
# #' # nef2Coe cannot really deduces some components of the
# #' @export
# nef2Coe <- function(nef.path) {
#   # change nef to coe one day
#   nef <- readLines(nef.path)
#   HARMO.l <- grep(pattern = "HARMO", nef)
#   nb.h <- as.numeric(substring(nef[HARMO.l], 8))
#   nef <- nef[-(1:HARMO.l)]
#   nb.coo <- length(nef)/(nb.h + 1)
#   coo_i <- 1:nb.coo
#   coo_beg <- (coo_i - 1) * (nb.h + 1) + 1
#   coo_end <- coo_beg + nb.h
#   res <- matrix(NA, nrow = nb.coo, ncol = nb.h * 4, dimnames = list(nef[coo_beg],
#                                                                     paste0(rep(LETTERS[1:4], each = nb.h), 1:nb.h)))
#   reorder <- c(1:nb.h * 4 - 3, 1:nb.h * 4 - 2, 1:nb.h * 4 -
#                  1, 1:nb.h * 4)
#   for (i in seq(along = coo_i)) {
#     nef.i <- nef[(coo_beg[i] + 1):coo_end[i]]
#     x <- as.numeric(unlist(strsplit(nef.i, " ")))
#     x <- x[!is.na(x)]
#     res[i, ] <- x[reorder]
#   }
#   return(res)
# }

# deprecated see import_tps
# #' From .tps to Coo objects
# #'
# #' Useful to convert .tps files into Coo objects.
# #' It returns a list of matrices of coordinates that can be passed to \link{Coo} (\link{Out}, \link{Opn} or \link{Ldk}).
# #' @param tps.path the path to the .tps file
# #' @param sep the separator between data
# #' @note I'm not very familiar to other morphometric formats.
# #' So if you have troubles importing your datasets, contact me, I can help. Or if you fix something,
# #' please let met know!
# #' @family babel functions
# #' @export
# tps2Coo <- function(tps.path, sep = " ") {
#   # we read all lines of the file
#   tps <- readLines(tps.path)
#   # we detect the beginning of every individual
#   tps_pos <- cbind(grep(pattern = "lm=", x = tps, ignore.case = TRUE),
#                    c(grep(pattern = "lm=", x = tps, ignore.case = TRUE)[-1] -
#                        1, length(tps)))
#   # we prepare a vector and a list to host the data
#   img.names <- character()
#   coo_list <- list()
#   # and we loop over individuals
#   for (i in 1:nrow(tps_pos)) {
#     # first we pick one of the individuals
#     tps_i <- tps[tps_pos[i, 1]:tps_pos[i, 2]]
#     # and we grab and clean the image name information
#     img.i <- tps_i[grep("image", tps_i, ignore.case = TRUE)]
#     img.i <- gsub("image=", "", img.i, ignore.case = TRUE)
#     img.names[i] <- gsub(".jpg", "", img.i, ignore.case = TRUE)
#     # here we exclude every line that start with a letter
#     coo_i <- tps_i[-grep(pattern = "[[:alpha:]]", tps_i)]
#     # and convert it as a matrix of coordinates
#     coo_i <- unlist(strsplit(coo_i, sep))
#     coo_list[[i]] <- matrix(as.numeric(coo_i), ncol = 2,
#                             byrow = TRUE)
#   }
#   coo_list <- lapply(coo_list, function(x) colnames())
#   names(coo_list) <- img.names
#   return(coo_list)
# }

# #' Imports .nts to Coo objects
# #'
# #' Useful to convert .nts files into \link{Coo} objects. For .nts provided as rows, use
# #' ntsrow2Coo; for .nts provided as columns of coordinates, try ntscol2Coo. It
# #' returns a list of matrices of coordinates that can be passed to \link{Coo} (\link{Out}, \link{Opn} or \link{Ldk}).
# #' @aliases ntscol2Coo ntsrow2Coo
# #' @param nts.path the path to the .nts file
# #' @param sep the separator between data
# #'
# #' @note Note this function will be deprecated from Momocs
# #' when \code{Momacs} and \code{Momit}
# #' will be fully operationnal.
# #'
# #' @examples
# #' # That's how wings dataset was created
# #' # made a local copy from http://life.bio.sunysb.edu/morph/data/RohlfSlice1990Mosq.nts
# #' # then :
# #' # coo_list  <- ntscol2Coo('~/Desktop/mosquitowings.nts)
# #' # fac       <- data.frame(fac=factor(substr(names(coo_list), 1, 2)))
# #' # wings <- Ldk(coo_list, fac=fac)
# #' @family babel functions
# #' @export
# ntsrow2Coo <- function(nts.path, sep = "\t") {
#   # we read all lines and remove the first one
#   nts <- readLines(nts.path, warn = FALSE)
#   comments <- grep(pattern = "\"", nts)
#   nts <- nts[-comments]
#   # we prepare a vector and a list to store the data
#   img.i <- character()
#   coo_list <- list()
#   # we loop over every individual
#   for (i in 1:length(nts)) {
#     # we pick every individual
#     ind.i <- unlist(strsplit(nts[i], sep))
#     # the first element is the name
#     img.i[i] <- ind.i[1]
#     # then we convert the rest as a matrix
#     coo_list[[i]] <- matrix(as.numeric(ind.i[-1]), ncol = 2,
#                             byrow = TRUE)
#   }
#   # we rename list components with image names
#   names(coo_list) <- img.i
#   return(coo_list)
# }
#
# #' @export
# ntscol2Coo <- function(nts.path, sep = "\t") {
#   # candidate for the most ugly function ever?  we read all
#   # lines and remove the skip one
#   nts <- readLines(nts.path, warn = FALSE)
#   comments <- grep(pattern = "\"", nts)
#   nts <- nts[-comments]
#   nb.ldk <- as.numeric(strsplit(nts[1], " ")[[1]][3])
#   nts <- nts[-1]
#   nts <- unlist(strsplit(nts, " "))
#   nts <- nts[-which(nchar(nts) == 0)]
#   nb.nts <- length(nts)/(nb.ldk + 1)
#   # we prepare a vector and a list to store the data
#   names.id <- 1 + (nb.ldk + 1) * (0:(nb.nts - 1))
#   start.id <- names.id + 1
#   end.id <- start.id + nb.ldk - 1
#   img.i <- nts[names.id]
#   coo_list <- list()
#   # we loop over every individual
#   for (i in 1:nb.nts) {
#     coo_list[[i]] <- matrix(as.numeric(nts[start.id[i]:end.id[i]]),
#                             ncol = 2, byrow = TRUE)
#   }
#   # we rename list components with image names
#   names(coo_list) <- img.i
#   return(coo_list)
# }

# bind_db
# TODO: implement a proper join method

# #' Binds with a database
# #'
# #' Adds columns to a \link{Coo} or \link{Coe} object from a data base. Data base must
# #' be provided as a data.frame or as a path which will be \link{read.table}d with \code{...}
# #' arguments.
# #'
# #' Many checks are done on the binding and this is the main advantage of using this method.
# #' It requires an "id" on both the Coo/Coe and the database. There is no assumption
# #' that shapes/coefficients are in the right order in the Coo/Coe
# #' (but a mutate(your_object, id=1:length(your_object)) would do the trick, see examples).
# #'
# #' @param x Coo or Coe object
# #' @param fac_col \code{character} (no numeric here) where to find ids in the fac
# #' @param db \code{data.frame} with the right number of rows, or a path as \code{character}. Then use ... to pass arguments to \link{read.table}
# #' @param db_col \code{character} where to fin ids in db
# #' @param ... more parameters passed to \link{read.table}
# #' @family babel functions
# #' @examples
# #' # Coo example
# #' df <- data.frame(foo_id=40:1, fake1=rnorm(40), fake2=factor(rep(letters[1:4], 10)))
# #' bot <- mutate(bot, hello=1:length(bot))
# #' bind_db(bot, "hello", df, "foo_id")
# #'
# #' # example on a Coe
# #' bf <- efourier(bot, 12)
# #' bind_db(bf, "hello", df, "foo_id")
# #'
# #' @export
# bind_db <- function(x, fac_col="id", db, db_col="id", ...){
#   UseMethod("bind_db")
# }
#
# #' @export
# bind_db.default <- function(x, ...){
#   message("only implemented on Coo and Coe objects")
# }
#'
# #' @export
# bind_db.Coo <- function(x, fac_col="id", db, db_col="id", ...){
#   # checks a bit on x if its a Coo (waiting for verify.Coe todo)
#   if (is_Coo(x))
#     x <- verify(x)
#   .check(!missing(db),
#          "db must be provided")
#   # if db is provided as a path
#   if (is.character(db))
#     db <- utils::read.table(db, ...)
#   # lots of check
#   .check(is.data.frame(db),
#          "db must be a data.frame")
#   .check(nrow(db)==length(x),
#          "nrow(db) does not match")
#   .check(any(colnames(db)==db_col),
#          "db_col not found in db")
#   .check(any(colnames(x$fac)==fac_col),
#          "fac_col not found")
#   x_id <- x$fac[, fac_col]
#   db_id <- db[, db_col]
#   .check(length(unique(x_id))==length(unique(db_id)),
#          "number of unique id must match")
#   map_id <- match(db_id, x_id)
#   .check(all(!is.na(map_id)),
#          "ids mismatch")
#   # finally prepare the db
#   db_lite <- db[map_id, ] # reorders
#   db_lite <- db_lite[,  -which(colnames(db_lite)==db_col)] # -dbcol doesnt work!?
#   # and adds it to fac
#   x$fac <- cbind(x$fac, db_lite)
#   x
# }
#
# #' @export
#' bind_db.Coe <- bind_db.Coo


# helpers ######################################################################

#' Extracts structure from filenames
#'
#' If filenames are consistently named with the same character serating factors,
#' and with every individual including its belonging levels, e.g.:
#' \itemize{
#' \item \code{001_speciesI_siteA_ind1_dorsalview}
#' \item \code{002_speciesI_siteA_ind2_lateralview} } etc., this function returns a \link{data.frame}
#' from it that can be passed to \link{Out}, {Opn}, {Ldk} objects.
#'
#' The number of groups must be consistent across filenames.
#' @param lf a list (its names are used, except if it is a list from \link{import_tps}
#' in this case \code{names(lf$coo)} is used) of a list of filenames, as characters, typically such as
#' those obtained with \link{list.files}. Alternatively, a path to a folder
#' containing the files. Actually, if lf is of length 1 (a single character),
#' the function assumes it is a path and do a \link{list.files} on it.
#' @param names the names of the groups, as a vector of characters which length corresponds
#' to the number of groups.
#' @param split character, the spliting factor used for the file names.
#' @param trim.extension logical. Whether to remove the last for characters in
#' filenames, typically their extension, e.g. '.jpg'.
#' @return data.frame with, for every individual, the corresponding level
#' for every group.
#' @note This is, to my view, a good practice to 'store' the grouping structure
#' in filenames, but it is of course not mandatory.
#'
#' Note also that you can: i) do a \link{import_jpg} and save is a list, say 'foo';
#' then ii) pass 'names(foo)' to lf_structure. See Momocs' vignette for an illustration.
#'
#' @note Note this function will be deprecated from Momocs
#' when \code{Momacs} and \code{Momit}
#' will be fully operationnal.
#'
#' @seealso \link{import_jpg1}, \link{import_Conte}, \link{import_txt}, \link{lf_structure}.
#' See also Momocs' vignettes for data import.
#' @family babel functions
#' @export
lf_structure <- function(lf, names = character(), split = "_",
                         trim.extension = FALSE) {
  # after import_tps case
  if (is.list(lf)) {
    # we handle the import_tps case
    if (identical(names(lf), c("coo", "cur", "scale")))
      lf <- names(lf$coo)
    else
      lf <- names(lf)
  }
  # allow to pass many thing, including $fac columns
  if (!is.character(lf)) lf <- as.character(lf)
  # eg a path
  if (length(lf) == 1) {
    lf <- list.files(lf, full.names = FALSE)
  }
  if (trim.extension) {
    lf <- strtrim(lf, nchar(lf) - 4)
  }
  lf0 <- strsplit(lf, split = split)
  # we check that all files have the same name structure
  lfl <- sapply(lf0, length)
  nc <- unique(lfl)
  ### todo. which ones ?
  if (length(nc) != 1) {
    most.ab <- as.numeric(names(sort(table(lfl), decreasing = TRUE)[1]))
    lfl.pb <- which(lfl != most.ab)
    cat(" * Most of the filenames have", most.ab, "groups.\n",
        " * Maybe you should inspect these file(name)s:\n")
    cat(lf[lfl.pb], sep = "\n")
    stop("the files do not have the same filename structure.")
  }
  fac <- as.data.frame(matrix(NA, nrow = length(lf), ncol = nc))  # dirty
  if (!missing(names)) {
    if (length(names) != nc) {
      stop("the number of 'names' is different from the number of groups.")
    }
    names(fac) <- names
  }
  # no rownames
  rownames(fac) <- NULL
  for (i in 1:nc) {
    # really ugly way to fill the df
    fac[, i] <- factor(sapply(lf0, function(x) x[i]))
  }
  return(fac)
}

#' Binds .jpg outlines from .txt landmarks taken on them
#'
#' Given a list of files (lf) that includes matching filenames with .jpg (black masks)
#' and .txt (landmark positions on them as .txt), returns an Out with $ldk defined.
#' Typically be useful if you use ImageJ to define landmarks on your outlines.
#'
#' @param lf a list of filenames
#' @note Not optimized (images are read twice). Please do not hesitate to contact me
#' should you have a particular case or need something.
#' @family babel functions
#' @export
tie_jpg_txt <- function(lf){

  # check pairs
  mismatches <- lf %>% .trim.both() %>%
    table %>% `!=`(2) %>% which
  if (length(mismatches)>0)
    stop("* mismatches found between .jpg and .txt filenames: ", names(mismatches))

  # we retrieve the list of .txt et.jpg
  out.lf <- lf[grep(".jpg", lf)]
  ldk.lf <- lf[grep(".txt", lf)]

  # we extract outline coordinates
  out.xy <- import_jpg(out.lf)

  # and landmark coordinates
  ldk.xy <- import_txt(ldk.lf)

  # below, we bind together landmarks and outlines but we need to 'invert' y-coordinates

  # here we extract the nb of pixels in y (nrow)
  nrs <- numeric()
  for (i in seq_along(out.lf)){
    nrs[i] <- nrow(jpeg::readJPEG(out.lf[i]))
  }

  # here, the loop that 'inverts'
  res.pos <- list()
  for (i in seq_along(out.xy)){
    out1 <- out.xy[[i]]
    ldk1 <- ldk.xy[[i]]
    ldk1[, 2] <- round(nrs[i] - ldk1[, 2])
    res.pos[[i]] <- edm_nearest(ldk1, out1, TRUE)$pos
  }

  # we now create a Out with all information
  Out <- Out(out.xy, ldk = res.pos)

  # we align it and then save it
  # Out <- coo_slide(Out, ldk=1)
  # Out <- coo_bookstein(Out)
  return(Out)
}

#' Plots a .jpg image
#'
#' A very simple image plotter. If provided with a path,
#' reads the .jpg and plots it. If not provided with an imagematrix, will
#' ask you to choose interactively a \code{.jpeg} image.
#'
#' \code{img_plot} is used in import functions such as \link{import_jpg1};
#' \code{img_plot0} does the same job but preserves the \code{par} and plots axes.
#'
#' @param img a matrix of an image, such as those obtained with \link{readJPEG}.
#' @rdname img_plot
#' @export
img_plot <- function(img) {
  # dirty here but made for convenience to have a fast img
  # plotter..
  if (missing(img)) {
    source <- file.choose()
    img <- jpeg::readJPEG(source)
    cat("img_plot('", source, "')\n", sep = "")
  }
  if (is.character(img)) {
    img <- jpeg::readJPEG(img)
  }
  if (!is.matrix(img)) {
    img <- (img[, , 1] + img[, , 2] + img[, , 3])/3
  }
  op <- par(mar = rep(0.25, 4))
  on.exit(par(op))
  # op <- par(mar=rep(4, 4))
  h <- nrow(img)
  w <- ncol(img)
  plot(NA, xlim = c(1, w), ylim = c(1, h), asp = 1, frame = FALSE,
       axes = FALSE, ann = FALSE)
  # plot(NA, xlim=c(1, w), ylim=c(1, h), asp=1, frame=TRUE,
  # axes=TRUE, ann=TRUE)
  rasterImage(img, 1, 1, w, h, interpolate = FALSE)
  # rect(1, 1, w, h, col=NA, border='red')
  .title(paste(w, h, sep = " x "))
  box()
}

#' @rdname img_plot
#' @export
img_plot0 <- function(img) {
  # dirty here but made for convenience to have a fast img
  # plotter..
  if (missing(img)) {
    source <- file.choose()
    img <- jpeg::readJPEG(source)
    cat("img_plot('", source, "')\n", sep = "")
  }
  if (is.character(img)) {
    img <- jpeg::readJPEG(img)
  }
  if (!is.matrix(img)) {
    img <- (img[, , 1] + img[, , 2] + img[, , 3])/3
  }
  # op <- par(mar=rep(4, 4))
  h <- nrow(img)
  w <- ncol(img)
  img <- img[h:1, ]
  plot(NA, xlim = c(1, w), ylim = c(1, h), asp = 1, frame = TRUE,
       axes = TRUE, ann = TRUE, xlab = "imgmatrix COLs ids",
       ylab = "imgmatrix ROWs ids")
  # plot(NA, xlim=c(1, w), ylim=c(1, h), asp=1, frame=TRUE,
  # axes=TRUE, ann=TRUE)
  rasterImage(img, 1, 1, w, h, interpolate = FALSE)
  rect(1, 1, w, h, col = NA, border = "red")
  .title(paste(w, h, sep = " x "))
  box()
}

# misc ----------------------
# Given a list with individuals containing loci,
# returns a list of loci that contains individuals.
# There must be a more elegant way to do it
.rollup_list <- function(l){
  locus_names <- sapply(l, names) %>% as.character() %>% unique()
  res <- vector("list", length(locus_names))
  names(res) <- locus_names
  for (i in seq_along(l)){
    for (j in seq_along(l[[i]])){
      picked <- names(l[[i]][j])
      res[[picked]] <- append(res[[picked]], l[[i]][j])
      names(res[[picked]])[length(res[[picked]])] <- names(l)[[i]]
    }
  }
  return(res)
}
