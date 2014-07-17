# Functions and utilities to import data in Momocs,
# particularly from raw images,

#' Import coordinates from a .txt file
#' 
#' A wrapper around \link{read.table} that can be used to import outline/landmark coordinates.
#' 
#' By default, it works with the default arguments of \link{read.table}, e.g. assumes that the
#' columns are not named in the \code{.txt} files. You can tune this using the \code{...} argument.
#' Define the \link{read.table} arguments that allow to import a single file, and then
#' pass them to this function.
#' @param txt.paths a vector of paths corresponding to the .txt files to import. If not 
#' provided (or \code{NULL}), switches to the automatic version, just as in\link{import.jpg}.
#' See Details there.
#' @param ... arguments to be passed to \link{read.table}, eg. 'skip', 'dec', etc.
#' @return a list of matrix(ces) of (x; y) coordinates that can be passed to
#' \link{Out}, \link{Opn} and \link{Ldk}.
#' @seealso \link{import.jpg1}, \link{import.Conte}, \link{import.txt}, \link{lf.structure}.
#' See also Momocs' vignettes for data import.
#' @keywords Import
#' @export
import.txt <- function(txt.paths = NULL, ...) {
    if (is.null(txt.paths)) {
        txt.paths <- .lf.auto()
    }
    cat(" * Extracting", length(txt.paths), "..txt coordinates...\n")
    if (length(txt.paths) > 10) {
        pb <- txtProgressBar(1, length(txt.paths))
        t <- TRUE
    } else {
        t <- FALSE
    }
    res <- list()
    for (i in seq(along = txt.paths)) {
        coo <- read.table(txt.paths[i], ...)
        res[[i]] <- as.matrix(coo)
        if (t) 
            setTxtProgressBar(pb, i)
    }
    # names(res) <- substr(txt.paths, start=1,
    # stop=nchar(txt.paths)-4)
    names(res) <- .trim.ext(txt.paths)
    return(res)
}

#' Extract outlines coordinates from an image silhouette
#' 
#' Provided with an image 'mask' (i.e. black pixels on a white background),
#' and a point form where to start the algorithm, returns the (x; y) coordinates of its outline.
#' 
#' Used internally by \link{import.jpg1} but may be useful for other purposes.
#' @param img a matrix of a binary image mask.
#' @param x numeric the (x; y) coordinates of a starting point within the shape.
#' @return a matrix the (x; y) coordinates of the outline points.
#' @references
#' \itemize{
#' \item The original algorithm is due to: Pavlidis, T. (1982). \emph{Algorithms 
#' for graphics and image processing}. Computer science press.
#' \item is detailed in: Rohlf, F. J. (1990). An overview of image processing and 
#' analysis techniques for morphometrics. In \emph{Proceedings of the Michigan Morphometrics Workshop}. Special Publication No. 2 (pp. 47-60). University of Michigan Museum of Zoology: Ann Arbor.
#' \item and translated in R by: Claude, J. (2008). \emph{Morphometrics with R}. (p. 316). Springer.
#' }
#' @seealso \link{import.jpg1}, \link{import.Conte}, \link{import.txt}, \link{lf.structure}.
#' See also Momocs' vignettes for data import.
#' @keywords Import
#' @export
import.Conte <- function(img, x) {
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
#' single images and is wrapped by \link{import.jpg}. It relies itself on \link{import.Conte}
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
#' @details jpegs can be provided either as RVB or as 8-bit greylevels or monochrome.
#' The function binarizes pixels values using the 'threshold' argument. It will try to start to
#' apply the \link{import.Conte} algorithm from the center of
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
#' border of white pixels; otherwise \link{import.Conte} would fail and return an error.
#' 
#' Finally, remember that if the images are not in your working directory,
#' \link{list.files} must be called with the argument \code{full.names=TRUE}!
#' 
#' Note that the use of the \code{fun.notcentered} argument will probably leads to serious headaches
#' and will probably imply the dissection of these functions: \link{import.Conte}, \link{img.plot} and
#' \code{import.jpg} itself
#' @seealso \link{import.jpg}, \link{import.Conte}, \link{import.txt}, \link{lf.structure}.
#' See also Momocs' vignettes for data import.
#' @keywords Import
#' @return a matrix of (x; y) coordinates that can be passed to Out
#' @export
import.jpg1 <- function(jpg.path, auto.notcentered = TRUE, fun.notcentered = NULL, 
    threshold = 0.5) {
    img <- readJPEG(jpg.path)
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
            img.plot(img)
            while (img[x[1], x[2]] != 0) {
                cat(" * Click a point within the shape\n")
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
    # import.Conte
    out <- import.Conte(img, x)
    return(out)
}

#' Extract outline coordinates from multiple .jpg files
#' 
#' This function is used to import outline coordinates and is built around 
#' \link{import.jpg1}.
#' @param jpg.paths a vector of paths corresponding to the .jpg files to import. If not 
#' provided (or \code{NULL}), switches to the automatic version. See Details below.
#' @param auto.notcentered logical if TRUE random locations will be used until.
#' one of them is (assumed) to be within the shape (because of a black pixel);
#' if FALSE a \link{locator} will be called, and you will have to click on a 
#' point within the shape.
#' @param fun.notcentered NULL by default. Is your shapes are not centered and if a random pick of
#' a black pixel is not satisfactory. See \link{import.jpg1} help and examples.
#' @param threshold the threshold value use to binarize the images. Above, pixels
#' are turned to 1, below to 0.
#' @param verbose whether to print which file is being treated. Useful to detect problems.
#' @details see \link{import.jpg1} for important informations about how the outlines are extracted, 
#' and \link{import.Conte} for the algorithm itself.
#' 
#' If \code{jpg.paths} is not provided (or \code{NULL}), you will have to select any \code{.jpg}
#' file in the folder taht contains all your files. All the outlines should be imported then.
#' @keywords Import
#' @seealso \link{import.jpg1}, \link{import.Conte}, \link{import.txt}, \link{lf.structure}.
#' See also Momocs' vignettes for data import.
#' @return a list of matrices of (x; y) coordinates that can be passed to \link{Out}
#' @examples
#' \dontrun{
#' 
#' # if your images are in the folder '/foo/jpgs/'
#' lf <- list.files('/foo/jpegs', full.names=TRUE)
#' coo <- import.jpg(lf)
#' Out(coo)
#' 
#' # 'automatic' version
#' coo <- import.jpg()
#' }
#' @export
import.jpg <- function(jpg.paths = NULL, auto.notcentered = TRUE, 
    fun.notcentered = NULL, threshold = 0.5, verbose = TRUE) {
    # if not provided
    if (is.null(jpg.paths)) {
        jpg.paths <- .lf.auto()
    }
    cat(" * Extracting", length(jpg.paths), ".jpg outlines...\n")
    if (length(jpg.paths) > 10) {
        pb <- txtProgressBar(1, length(jpg.paths))
        t <- TRUE
    } else {
        t <- FALSE
    }
    # for a futurer safer import jpg.names <-
    # .trim.path(.trim.ext(jpgs.paths))
    res <- list()
    for (i in seq(along = jpg.paths)) {
        coo.i <- import.jpg1(jpg.paths[i], auto.notcentered = auto.notcentered, 
            fun.notcentered = fun.notcentered, threshold = threshold)
        res[[i]] <- coo.i
        # if (export){coo.export(coo.i, jpg.paths[i])}
        if (verbose) {
            cat(jpg.paths[i], "\n")
        } else {
            if (t) 
                setTxtProgressBar(pb, i)
        }
    }
    names(res) <- .trim.path(jpg.paths)
    cat(" * Done.")
    return(res)
}

#' Plots a .jpg image
#' 
#' A very simple image plotter. If provided with a path,
#' read the .jpg and plots it. If not provided with an imagematrix, will
#' ask you to choose interactively a \code{.jpeg} image.
#' 
#' \code{img.plot} is used in import functions such as \link{import.jpg1};
#' \code{img.plot0} does the same job but preserves the \code{par} and plots axes.
#' 
#' @param img a matrix of an image, such as those obtained with \link{readJPEG}.
#' @keywords Import
#' @rdname img.plot
#' @export
img.plot <- function(img) {
    # dirty here but made for convenience to have a fast img
    # plotter..
    if (missing(img)) {
        source <- file.choose()
        img <- readJPEG(source)
        cat("img.plot('", source, "')\n", sep = "")
    }
    if (is.character(img)) {
        img <- readJPEG(img)
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

#' @rdname img.plot
#' @export
img.plot0 <- function(img) {
    # dirty here but made for convenience to have a fast img
    # plotter..
    if (missing(img)) {
        source <- file.choose()
        img <- readJPEG(source)
        cat("img.plot('", source, "')\n", sep = "")
    }
    if (is.character(img)) {
        img <- readJPEG(img)
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

#' Extract structure from filenames
#' 
#' If filenames are consistently named with the same character serating factors,
#' and with every individual including its belonging levels, e.g.: 
#' \itemize{
#' \item \code{001_speciesI_siteA_ind1_dorsalview}
#' \item \code{002_speciesI_siteA_ind2_lateralview} } etc., this function returns a \link{data.frame}
#' from it that can be passed to \link{Out}, {Opn}, {Ldk} objects.
#' 
#' The number of groups must be consistent accross filenames.
#' @param lf list of filenames, as characters, typically such as
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
#' @note This is, to my view, a good practice to 'store' the grouoing structure
#' in filenames, but it is of course not mandatory.
#' 
#' Note also that you can: i) do a \link{import.jpg} and save is a list, say 'foo';
#' then ii) pass 'names(foo)' to lf.structure. See Momocs' vignette for an illustration.
#' @seealso \link{import.jpg1}, \link{import.Conte}, \link{import.txt}, \link{lf.structure}.
#' See also Momocs' vignettes for data import.
#' @keywords Import
#' @export
lf.structure <- function(lf, names = character(), split = "_", 
    trim.extension = FALSE) {
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
        stop("The files do not have the same filename structure.")
    }
    fac <- as.data.frame(matrix(NA, nrow = length(lf), ncol = nc))  # dirty
    if (!missing(names)) {
        if (length(names) != nc) {
            stop(" * The number of 'names' is different from the number of groups.")
        }
        names(fac) <- names
    }
    # nice rownames
    rownames(fac) <- lf
    for (i in 1:nc) {
        # really ugly way to fill the df
        fac[, i] <- factor(sapply(lf0, function(x) x[i]))
    }
    return(fac)
}

# Fridge -------

# splines <- function(coo, method='natural', deriv=2){ coo <-
# coo.check(coo) z <- coo.perim.cum(coo) fx <- splinefun(z,
# coo[, 1], method=method) fy <- splinefun(z, coo[, 2],
# method=method) xcoe <- fy(z, deriv=2) ycoe <- fy(z,
# deriv=2) return(list(xcoe=xcoe, ycoe=ycoe))} splines2 <-
# function(coo, nb.pts=100){ z <- coo.perim.cum(coo) x.i <-
# spline(z, coo[, 1], method='natural', n=100)$y y.i <-
# spline(z, coo[, 2], method='natural', n=100)$y
# return(cbind(x.i, y.i))} click.bez <- function(x, n=10){ x
# <- as.raster(x) plot(NA, xlim=c(1, dim(x)[1]), ylim=c(1,
# dim(x)[2]), asp=1) grid.raster(x) ldk <- matrix(NA, n, 2)
# bez <- NA ldk[1, ] <- l2m(locator(1)) for (i in 2:n){
# grid.raster(x) lines(bez, col='red') ldk[i, ] <-
# l2m(locator(1)) cat(ldk) bez <-
# bezier.i(bezier(ldk[1:i,])$B) }} click.splines <-
# function(x, n=20){ x <- as.raster(x) plot(NA, xlim=c(1,
# dim(x)[1]), ylim=c(1, dim(x)[2]), asp=1) grid.raster(x) ldk
# <- matrix(NA, n, 2) spl <- NA ldk[1, ] <- l2m(locator(1))
# for (i in 2:n){ grid.raster(x) points(ldk[1:i,], pch=20,
# col='black') lines(spl, col='red') ldk[i, ] <-
# l2m(locator(1)) cat(ldk) spl <- splines2(ldk[1:i,]) }} 
