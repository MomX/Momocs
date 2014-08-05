# 6. Morphospace functions
# -----------------------------------------------------
# stupid function
#' @export
.mprod <- function(m, s) {
    res <- m
    for (i in 1:ncol(m)) {
        res[, i] <- m[, i] * s[i]
    }
    return(res)
}

#' @export
.morphospacePCA <- function(PCA, xax, yax, pos.shp, nb.shp = 24, 
    nr.shp = 6, nc.shp = 5, amp.shp = 1, size.shp = 15, pts.shp = 60, 
    col.shp = "#00000011", border.shp = "#00000055", lwd.shp = 1) {
    # We retrive the values corresponding to the two plotted axes and the meanshape
    xy <- PCA$x[, c(xax, yax)]
    rot <- PCA$rotation[, c(xax, yax)]
    mshape <- PCA$mshape
    # we define the position of shapes
    pos <- pos.shapes(xy, pos.shp = pos.shp, nb.shp = nb.shp, 
        nr.shp = nr.shp, nc.shp = nc.shp)
    # according to the type of morphometrics applied, we switch the method
    # and the way we plot reconstruct shapes (coo.draw ie polygon, or just lines)
    method <- PCA$method
    
    if (method == "eFourier") {
        shp <- PCA2shp.efourier(pos = pos, rot = rot, mshape = mshape, 
            amp.shp = amp.shp, pts.shp = pts.shp)
        shp <- lapply(shp, coo.close)
        cd <- TRUE
    }
    if (method == "rFourier") {
        shp <- PCA2shp.rfourier(pos = pos, rot = rot, mshape = mshape, 
            amp.shp = amp.shp, pts.shp = pts.shp)
        shp <- lapply(shp, coo.close)
        cd <- TRUE
    }
    if (method == "tFourier") {
        shp <- PCA2shp.tfourier(pos = pos, rot = rot, mshape = mshape, 
            amp.shp = amp.shp, pts.shp = pts.shp)
        shp <- lapply(shp, coo.close)
        cd <- TRUE
    }
    ## open outlines
    if (method == "orthoPolynomials") {
        shp <- PCA2shp.polynomials(pos = pos, rot = rot, mshape = mshape, 
            amp.shp = amp.shp, pts.shp = pts.shp, ortho = TRUE, 
            baseline1 = PCA$baseline1, baseline2 = PCA$baseline2)
        cd <- FALSE
    }
    if (method == "rawPolynomials") {
        shp <- PCA2shp.polynomials(pos = pos, rot = rot, mshape = mshape, 
            amp.shp = amp.shp, pts.shp = pts.shp, ortho = FALSE, 
            baseline1 = PCA$baseline1, baseline2 = PCA$baseline2)
        cd <- FALSE
    }
    if (method == "procrustes") {
        shp <- PCA2shp.procrustes(pos = pos, rot = rot, mshape = mshape, 
            amp.shp = amp.shp)
        cd <- FALSE
    }
    # width <- (par('usr')[4] - par('usr')[3]) * size.shp shp <-
    # lapply(shp, coo.scale, 1/width) not compact enough. #todo
    # #switch ?
    shp <- lapply(shp, coo.template, size = (max(.wdw())/size.shp))
    
    for (i in 1:length(shp)) {
        shp[[i]] <- coo.trans(shp[[i]], pos[i, 1], pos[i, 2])
    }
    if (cd) {
        garbage <- lapply(shp, coo.draw, col = col.shp, border = border.shp, 
            lwd = lwd.shp, points = FALSE, centroid = FALSE, 
            first.point = FALSE)
    } else {
        garbage <- lapply(shp, lines, col = border.shp, lwd = lwd.shp * 
            2)
    }
}

#' @export
.morphospaceLDA <- function(LDA, xax, yax, pos.shp, nb.shp = 24, 
    nr.shp = 6, nc.shp = 5, amp.shp = 1, size.shp = 15, pts.shp = 60, 
    col.shp = "#00000011", border.shp = "#00000055") {
    
    xy <- LDA$mod.pred$x[, c(xax, yax)]
    rot <- LDA$LDs[, c(xax, yax)]
    mshape <- LDA$mshape
    
    # we fill any removed variables with 0s
    r <- LDA$removed
    if (length(r) > 0) {
        m2 <- matrix(rep(0, length(r) * 2), nrow = length(r), 
            byrow = TRUE, dimnames = list(names(r), colnames(rot)))
        m3 <- rbind(rot, m2)
        rot <- m3[match(names(mshape), rownames(m3)), ]
    }
    
    # we define the position of shapes
    pos <- pos.shapes(xy, pos.shp = pos.shp, nb.shp = nb.shp, 
        nr.shp = nr.shp, nc.shp = nc.shp)
    # according to the type of morphometrics applied, we
    # reconstruct shapes
    method <- LDA$method
    ## outlines
    if (method == "eFourier") {
        shp <- lda2shp.efourier(pos = pos, rot = rot, mshape = mshape, 
            amp.shp = amp.shp, pts.shp = pts.shp)
        cd <- TRUE
    }
    # if (method=='rFourier'){ shp <- PCA2shp.rfourier(pos=pos,
    # rot=rot, mshape=mshape, amp.shp=amp.shp, pts.shp=pts.shp)
    # cd <- TRUE} if (method=='tFourier'){ shp <-
    # PCA2shp.tfourier(pos=pos, rot=rot, mshape=mshape,
    # amp.shp=amp.shp, pts.shp=pts.shp) cd <- TRUE} ## open
    # outlines if (method=='orthoPolynomials'){ shp <-
    # PCA2shp.polynomials(pos=pos, rot=rot, mshape=mshape,
    # amp.shp=amp.shp, pts.shp=pts.shp, ortho=TRUE,
    # baseline1=PCA$baseline1, baseline2=PCA$baseline2) cd <-
    # FALSE} if (method=='rawPolynomials'){ shp <-
    # PCA2shp.polynomials(pos=pos, rot=rot, mshape=mshape,
    # amp.shp=amp.shp, pts.shp=pts.shp, ortho=FALSE,
    # baseline1=PCA$baseline1, baseline2=PCA$baseline2) cd <-
    # FALSE} if (method=='procrustes'){ shp <-
    # PCA2shp.procrustes(pos=pos, rot=rot, mshape=mshape,
    # amp.shp=amp.shp) cd <- FALSE} width <- (par('usr')[4] -
    # par('usr')[3]) * size.shp shp <- lapply(shp, coo.scale,
    # 1/width) not enough compact. #todo
    shp <- lapply(shp, coo.template, size = (max(.wdw())/size.shp))
    shp <- lapply(shp, coo.close)
    for (i in 1:length(shp)) {
        shp[[i]] <- coo.trans(shp[[i]], pos[i, 1], pos[i, 2])
    }
    if (cd) {
        garbage <- lapply(shp, coo.draw, col = col.shp, border = border.shp, 
            points = FALSE, centroid = FALSE, first.point = TRUE)
    } else {
        garbage <- lapply(shp, lines, col = border.shp)
    }
}

#' Calculates nice positions on a plan for drawing shapes
#' 
#' @param xy todo
#' @param pos.shp the way shape should be positionned
#' @param nb.shp the total number of shapes
#' @param nr.shp the number of rows to position shapes
#' @param nc.shp the number of cols to position shapes
#' @param circle.r.shp if circle, its radius
#' @keywords Graphics
#' @export
pos.shapes <- function(xy, pos.shp = c("range", "circle", "xy")[1], 
    nb.shp = 12, nr.shp = 6, nc.shp = 5, circle.r.shp) {
    if (is.data.frame(pos.shp) | is.matrix(pos.shp)) {
        return(as.matrix(pos.shp))
    }
    if (pos.shp == "xy") {
        return(xy)
    }
    if (pos.shp == "circle") {
        if (missing(circle.r.shp)) {
            # mean distance from origin
            circle.r.shp <- mean(apply(xy, 1, function(x) sqrt(sum(x^2))))
        }
        t <- seq(0, 2 * pi, len = nb.shp + 1)[-(nb.shp + 1)]
        pos <- cbind(circle.r.shp * cos(t), circle.r.shp * sin(t))
        colnames(pos) <- c("x", "y")  # pure cosmetics
        return(pos)
    }
    if (pos.shp == "range") {
        pos <- expand.grid(seq(min(xy[, 1]), max(xy[, 1]), len = nr.shp), 
            seq(min(xy[, 2]), max(xy[, 2]), len = nc.shp))
        pos <- as.matrix(pos)
        colnames(pos) <- c("x", "y")  # pure cosmetics
        return(pos)
    }
    if (pos.shp == "full") {
        # w <- par('usr') pos <- expand.grid(seq(w[1], w[2],
        # len=nr.shp), seq(w[3], w[4], len=nc.shp))
        w <- par("usr")
        pos <- expand.grid(seq(w[1] * 0.85, w[2] * 0.85, len = nr.shp), 
            seq(w[3] * 0.85, w[4] * 0.85, len = nc.shp))
        pos <- as.matrix(pos)
        colnames(pos) <- c("x", "y")  # pure cosmetics
        return(pos)
    }
    # if a non-valid method is passed
    return(xy)
}

#' Calculates shapes from PC plane: e/r/tfourier
#' 
#' @param pos the position on two PC axis
#' @param rot the corresponding loadings
#' @param mshape the meanshape
#' @param amp.shp amplification factor for the shape deformation
#' @param pts.shp number of points to reconstruct the shape
#' @keywords Graphics
#' @rdname PCA2shp.fourier
#' @export
PCA2shp.efourier <- function(pos, rot, mshape, amp.shp = 1, pts.shp = 60) {
    if (ncol(pos) != ncol(rot)) 
        stop("'rot' and 'pos' must have the same ncol")
    if (length(mshape) != nrow(rot)) 
        stop("'mshape' and ncol(rot) lengths differ")
    nb.h <- length(mshape)/4
    n <- nrow(pos)
    # we prepare the array
    res <- list()
    for (i in 1:n) {
        ax.contrib <- .mprod(rot, pos[i, ]) * amp.shp
        coe <- mshape + apply(ax.contrib, 1, sum)
        xf <- coeff.split(coe)
        coo <- efourier.i(xf, nb.h = nb.h, nb.pts = pts.shp)
        # reconstructed shapes are translated on their centroid if
        # (trans) {
        dx <- pos[i, 1] - coo.centpos(coo)[1]
        dy <- pos[i, 2] - coo.centpos(coo)[2]
        coo <- coo.trans(coo, dx, dy)
        # }
        res[[i]] <- coo
    }
    return(res)
}

#' @rdname PCA2shp.fourier
#' @export
PCA2shp.rfourier <- function(pos, rot, mshape, amp.shp = 1, pts.shp = 60) {
    if (ncol(pos) != ncol(rot)) 
        stop("'rot' and 'pos' must have the same ncol")
    if (length(mshape) != nrow(rot)) 
        stop("'mshape' and ncol(rot) lengths differ")
    nb.h <- length(mshape)/2
    n <- nrow(pos)
    # we prepare the array
    res <- list()
    for (i in 1:n) {
        ax.contrib <- .mprod(rot, pos[i, ]) * amp.shp
        coe <- mshape + apply(ax.contrib, 1, sum)
        xf <- coeff.split(coe, cph = 2)
        coo <- rfourier.i(xf, nb.h = nb.h, nb.pts = pts.shp)
        # reconstructed shapes are translated on their centroid if
        # (trans) {
        dx <- pos[i, 1] - coo.centpos(coo)[1]
        dy <- pos[i, 2] - coo.centpos(coo)[2]
        coo <- coo.trans(coo, dx, dy)
        # }
        res[[i]] <- coo
    }
    return(res)
}

#' @rdname PCA2shp.fourier
#' @export
PCA2shp.tfourier <- function(pos, rot, mshape, amp.shp = 1, pts.shp = 60) {
    if (ncol(pos) != ncol(rot)) 
        stop("'rot' and 'pos' must have the same ncol")
    if (length(mshape) != nrow(rot)) 
        stop("'mshape' and ncol(rot) lengths differ")
    nb.h <- length(mshape)/2
    n <- nrow(pos)
    # we prepare the array
    res <- list()
    for (i in 1:n) {
        ax.contrib <- .mprod(rot, pos[i, ]) * amp.shp
        coe <- mshape + apply(ax.contrib, 1, sum)
        xf <- coeff.split(coe, 2)
        coo <- tfourier.i(xf, nb.h = nb.h, nb.pts = pts.shp, 
            force2close = TRUE)
        # reconstructed shapes are translated on their centroid if
        # (trans) {
        dx <- pos[i, 1] - coo.centpos(coo)[1]
        dy <- pos[i, 2] - coo.centpos(coo)[2]
        coo <- coo.trans(coo, dx, dy)
        # }
        res[[i]] <- coo
    }
    return(res)
}

#' Calculates shapes from PC plane: polynomials
#' 
#' @param pos the position on two PC axis
#' @param rot the corresponding loadings
#' @param mshape the meanshape
#' @param amp.shp amplification factor for the shape deformation
#' @param pts.shp number of points to reconstruct the shape
#' @param ortho logical whether working with raw or orthogonal polynomials
#' @param baseline1 the (x; y) coordinates of the first baseline point
#' @param baseline2 the (x; y) coordinates of the second baseline point
#' @keywords Graphics
#' @export
PCA2shp.polynomials <- function(pos, rot, mshape, amp.shp = 1, 
    pts.shp = 60, ortho, baseline1, baseline2) {
    if (ncol(pos) != ncol(rot)) 
        stop("'rot' and 'pos' must have the same ncol")
    if (length(mshape) != nrow(rot)) 
        stop("'mshape' and ncol(rot) lengths differ")
    degree <- length(mshape)
    n <- nrow(pos)
    # an empy pol object
    pol <- list(coeff = rep(NA, degree), ortho = ortho, baseline1 = baseline1, 
        baseline2 = baseline2)
    # we prepare the array
    res <- list()
    for (i in 1:n) {
        ax.contrib <- .mprod(rot, pos[i, ]) * amp.shp
        pol$coeff <- mshape + apply(ax.contrib, 1, sum)
        coo <- polynomials.i(pol, nb.pts = pts.shp, reregister = TRUE)
        pol$coeff <- rep(NA, degree)
        # reconstructed shapes are translated on their centroid if
        # (trans) {
        dx <- pos[i, 1] - coo.centpos(coo)[1]
        dy <- pos[i, 2] - coo.centpos(coo)[2]
        coo <- coo.trans(coo, dx, dy)
        res[[i]] <- coo
    }
    # }
    return(res)
}

#' Calculates shapes from PC plane: (aligned) landmarks
#' 
#' @param pos the position on two PC axis
#' @param rot the corresponding loadings
#' @param mshape the meanshape
#' @param amp.shp amplification factor for the shape deformation
#' @keywords Graphics
#' @export
PCA2shp.procrustes <- function(pos, rot, mshape, amp.shp = 1) {
    if (ncol(pos) != ncol(rot)) 
        stop("'rot' and 'pos' must have the same ncol")
    if (length(mshape) != nrow(rot)) 
        stop("'mshape' and ncol(rot) lengths differ")
    n <- nrow(pos)
    # we prepare the array
    res <- list()
    for (i in 1:n) {
        ax.contrib <- .mprod(rot, pos[i, ]) * amp.shp
        shape.i <- mshape + apply(ax.contrib, 1, sum)
        coo <- matrix(shape.i, ncol = 2, byrow = FALSE)
        # reconstructed shapes are translated on their centroid if
        # (trans) {
        dx <- pos[i, 1] - coo.centpos(coo)[1]
        dy <- pos[i, 2] - coo.centpos(coo)[2]
        coo <- coo.trans(coo, dx, dy)
        res[[i]] <- coo
    }
    # }
    return(res)
}

# todo r/t
#' Calculates shapes from LD plane: e/r/tfourier
#' 
#' @param pos the position on two PC axis
#' @param rot the (unstardized) loadings
#' @param mshape the meanshape
#' @param amp.shp amplification factor for the shape deformation
#' @param pts.shp number of points to reconstruct the shape
#' @keywords Graphics
#' @rdname lda2shp.fourier
#' @export
lda2shp.efourier <- function(pos, rot, mshape, amp.shp = 1, pts.shp = 60) {
    if (ncol(pos) != ncol(rot)) 
        stop(" * 'rot' and 'pos' must have the same ncol")
    if (length(mshape) != nrow(rot)) 
        stop(" * 'mshape' and ncol(rot) lengths differ")
    nb.h <- length(mshape)/4
    n <- nrow(pos)
    # we prepare the array
    res <- list()
    for (i in 1:n) {
        ax.contrib <- .mprod(rot, pos[i, ]) * amp.shp
        coe <- mshape + apply(ax.contrib, 1, sum)
        xf <- coeff.split(coe)
        coo <- efourier.i(xf, nb.h = nb.h, nb.pts = pts.shp)
        # reconstructed shapes are translated on their centroid if
        # (trans) {
        dx <- pos[i, 1] - coo.centpos(coo)[1]
        dy <- pos[i, 2] - coo.centpos(coo)[2]
        coo <- coo.trans(coo, dx, dy)
        # }
        res[[i]] <- coo
    }
    return(res)
}

#' @keywords Graphics
#' @rdname lda2shp.fourier
#' @export
lda2shp.rfourier <- function() {
}

#' @keywords Graphics
#' @rdname lda2shp.fourier
#' @export
lda2shp.tfourier <- function() {
}

##### end morphospaces 
