morphospace.pos <-
  function(xy,
           pos.shp=c("axes", "circle", "confell", "full", "range", "xy")[4],
           nb.shp = 12, nr.shp = 6, nc.shp = 5,
           r.shp = 1, conf.shp = 0.5,
           gg){
    # if a matrix or a data.frame is passed
    if (!is.character(pos.shp)) return(pos.shp)

    # shapes along axes
    if (pos.shp == "axes") {
      xr <- .x.range.gg(gg) * 0.9
      yr <- .y.range.gg(gg) * 0.9
      dfx <- data.frame(x=seq(xr[1], xr[2], length=nc.shp), y=0)
      dfy <- data.frame(x=0, y=seq(yr[1], yr[2], length=nr.shp))
      pos <- bind_rows(dfx, dfy) %>% unique()
      return(pos)
    }

    # shapes arranged on a circle
    if (pos.shp == "circle") {
      if (missing(r.shp)) {
        circle.r.shp <- coo_centsize(xy)
      }
      t <- seq(0, 2 * pi, len = nb.shp + 1)[-(nb.shp + 1)]
      pos <- data.frame(x=circle.r.shp * cos(t),
                        y=circle.r.shp * sin(t))
      return(pos)}

    # shapes arranged on a confidence ellipse
    if (pos.shp == "confell"){
      pos <- m2d(conf_ell(x=xy[, 1], y=xy[, 2],
                          conf = conf.shp, nb.pts = nb.shp)$ell)
      return(pos)}
    # shapes covering the entire graph
    if (pos.shp == "full") {
      # because of coord_equal in plot.PCA
      w <- max(.wdw.gg(gg)) * 0.45
      pos <- expand.grid(seq(-w, w, len = nr.shp),
                         seq(-w, w, len = nc.shp))
      colnames(pos) <- c("x", "y")  # pure cosmetics
      return(pos)}
    # shapes covering the range of data
    if (pos.shp == "range") {
      pos <- expand.grid(seq(min(xy[, 1]), max(xy[, 1]), len = nr.shp),
                         seq(min(xy[, 2]), max(xy[, 2]), len = nc.shp))
      colnames(pos) <- c("x", "y")  # pure cosmetics
      return(pos)
    }
    # if anything else is passed
    return(xy)
  }


morphospace2PCA <- function(PCA, xax, yax, pos,
                            size.shp=1, amp.shp=1,
                            wdw=0.1){
  pts.shp = 60
  # should be avoided
  pos0 <- pos
  pos <- as.matrix(pos)
  # we check here, though it shoudl have been before
  if (length(PCA$method)>4 | is.null(PCA$method)) {
    stop("morphospacePCA needs a $method of length <= 5")}
  # we retrive the values corresponding to the two plotted axes and the meanshape
  xy  <- PCA$x[, c(xax, yax)]
  rot <- PCA$rotation[, c(xax, yax)]
  mshape <- PCA$mshape
  # we define the position of shapes
  # according to the type of morphometrics applied, we switch the method
  # and the way we plot reconstruct shapes (polygon, lines, points for Out, Opn, Ldk)
  # when the object combines different morphometric approaches (up to 4)
  # their size is divided by 2 and the shapes and set (of d) around the (x; y) coordinates of pos.shp
  method <- PCA$method
  nb.met <- length(method)
  if (length(size.shp)!=nb.met) size.shp <- rep(size.shp[1], nb.met)
  size.shp.final <- (size.shp*wdw/14) / ifelse(nb.met<2, 1, 2)
  d <- mean(size.shp.final) / 2
  # here we define the translation x and y for every sub-morphoshape
  # and the coe to retrieve
  if (nb.met==1){
    dx <- 0
    dy <- 0}
  if (nb.met==2){ #met1 over met2 - h center
    dx <- c(0, 0)
    dy <- c(d, -d)}
  if (nb.met==3){ #podium arrangement
    dx <- c(0, -d, d)
    dy <- c(d, -d, -d)}
  if (nb.met==4){ #form top left, clockwise
    dx <- c(-d, d, -d, d)
    dy <- c(d, d, -d, -d)}
  # indices of succesive coe to select
  if (nb.met==1){
    col.start <- 1
    col.end   <- length(mshape)
  } else {
    col.start <- cumsum(PCA$cuts) - PCA$cuts + 1
    col.end   <- cumsum(PCA$cuts)}
  # not very satisfactory...
  # hack in case of multi
  SHP <- list()
  for (i in seq(along=method)){
    shp <- NULL
    ids <- col.start[i]:col.end[i]
    # outlines
    # efourier
    if (method[i] == "efourier") {
      shp <- PCA2shp_efourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids],
                              amp.shp = amp.shp, pts.shp = pts.shp)}
    # rfourier
    if (method[i] == "rfourier") {
      shp <- PCA2shp_rfourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids],
                              amp.shp = amp.shp, pts.shp = pts.shp)}
    # tfourier
    if (method[i] == "tfourier") {
      shp <- PCA2shp_tfourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids],
                              amp.shp = amp.shp, pts.shp = pts.shp)}
    #dfourier
    if (method[i] == "dfourier") {
      shp <- PCA2shp_dfourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids],
                              amp.shp = amp.shp, pts.shp = pts.shp)}
    # opoly
    if (method[i] == "opoly") {
      shp <- PCA2shp_polynomials(pos = pos, rot = rot[ids, ], mshape = mshape[ids],
                                 amp.shp = amp.shp, pts.shp = pts.shp, ortho = TRUE,
                                 baseline1 = PCA$baseline1, baseline2 = PCA$baseline2)}
    # npoly
    if (method[i] == "npoly") {
      shp <- PCA2shp_polynomials(pos = pos, rot = rot[ids, ], mshape = mshape[ids],
                                 amp.shp = amp.shp, pts.shp = pts.shp, ortho = FALSE,
                                 baseline1 = PCA$baseline1, baseline2 = PCA$baseline2)}
    ### configuration of landmarks
    if (method[i] == "procrustes") {
      shp <- PCA2shp_procrustes(pos = pos, rot = rot[ids, ],
                                mshape = mshape[ids],
                                amp.shp = amp.shp)}
    ### Then...
    # we template and center shapes
    shp <- lapply(shp,
                  function(x) x %>%
                    coo_template(size = size.shp.final[i]) %>%
                    coo_center() %>%
                    coo_close() %>%
                    coo_sample(pts.shp))
    # thus pts.shp must be +1
#     pts.shp <- pts.shp + 1
    SHP <- c(SHP, shp)
  }
# return(SHP)
  #### loop end
  # we bind the list and prepare for sth gg-friendly
  # centered positions
  df_shp <- do.call(rbind, SHP) %>% as.data.frame()
  colnames(df_shp) <- c("x_c", "y_c")
  # shape (for combined) + mini translation
  k <- nrow(pos0) * pts.shp
  df_shp <- mutate(df_shp,
                   SHP = rep(1:nb.met, each=k),
                   shp = rep(rep(1:nrow(pos0), each=pts.shp), times=nb.met),
                   shp1 = rep(1:(nrow(pos0)*nb.met), each=pts.shp),
                   x_d = rep(dx, each=k),
                   y_d = rep(dy, each=k))
  # translation (ie the original positions on the plane)
  i <- 1:nrow(pos0)
  i_n <- rep(i, each=pts.shp)
  df_trans <- select(pos0, x_t=x, y_t=y) %>% dplyr::slice(rep(i_n, nb.met))
  # we bind together and apply the translation
  df <- bind_cols(df_shp, df_trans) %>%
    # we add the two translations
    mutate_(x = quote(x_c + x_t + x_d), y = quote(y_c + y_t + y_d))
  # %>%
    # and rearrange the columsn (pure cosmetics)
    #select(SHP, shp, shp1, x, y,
    #        x_c, y_c, x_t, y_t, x_d, y_d)
  return(df)
}



