library(Momocs)
shapes[4] %>%
  coo_dxy() %>%
  dplyr::mutate(id=1:n()) %>%
  tidyr::gather(key = "what", value="value", dx, dy) %>%
  ggplot() +
  aes(x=id, y=value, col=what) +
  geom_line() +
  theme_minimal() +
  ylab("") + xlab("Points along outlines") +
  guides(colour = guide_legend(""))

coo_oscillo



coo_oscillo2 <- function(coo, method=c("dxy", "radius", "tangent")[1]){
  .check(any(method %in% c("dxy", "radius", "tangent")),
         "unvalid method")
  if (method=="radius"){
    value=coo_centdist(coo)
    df <- dplyr::data_frame(value=value,
                            id=seq_along(value),
                            what="centroid_dist")
  }
  if (method=="tangent"){
    value=coo_angle_tangent(coo)
    df <- dplyr::data_frame(value=value,
                            id=seq_along(value),
                            what="tangent_angle")
  }
  if (method=="dxy"){
    tmp <- coo_dxy(coo)
    df <- dplyr::data_frame(value=c(tmp$dx, tmp$dy),
                            id=rep(seq_along(tmp$dx), 2),
                            what=rep(c("dx", "dy"), each=nrow(tmp)))
  }

  # now the gg
  df %>% ggplot() +
    aes(x=id, y=value, col=what) +
    geom_line() +
    theme_minimal() +
    xlab("Points alongs ids") +
    ylab("") +
    guides(colour=guide_legend(""))
}
