library(ggplot2)
library(ggthemes)
library(ggvis)
library(microbenchmark)
library(Momocs)
devtools::load_all()

rm(list=ls())

####
gg_stack <- function(Out, ...){
  Out %>% as_df() %>% {
    ggplot(.) +
      aes(x=x, y=y, group=id) + geom_path(...)
  }
}

x <- bot %>% coo_sample(64) %>% gg_stack(alpha=0.25)

x0 <- bot %>% efourier(6) %>% PCA
x <- x0 %>% as_df() %>% ggplot(.) +
  geom_point() +
  aes(x=PC1, y=PC2, col=type) +
  coord_equal() +
  theme_minimal()
x_built <- ggplot_build(x)
wdw_max <- max(.wdw.gg(x_built))

pos <- x$data %>% select(x=PC1, y=PC2) %>%
      morphospace.pos(pos.shp="range", gg=x)

df_shp <- morphospace2PCA(x0, 1, 2, pos, wdw=wdw_max)

x + geom_path(data=df_shp, aes(x=x, y=y, group=shp1), col="black") +
  coord_cartesian(xlim=c(-wdw_max, wdw_max), ylim=c(-wdw_max, wdw_max))

ggplot(df_shp) + aes(x, y, group=shp1) + geom_path()
if (shapes) {
  gg0 <- gg + geom_point()
  if (center){
    wdw <- max(abs(.x.range.gg(gg0)), abs(.y.range.gg(gg0)))
    gg0 <- gg0 + coord_equal(xlim=c(-wdw, wdw), ylim=c(-wdw, wdw))
  }
  pos <- morphospace.pos(select_(df, x=xax, y=yax),
                         pos.shp=shapes_pos, gg=gg0, ...)
  df_shp <- morphospace2PCA(x, xax, yax, pos, wdw=max(.wdw.gg(gg0)))
  gg <- gg + geom_path(data=df_shp,
                       aes_string(x="x", y="y", group="shp1"), col="black", alpha=0.5)

x + geom_text(aes(label=.id))


Momocs:::.morpho
