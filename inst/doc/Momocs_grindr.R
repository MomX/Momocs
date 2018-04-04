## ---- echo=FALSE---------------------------------------------------------
library(knitr)
opts_chunk$set(eval = TRUE, message=FALSE,
               warnings=FALSE, results="hold")

## ---- echo=FALSE---------------------------------------------------------
library(Momocs)
devtools::load_all()

## ------------------------------------------------------------------------
bot[1] %>% paper # single shape
olea %>% paper   # Coo (here Opn) object

## ------------------------------------------------------------------------
bot[1] %>% paper %>% draw_outline
olea %>% paper %>% draw_curve

## ------------------------------------------------------------------------
bot %>% paper_chess %>% draw_outline -> x
x

## ------------------------------------------------------------------------
apropos("paper")

## ------------------------------------------------------------------------
bot %>% paper_grid %>%
    draw_outline(~type, bor=col_qual) %>%
    draw_ticks %>% draw_centroid %>% draw_firstpoint

## ------------------------------------------------------------------------
paper(bot) %>% draw_outlines(~type, bor=col_qual)
paper(bot) %>%
    draw_outlines(factor(rep(1:5, 8)), bor=col_qual) %>%
    draw_centroid(~type, pch=c(1, 3))

## ------------------------------------------------------------------------
x <- bot %>% efourier(6) %>% PCA
plot_PCA(x, ~type) %>%
    layer_fullframe %>%
    layer_morphospace %>%
    layer_points %>% layer_chull %>%
    layer_eigen %>% layer_legend

