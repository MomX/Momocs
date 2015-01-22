
theme_empty <- function (base_size = 12, base_family = "", ...){
  modifyList(theme_light(
    base_size = base_size,
    base_family = base_family),
    list (
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank(),
      strip.background=element_rect(fill="grey95"),
      strip.text=element_text(colour = "grey10"),
      strip.text.x=element_text(colour = "grey10"),
      strip.text.y=element_text(colour = "grey10")
      ))}

