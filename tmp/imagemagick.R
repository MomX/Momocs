# https://github.com/ropensci/magick#readme
#  brew install imagemagick --with-fontconfig --with-librsvg --with-fftw
install.packages("magick")
library(magick)
library(magrittr)
frink <- image_read("https://jeroenooms.github.io/images/frink.png")
image_trim(frink)

######

devtools::install_github("timelyportfolio/imageR")

library(imageR)

tf <- tempfile()
png( file = tf, height = 400, width = 600 )
plot(1:50)
dev.off()

intense(base64::img(tf))

####

install.packages("htmltools", "curl", "navr", "sortableR", "imageR")

library(htmltools)
library(curl)
library(navr)
library(sortableR)
library(imageR)

n1 <- navr(
  selector = "#sortableR-toolbar"
  ,taglist = tagList(
    tags$ul(id = "sort-navr"
            ,style="line-height:120px; text-align:center; vertical-align:middle;"
            ,tags$li(
              style="border: solid 0.1em white;border-radius:100%;line-height:inherit;width:130px;height:130px;"
              , class="fa fa-binoculars fa-4x"
              #  attribution everywhere Creative Commons Flickr
              #  awesome picture by https://www.flickr.com/photos/12859033@N00/2288766662/
              , "data-image" = paste0(
                "data:image/jpg;base64,"
                ,base64enc::base64encode(
                  curl("https://farm4.staticflickr.com/3133/2288766662_c40c168b76_o.jpg","rb")
                )
              )
              , "data-title" = "Binoculars, a working collection"
              , "data-caption" = "awesome picture courtesy Flickr Creative Commons
              <a href = 'https://www.flickr.com/photos/12859033@N00/2288766662/'>jlcwalker</a>"
            )
            ,tags$li(
              style="border: solid 0.1em white;border-radius:100%;line-height:inherit;width:130px;height:130px;"
              , class="fa fa-camera fa-4x"
              #  attribution everywhere Creative Commons Flickr
              #  awesome picture by https://www.flickr.com/photos/s58y/5607717791
              , "data-image" = paste0(
                "data:image/jpg;base64,"
                ,base64enc::base64encode(
                  curl("https://farm6.staticflickr.com/5309/5607717791_b030229247_o.jpg","rb")
                )
              )
              , "data-title" = "Leica IIIc converted to IIIf BD ST"
              , "data-caption" = "awesome picture courtesy Flickr Creative Commons
            <a href = 'https://www.flickr.com/photos/s58y/5607717791'>s58y</a>"
            )
            )
)
)

html_print(tagList(
  tags$div(
    id = "sortableR-toolbar"
    ,style="width:300px;border: dashed 0.2em lightgray; float:left;"
    ,tags$h3("sortableR Icons for Intense Images")
    ,"These icons drag and drop. Click on them for an"
    ,tags$strong("intense")
    ,"result."
  )
  ,add_font_awesome(n1)
  ,sortableR("sort-navr")
  ,intense( selector = "#sort-navr li" )
))
