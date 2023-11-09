context("ffmpeg functions")

v <- system.file("extdata/vid", "sunfish_BCF.avi", package = "trackter")
file.copy(v,tempdir())
dir.create(paste0(tempdir(),"/test"))

try(vid.to.images(vid.path = paste0(tempdir(),"/sunfish_BCF.avi"),out.dir =paste0(tempdir(),"/test")))

ff <- length(list.files(paste0(tempdir(),"/test")))==0

test_that("vid.to.images works", {
  
  skip_if(ff)
  
  v <- system.file("extdata/vid", "sunfish_BCF.avi", package = "trackter")
  file.copy(v,tempdir())
  dir.create(paste0(tempdir(),"/test_images"))
  
  vid.to.images(vid.path = paste0(tempdir(),"/sunfish_BCF.avi"),out.dir =paste0(tempdir(),"/test_images"))
  
  expect_true(length(list.files(paste0(tempdir(),"/test_images")))==2)
  expect_error(vid.to.images(vid.path = paste0(tempdir(),"/sunfish_BCF.avi"),out.dir = NULL),"'out.dir' not specified")
  expect_error(vid.to.images(vid.path = paste0(tempdir(),"/sunfish_BCF.avi"),out.dir =paste0(tempdir(),"/foo") ),"Directory specified by 'out.dir'")
  expect_error(vid.to.images(vid.path = paste0(tempdir(),"/foo.avi"),out.dir=paste0(tempdir(),"/test_images")),"Path specified by 'vid.path'")

  unlink(paste0(tempdir(),"/test_images"),recursive = TRUE)
  unlink(paste0(tempdir(),"/sunfish_BCF.avi"))
})

test_that("images.to.video works", {
  skip_if(ff)
  
  if(dir.exists(paste0(tempdir(),"/sunfish"))) unlink(paste0(tempdir(),"/sunfish"),recursive = TRUE)
  
  dir.create(paste0(tempdir(),"/sunfish"))
  v <- system.file("extdata/img", "sunfish_BCF.jpg", package = "trackter")
  
  file.copy(v,paste0(tempdir(),"/sunfish/img_001.jpg"))
  file.copy(v,paste0(tempdir(),"/sunfish/img_002.jpg"))
  
  images.to.video(image.dir = paste0(tempdir(),"/sunfish"),vid.name = "test.mp4",out.dir=tempdir(),silent = TRUE)
  
  
  
  expect_true(file.exists(paste0(tempdir(),"/test.mp4")))
  expect_true(file.size(paste0(tempdir(),"/test.mp4"))>10)
  
  
  expect_error(images.to.video(image.dir = paste0(tempdir(),"/sunfish"),out.dir = NULL),"'out.dir' not specified")
  
  expect_error(images.to.video(image.dir = paste0(tempdir(),"/sunfish"),out.dir = "/foo"),"Directory specified by 'out.dir'")
  
  expect_error(images.to.video(image.dir = paste0(tempdir(),"/foo"),out.dir = tempdir()),"Directory specified by 'image.dir'")
  
  expect_error(images.to.video(image.dir = paste0(tempdir(),"/sunfish"),vid.name = "test.mp4",out.dir=tempdir(),silent = TRUE,overwrite = FALSE),"'overwrite' must be 'TRUE'")
  
  unlink(paste0(tempdir(),"/test.mp4"))
  
  unlink(paste0(tempdir(),"/sunfish"),recursive = TRUE)
  
 })


test_that("vid.to.images2 works", {
  
  skip_if(ff)
  
  v <- system.file("extdata/vid", "sunfish_BCF.avi", package = "trackter")
  dir.create(paste0(tempdir(),"/test_images"))
  file.copy(v,tempdir())
  
  vid.to.images2(vid.path = paste0(tempdir(),"/sunfish_BCF.avi"),out.dir =paste0(tempdir(),"/test_images"))
  
  expect_true(length(list.files(paste0(tempdir(),"/test_images")))==2)
  
  expect_error(vid.to.images2(vid.path = paste0(tempdir(),"/sunfish_BCF.avi"),out.dir = NULL),"'out.dir' not specified")
  expect_error(vid.to.images2(vid.path = paste0(tempdir(),"/sunfish_BCF.avi"),out.dir = paste0(tempdir(),"/foo")),"Directory specified by 'out.dir'")
  img1 <- EBImage::readImage(paste0(tempdir(),"/test_images/",list.files(paste0(tempdir(),"/test_images"))[1]))
  
  expect_error(vid.to.images2(vid.path = NULL,out.dir =paste0(tempdir(),"/test_images") ),"vid.path' not specified")
  expect_error(vid.to.images2(vid.path = paste0(tempdir(),"/foo.avi"),out.dir =paste0(tempdir(),"/test_images")),"Path specified by 'vid.path'")
  
  vid.to.images2(vid.path = paste0(tempdir(),"/sunfish_BCF.avi"),filt = " -vf scale=200:-1 ",out.dir =paste0(tempdir(),"/test_images")) 
  img2 <- EBImage::readImage(paste0(tempdir(),"/test_images/",list.files(paste0(tempdir(),"/test_images"))[1]))
  expect_true(dim(img1)[1]>dim(img2)[1]) #images with scaling filter are smaller
  
  
  unlink(paste0(tempdir(),"/test_images"),recursive = TRUE)
  unlink(paste0(tempdir(),"/sunfish_BCF.avi"))
  
})

test_that("images.to.video2 works", {
  skip_if(ff)
 
  if(dir.exists(paste0(tempdir(),"/sunfish"))) unlink(paste0(tempdir(),"/sunfish"),recursive = TRUE)
  
  dir.create(paste0(tempdir(),"/sunfish"))
  v <- system.file("extdata/img", "sunfish_BCF.jpg", package = "trackter")
  
  file.copy(v,paste0(tempdir(),"/sunfish/img_001.jpg"))
  file.copy(v,paste0(tempdir(),"/sunfish/img_002.jpg"))
  
  images.to.video2(image.dir = paste0(tempdir(),"/sunfish"),vid.name ="test", out.dir=tempdir(),silent = TRUE,raw = TRUE)

  
  expect_true(file.exists(paste0(tempdir(),"/test.avi")))
  expect_true(file.size(paste0(tempdir(),"/test.avi"))>10)
  
  f.s1 <- file.size(paste0(tempdir(),"/test.avi"))
  images.to.video2(image.dir = paste0(tempdir(),"/sunfish"),vid.name = "test",out.dir=tempdir(),silent = TRUE,raw = FALSE,overwrite = TRUE)
  f.s2 <- file.size(paste0(tempdir(),"/test_red.mp4"))
  
  expect_true(f.s1>f.s2) #is compressed file <raw file
  
  images.to.video2(image.dir = paste0(tempdir(),"/sunfish"),vid.name = "test",out.dir=tempdir(),silent = TRUE,raw = FALSE,filt = " -s 120x80 ",overwrite = TRUE) #reduce scale
  f.s3 <- file.size(paste0(tempdir(),"/test_red.mp4"))
  
  expect_true(f.s1>f.s3)
  expect_true(f.s2>f.s3)
  
  expect_message(images.to.video2(paste0(tempdir(),"/sunfish"),vid.name = "test",out.dir=tempdir(),silent = TRUE,raw = TRUE,overwrite = TRUE),"video saved to")
  
  expect_error(images.to.video2(image.dir = paste0(tempdir(),"/sunfish"),out.dir = NULL),"'out.dir' not specified")
  
  expect_error(images.to.video2(image.dir = paste0(tempdir(),"/sunfish"),out.dir = "/foo"),"Directory specified by 'out.dir'")
  
  expect_error(images.to.video2(image.dir = paste0(tempdir(),"/foo"),out.dir = tempdir()),"Directory specified by 'image.dir'")
  
  expect_error(images.to.video2(image.dir = paste0(tempdir(),"/sunfish"),vid.name = "test",out.dir=tempdir(),silent = TRUE,overwrite = FALSE),"'overwrite' must be 'TRUE'")
                   
  unlink(paste0(tempdir(),"/test.avi"))
  unlink(paste0(tempdir(),"/test.mp4"))
  unlink(paste0(tempdir(),"/test_red.mp4"))
  unlink(paste0(tempdir(),"/sunfish"),recursive = TRUE)
  
  
})
