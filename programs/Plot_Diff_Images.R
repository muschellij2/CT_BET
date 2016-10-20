rm(list=ls())
library(cttools)
library(fslr)
library(oro.dicom)
library(bitops)
library(arules)
library(scales)
options(matlab.path='/Applications/MATLAB_R2014b.app/bin')

# username <- Sys.info()["user"][[1]]
rootdir = path.expand("~/Dropbox/CTR/DHanley/CT_Registration")

ROIformat = FALSE
study = "Original_Images"
if (ROIformat) {
  study = "ROI_images"
}

homedir <- file.path(rootdir, "Final_Brain_Seg", study)
#basedir <- file.path("/Volumes/CT_Data/MISTIE")

setwd(homedir)

ids = list.dirs(homedir, recursive=FALSE, full.names=FALSE)
ids = basename(ids)
ids = grep("\\d\\d\\d-(\\d|)\\d\\d\\d", ids, value=TRUE)
length(ids)

iid = 1

sdir = file.path(rootdir, "Final_Brain_Seg", "Original_Images")

imgs = list.files(sdir, pattern = "\\.nii\\.gz", recursive=TRUE,
    full.names=TRUE)
imgs = imgs[ !grepl("dcm2nii", imgs)]
imgs = imgs[ !grepl("Skull_Stripped", imgs)]

img.df = data.frame(img=imgs, stringsAsFactors=FALSE)
img.df$id = basename(img.df$img)
ss = strsplit(img.df$id, "_")
img.df$id = sapply(ss, function(x) {
  paste(x[1:4], collapse="_", sep="")
})

roidir = file.path(rootdir, "Final_Brain_Seg", "ROI_Images")

rimgs = list.files(roidir, pattern = "\\.nii\\.gz", recursive=TRUE,
    full.names=TRUE)
rimgs = rimgs[ !grepl("dcm2nii", rimgs)]
rimgs = rimgs[ !grepl("Skull_Stripped", rimgs)]

rimg.df = data.frame(rimg=rimgs, stringsAsFactors=FALSE)
rimg.df$id = basename(rimg.df$rimg)
ss = strsplit(rimg.df$id, "_")
rimg.df$id = sapply(ss, function(x) {
  paste(x[1:4], collapse="_", sep="")
})

df = merge(rimg.df, img.df, all.x=TRUE)

ss.imgs = list.files(sdir, pattern = "\\.nii\\.gz", recursive=TRUE,
    full.names=TRUE)
ss.imgs = ss.imgs[ grepl("Skull_Stripped", ss.imgs)]
ss.imgs = ss.imgs[ grepl("[M|m]ask", ss.imgs)]

ss.df = data.frame(ssimg=ss.imgs, stringsAsFactors=FALSE)
ss.df$id = basename(ss.df$ssimg)
ss = strsplit(ss.df$id, "_")
ss.df$id = sapply(ss, function(x) {
  paste(x[1:4], collapse="_", sep="")
})


ssdf = merge(ss.df, df, all.x=TRUE)

irow = 1

  mid.folder = function(x, folname = "", level=1){
    d = x
    b = basename(x)
    for (ilev in seq(level)) d = dirname(d)
    file.path(d, folname, b)
  }

pngnames = NULL


splitdf = split(ssdf, ssdf$img)

for (irow in seq_along(splitdf)) {
  
  rundf = splitdf[[irow]]
  img.f = rundf$img[1]
  roi.f = rundf$rimg[1]
  id = rundf$id[1]

  img = readNIfTI(img.f, reorient=FALSE)
  img = window_img(img, window=c(0, 100), replace= "window")
  roi = readNIfTI(roi.f, reorient=FALSE) > 0
  iimg = 1

  # mask.overlay(img, roi, 
  # col = gray(0:64/64, alpha = 0.5),
  #   col.y = alpha("red", 0.5), 
  #   text ="Manual",
  #   text.cex = 1)  

  newimg = img
  newimg[ roi == 1] = 105
  newimg = cal_img(newimg)

  ########################################
  # Red - overlay ROI
  ########################################
  pngname = paste0(nii.stub(roi.f), ".png")
  pngname = mid.folder(pngname, folname = "overlay")
  folname = dirname(pngname)
  if (!file.exists(folname)){
    dir.create(folname)
  }
  if (!file.exists(pngname)){
    png(pngname, res=600, 
      height=7, width=7, units = "in",
      type="cairo")    
        image(newimg, col = c(gray(0:64/64), 
          "red", "red"),
          z = floor(dim(img)[2]/2), 
          plane = "sagittal",
          plot.type="single"
        )
    dev.off()
  }

  print(irow)


  for (iimg in seq(nrow(rundf))){
      
      ss.f = rundf$ssimg[iimg]
      ssimg = readNIfTI(ss.f, reorient=FALSE)


      pngname = nii.stub(ss.f)
      text = basename(pngname)
      text = gsub(".*_SS_(.*)", "\\1", text)
      pngname = mid.folder(pngname, "overlay", level=2)
      pngstub = pngname
      pngname = paste0(pngname, "_Diff.png")

      folname = dirname(pngname)
      if (!file.exists(folname)){
        dir.create(folname)
      }
      int = gsub(".*(0.01|0.35|0.1).*", "\\1", text)
      smooth = c("Unsmoothed", "Smoothed")[
        grepl("nopresmooth", ss.f) + 1
        ]


      if (!file.exists(pngname)){
        dimg = niftiarr(roi, (roi - ssimg))
        png(pngname, res=600, 
          height=7, width=7, units = "in",
          type="cairo")
          image(dimg, 
            col = c("blue", "blue", "black", "red", "red"),
            z = floor(dim(img)[2]/2), 
            zlim=c(-1, 1),
            plane = "sagittal",
            plot.type="single",
          )
 
        dev.off()
      }

      # mask.overlay(img, ssimg, 
      # col = gray(0:64/64, alpha = 0.5),
      #   col.y = alpha("blue", 0.5), 
      #   text = paste0("BET\n",
      #     "FI: ", int, "\n",
      #     smooth, "\nData"),
      #   text.cex = 3)  

      pngname = paste0(pngstub, ".png")
      newimg = img
      newimg[ ssimg == 1] = 105
      newimg = cal_img(newimg)

      if (!file.exists(pngname)){
        png(pngname, res=600, 
          height=7, width=7, units = "in",
          type="cairo")    
            image(newimg, col = c(gray(0:64/64), 
              "blue", "blue"),
              z = floor(dim(img)[2]/2), 
              plane = "sagittal",
              plot.type="single"
            )        
        dev.off()
      }
      print(iimg)
  }

}


# newimg = img
          # newimg[ dimg == -1] = -5
          # newimg[ dimg == 2] = 105
          # newimg = cal_img(newimg)
          # image(newimg, col = c("blue", "blue", gray(0:64/64), 
          #   "red", "red"),
          #   z = floor(dim(img)[2]/2), 
          #   plane = "sagittal",
          #   plot.type="single"
          # )


          # dimg[dimg == 1] = 2

          # uvals = sort(unique(c(dimg)))
          # cols = c("blue", "black", "red")

          # runcol = cols[  c(-1, 0, 2) %in% uvals]          
          # dimg[dimg == 0] = NA

          # orthographic(dimg, col = runcol, text.cex = 1, 
          #   zlim=c(-.99, 1.01))    