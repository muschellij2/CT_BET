rm(list=ls())
library(cttools)
library(fslr)
library(oro.dicom)
library(bitops)
library(arules)
options(matlab.path='/Applications/MATLAB_R2013b.app/bin')

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


ssdf = merge(ss.df, img.df, all.x=TRUE)

irow = 1

  mid.folder = function(x, folname = "", level=1){
    d = x
    b = basename(x)
    for (ilev in seq(level)) d = dirname(d)
    file.path(d, folname, b)
  }


# for (irow in seq(nrow(df))) {
  
#   rimg.f = df$rimg[irow]
#   img.f = df$img[irow]
#   id = df$id[irow]

#   img = readNIfTI(img.f, reorient=FALSE)
#   rimg = readNIfTI(rimg.f, reorient=FALSE)

#   pngname = nii.stub(img.f)
#   pngname = paste0(pngname, "ROI.png")
#   pngname = mid.folder(pngname, "overlay")

#   dir.create(dirname(pngname))

#   png(pngname, res=600, height=7, width=7, units = "in")
#     mask.overlay(img, rimg, text= paste0("ROI for\n", id),
#       text.cex = 1)
#   dev.off()
#   print(irow)
# }

for (irow in seq(nrow(ssdf))) {
  
  ss.f = ssdf$ssimg[irow]
  img.f = ssdf$img[irow]
  id = ssdf$id[irow]

  img = readNIfTI(img.f, reorient=FALSE)
  ssimg = readNIfTI(ss.f, reorient=FALSE)

  pngname = nii.stub(ss.f)
  text = basename(pngname)
  text = gsub(".*_SS_(.*)", "\\1", text)
  pngname = paste0(pngname, ".png")
  pngname = mid.folder(pngname, "overlay", level=2)

  dir.create(dirname(pngname))

  png(pngname, res=600, height=7, width=7, units = "in")
    mask.overlay(img, ssimg, text= paste0("SS ", text, "\n", id),
      text.cex = 1)
  dev.off()
  print(irow)
}


x = list.files(homedir, pattern=".png", full.names=TRUE, recursive=TRUE)
x = x[grep("overlay", x)]
library(smallPDF)
p = paste(x, collapse=" ")
view.png(p, viewer="open")
# sapply(x, view.png, viewer = "open")