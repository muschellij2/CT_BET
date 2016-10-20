rm(list=ls())
library(cttools)
library(fslr)
library(oro.dicom)
library(bitops)
library(arules)
options(matlab.path='/Applications/MATLAB_R2014b.app/bin')
options(fsl.path='/usr/local/fsl/')

# username <- Sys.info()["user"][[1]]
rootdir = path.expand("~/CT_Registration/Final_Brain_Seg/Skull_Strip_Paper/")
homedir <- file.path(rootdir)
#basedir <- file.path("/Volumes/CT_Data/MISTIE")

setwd(homedir)

ids = list.dirs(homedir, recursive=FALSE, full.names=FALSE)
ids = basename(ids)
ids = grep("\\d\\d\\d-(\\d|)\\d\\d\\d", ids, value=TRUE)
length(ids)

iid = 1

sdir = file.path(rootdir)

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

ssdf$AddText = NA
ssdf$AddText[seq(1, nrow(ssdf), by=2)] = LETTERS[1:3]
ssdf$AddText[seq(2, nrow(ssdf), by=2)] = LETTERS[4:6]

for (irow in seq(nrow(ssdf))) {
  
  ss.f = ssdf$ssimg[irow]
  img.f = ssdf$img[irow]
  id = ssdf$id[irow]

  
  pngname = nii.stub(ss.f)
  text = basename(pngname)
  text = gsub(".*_SS_(.*)", "\\1", text)
  text = gsub("_Mask", "", text)
  int = sapply(strsplit(text, "_"), function(x) x[1])
  smooth = sapply(strsplit(text, "_"), function(x) x[2])
  
  if (is.na(smooth)){
    smooth = "Smoothed"
  }
  if (smooth == "nopresmooth"){
    smooth = "Unsmoothed"
  }
  int = paste0("FI = ", int)
  text = paste0(ssdf$AddText[irow], "\n", int, "\n", smooth)
  
  img = readNIfTI(img.f, reorient=FALSE)
  ssimg = readNIfTI(ss.f, reorient=FALSE)

  
  pngname = paste0(pngname, ".png")
  pngname = file.path(rootdir, "figure", basename(pngname))

  dn = dirname(pngname)
  if (!file.exists(dn)) {
    dir.create(dn)
  }

  png(pngname, res=600, height=7, width=7, units = "in")
    mask.overlay(img, ssimg, text= text,
      text.cex = 3)
  dev.off()
  print(irow)
}


# x = list.files(homedir, pattern=".png", full.names=TRUE, recursive=TRUE)
# x = x[grep("overlay", x)]
# library(smallPDF)
# p = paste(x, collapse=" ")
# view.png(p, viewer="open")
# sapply(x, view.png, viewer = "open")