rm(list=ls())
library(fslr)
options(fsl.path='/usr/local/fsl/')


ptpat <- "\\d\\d\\d-(\\d|)\\d\\d\\d"

username <- Sys.info()["user"][[1]]

rootdir <- path.expand(file.path("~/CT_Registration/Final_Brain_Seg"))
basedir = file.path(rootdir, "Original_Images")
outdir = file.path(rootdir, "results")

dirs <- list.dirs(basedir, full.names=TRUE, recursive=FALSE)
dirs <- dirs[!grepl("(Images|programs)$", dirs)]
dirs = dirs[grepl("144-4220", dirs, fixed=TRUE)]
idir = dirs[1]
f.img <- dir(pattern=".*.nii.gz", path=idir, 
              recursive=FALSE, full.names=TRUE)
f.ssimg = file.path(dirname(f.img), "Skull_Stripped", 
                  paste0(nii.stub(f.img, bn=TRUE), "_SS_0.01.nii.gz"))

img = readNIfTI(f.img, reorient=FALSE)
ssimg = readNIfTI(f.ssimg, reorient=FALSE)

pngname = file.path(outdir, "Original_Image.png")
png(pngname, res=600, height=7, width=7, units = "in")
ortho2(img, window=c(0, 100), text = "Original Image")
dev.off()


thresh =  fslthresh(img, thresh = 0, uthresh = 100, 
                    retimg = TRUE)

pngname = file.path(outdir, "Thresh_Image.png")
png(pngname, res=600, height=7, width=7, units = "in")
  ortho2(thresh, window=c(0, 100), text = "Thresholded Image")
dev.off()

smooth = fslsmooth(thresh, retimg=TRUE, sigma = 1)

pngname = file.path(outdir, "Smooth_Thresh_Image.png")
png(pngname, res=600, height=7, width=7, units = "in")
  ortho2(smooth, window=c(0, 100), text = "Smoothed\nThresholded Image")
dev.off()

pngname = file.path(outdir, "SS_Image.png")
png(pngname, res=600, height=7, width=7, units = "in")
  ortho2(ssimg, window=c(0, 100), text = "Brain-Extracted\n Image")
dev.off()
