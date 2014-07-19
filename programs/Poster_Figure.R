rm(list=ls())
library(oro.dicom)
library(bitops)
library(arules)
library(ggplot2)
ptpat <- "\\d\\d\\d-(\\d|)\\d\\d\\d"

username <- Sys.info()["user"][[1]]

basedir <- file.path("/Users", username, "Dropbox/CTR/DHanley/CT_Registration/Brain_Seg_Rerun")
setwd(basedir)
dirs <- list.dirs(basedir, full.names=TRUE, recursive=FALSE)
dirs <- dirs[!grepl("(Images|programs)", dirs)]
rois <- imgs <- NULL
for (idir in dirs){
  im <- dir(pattern=".*.nii.gz", path=idir, recursive=FALSE, full.names=TRUE)
  rois <- c(rois, im[grepl("ROI", im)])
  imgs <- c(imgs, im[!grepl("ROI", im)]) 
}
check <- sapply(strsplit(rois, "_"), function(x) {
  x <- x[x!= "ROI"]
  paste(x, collapse="_")
})

imgs <- imgs[imgs %in% check]
imgs <- imgs[!grepl(imgs, pattern="205-509", fixed=TRUE)]

# imgs <- imgs[1]
# icutoff <- 0.1
cutoffs <- c(0.1, 0.35, 0.7)
ss <- NULL
ss <- sapply(cutoffs, function(icutoff) gsub(imgs, pattern="\\.nii\\.gz", replacement = paste0("_SS_No1024_", icutoff, "\\.nii\\.gz")))
dmat <- dim(ss)
ss <- file.path(dirname(ss), "Skull_Stripped", basename(ss))
ss <- matrix(ss, nrow=dmat[1], ncol=dmat[2])

### Took out the wrong ones with 205-509
imgs <- c(imgs, ss)

iimg <- 39
for (iimg in 1:length(imgs)){
	img <- imgs[iimg]
	imgdir <- file.path(gsub(paste0("(.*", ptpat, "/).*"), "\\1", img), "pngs")
	if (!file.exists(imgdir)) dir.create(imgdir)
	pngname <- gsub(".nii.gz", ".png", basename(img), fixed=TRUE)
	pngname <- file.path(imgdir, pngname)
	manual <- readNIfTI(img, reorient=FALSE)
	png(pngname)
		orthographic(manual)
	dev.off()
	print(iimg)
}

