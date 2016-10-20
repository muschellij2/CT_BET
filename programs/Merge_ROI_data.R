rm(list=ls())
library(cttools)
library(fslr)
library(oro.dicom)
library(bitops)
library(arules)
library(extrantsr)
library(plyr)
options(matlab.path='/Applications/MATLAB_R2014b.app/bin')
options(fsl.path='/usr/local/fsl')

# username <- Sys.info()["user"][[1]]
rootdir = path.expand("~/Dropbox/CTR/DHanley/CT_Registration")
basedir = file.path(rootdir, "Final_Brain_Seg")
am_dir <- file.path(basedir, "AM_ROI_images")
nat_dir <- file.path(basedir, "ROI_images")

resdir = file.path(basedir, "results")

ids = list.dirs(nat_dir, recursive=FALSE, full.names=FALSE)
ids = basename(ids)
ids = grep("\\d\\d\\d-(\\d|)\\d\\d\\d", ids, value=TRUE)
length(ids)

am_iddirs = file.path(am_dir, ids)
nat_iddirs = file.path(nat_dir, ids)
iid = 1

######################
# Getting Andrew's scans
######################
niis = sapply(ids, function(x) {
  iddir = file.path(am_dir, x)
  x = list.files(path=  iddir, pattern=".nii.gz", 
                 recursive = FALSE, full.names = TRUE)
})
keep.ids = ids[sapply(niis, length) > 0]
imgs = unlist(niis)
img.df = data.frame(am_img=imgs, stringsAsFactors=FALSE)
img.df$id = basename(img.df$am_img)
ss = strsplit(img.df$id, "_")
img.df$id = sapply(ss, function(x) {
  paste(x[1:4], collapse="_", sep="")
})

ximg.df = img.df

######################
# Getting natalie's scans
######################
niis = sapply(ids, function(x) {
  iddir = file.path(nat_dir, x)
  x = list.files(path=  iddir, pattern=".nii.gz", 
                 recursive = FALSE, full.names = TRUE)
})
keep.ids = ids[sapply(niis, length) > 0]
imgs = unlist(niis)
img.df = data.frame(nat_img=imgs, stringsAsFactors=FALSE)
img.df$id = basename(img.df$nat_img)
ss = strsplit(img.df$id, "_")
img.df$id = sapply(ss, function(x) {
  paste(x[1:4], collapse="_", sep="")
})


all.df = merge(ximg.df, img.df, by="id", all=TRUE)

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

img.df = merge(all.df, img.df, by="id", all=TRUE)
cc = complete.cases(img.df)

##########
# Make sure all andrew's are in there
#########
stopifnot(sum(cc) == nrow(ximg.df))

df = img.df[cc,]


df$hdr = file.path(dirname(df$img), "Sorted", 
                   paste0(nii.stub(df$img, bn=TRUE), "_Header_Info.Rda"))
df$pid = sapply(strsplit(df$id, "_"), `[`, 1)


######################
# Keeping only one scan per person
######################
df = ddply(df, .(pid), function(x){
  x = x[ x$id == x$id[1], ]
  x
})

n.pid = length(unique(df$pid))
n.img = length(unique(df$id))

stopifnot(n.pid == n.img)


splitdf = split(df, df$nat_img)
iimg = 1


for (iimg in seq_along(splitdf)){
  
  dd = splitdf[[iimg]]
  rimg = dd$nat_img[1]
  roi = readNIfTI(rimg, reorient=FALSE)
  roi = roi > 0
  
  hdr = dd$hdr[1]
  load(hdr)
  
  vol.roi = get_roi_vol(img = roi, dcmtables = dcmtables)
  varslice = vol.roi$varslice
  vol.roi = vol.roi$truevol
  
  results = laply(dd$am_img, function(x){
    ss = readNIfTI(x, reorient=FALSE)
    ss = ss > 0
    truevol = get_roi_vol(img = ss, dcmtables = dcmtables)$truevol
    dman = c(roi)
    dauto = c(ss)
    dim.dman = dim(roi)
    dim.dauto = dim(ss)
    
    res = extrantsr:::sim(dman, dauto)	
    res$am_vol = truevol
    return(res)
  }, .progress = "text")
  
  dd = cbind(dd, results)
  dd$nat_vol = vol.roi
  
  splitdf[[iimg]] = dd
  print(iimg)
}

ddf = do.call("rbind", splitdf)
rownames(ddf) = NULL

save(ddf, file=file.path(resdir, "AM_NU_Overlap_Statistics.Rda"))
