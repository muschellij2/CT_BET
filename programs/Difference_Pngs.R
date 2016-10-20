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

union_dir = file.path(basedir, "Union_ROI_Images")
int_dir = file.path(basedir, "Intersection_ROI_Images")
diff_dir = file.path(basedir, "Difference_ROI_Images")

df$union = file.path(union_dir, df$pid, 
                     paste0(nii.stub(df$am_img, bn=TRUE), "_Union"))
df$int = file.path(int_dir, df$pid, 
                   paste0(nii.stub(df$am_img, bn=TRUE), "_Intersection"))

df$diff = file.path(diff_dir, df$pid, 
                    paste0(nii.stub(df$am_img, bn=TRUE), "_Difference"))

#sapply(file.path(union_dir, df$pid), dir.create, recursive = TRUE)
#sapply(file.path(int_dir, df$pid), dir.create, recursive = TRUE)
# sapply(file.path(diff_dir, df$pid), dir.create, recursive = TRUE)

splitdf = split(df, df$nat_img)
iimg = 1


for (iimg in seq_along(splitdf)){
  
  dd = splitdf[[iimg]]
  d = dd$diff[1]
  diff = readNIfTI(d, reorient = FALSE)
  pngname = paste0(d, ".png")
  diff[ diff == -1 ] = 2  
  crap = niftiarr(diff, array(0, dim=dim(diff)))
  png(pngname)
    mask.overlay(crap, diff, 
                 ybreaks=c(0, 1, 2),   zlim.y = c(0,2),
                 col.y = c("blue", "red"))
  dev.off()
  print(iimg)
}
