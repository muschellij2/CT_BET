rm(list=ls())
library(oro.dicom)
library(oro.nifti)
library(bitops)
library(arules)
library(ggplot2)
library(stringr)
library(reshape2)

basedir <- file.path("~/Dropbox/CTR/DHanley/CT_Registration/Final_Brain_Seg/")
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


cutoffs <- c(0.1, .35, 0.7)
adders = c(1, 1024, "")
fnames = c("Human", "SS_Add", "SS_Add_Mask", "Raw", "Raw_Mask", "SS", "SS_Mask")
scenarios = expand.grid(head=fnames, adder=adders, cutoff=cutoffs, stringsAsFactors=FALSE)
mat = scenarios = as.matrix(scenarios)
scenarios = apply(scenarios, 1, paste, collapse="_", sep="")
mat = cbind(mat, scenarios)
drop = which(grepl("__", scenarios) & grepl("Human|SS_Add", scenarios))
drop = c(drop, which(grepl("SS_1|SS_Mask_1", scenarios) | grepl("Raw.*_1", scenarios)))
mat = mat[-drop, ]
mat = data.frame(mat, stringsAsFactors=FALSE)
mat$scenarios = gsub(mat$scenarios, pattern="__", replacement="_")
mat$scenarios = gsub("(.*)0$", "\\1", mat$scenarios)


dn = file.path(dirname(imgs), "Skull_Stripped")
stub = bn = basename(imgs)
bn = gsub(".nii.gz", "", bn, fixed=TRUE)
dn = file.path(dn, bn)

dat = t(sapply(mat$scenarios, function(x) paste(dn, x, sep="_")))
dat = data.frame(fname=c(dat), stringsAsFactors=FALSE)
dat$head = mat$head
dat$adder = mat$adder
dat$cutoff = mat$cutoff
dat$scenario = mat$scenarios
dat$fname = paste0(dat$fname, ".nii.gz")
dat$roi = rep(rois, each=nrow(mat))
dat$stub = rep(bn, each = nrow(mat))

xlong = dat
load(file=file.path(basedir, "Overlap_Data.Rda"))

types = unique(dd$scenario)
tcols = c(sapply(types, paste, cols, sep="_"))


# tmp = dat
# tmp$roi = NULL
# cn = colnames(tmp)
# for (icol in types) cn[cn == icol] = paste0(icol, ".fname")
# for (icol in cols) cn = gsub(paste0("_", icol), paste0(".", icol), cn,
#                              fixed=TRUE)
# cn = gsub("_0.", "_0_", cn, fixed=TRUE)
# colnames(tmp) = cn
# tmp$ID = 1:nrow(tmp)
# 
# long = reshape(tmp, direction="long", 
#                varying=cn, 
#                idvar="ID", timevar="value" )
# cn = colnames(long)
# cn = gsub("_0_", ".0_", cn, fixed=TRUE)
# colnames(long) = cn
# cn = cn[! cn %in% c("ID", "value")]
# rownames(long) = NULL
# long = reshape(long, direction="long", 
#                varying=cn, 
#                idvar=c("ID", "value"), timevar="cutoff" )
# long$cutoff = gsub("_", ".", long$cutoff)
# rownames(long) = NULL

scuts = as.character(cutoffs)
scuts = gsub("0$", "", scuts)
for (icut in scuts) {
  for (icol in cols){
    stopifnot(all(tmp[, paste0("Human_1_", icut, "_", icol)] ==
                    tmp[, paste0("Raw_", icut, "_", icol)]))
  }
}


tmp = dat
tmp$id = dn
tmp$roi = NULL
tmp = tmp[, c("id", tcols)]
long = melt(tmp, id.vars="id")
long$variable = as.character(long$variable)
long$variable = gsub("_Mask", ".Mask", long$variable)
long$variable = gsub("_Add", ".Add", long$variable)
ss = strsplit(long$variable, "_")
head = sapply(ss, function(x){
  x[1]
})
val.type = sapply(ss, function(x){
  x[length(x)]
})
cutoff = sapply(ss, function(x){
  x[length(x)-1]
})
adder = sapply(ss, function(x){
  ifelse(length(x) == 4, x[2], "0")
})

scen = cbind(head, adder, cutoff, val.type)
colnames(scen) = c("head", "adder", "cutoff", "val.type")
long = cbind(long, scen)
long$head = gsub(".", "_", long$head, fixed=TRUE)
# long$variable = NULL
long$Mask = grepl("Mask", long$variable)




save(long, file=file.path(basedir, "Overlap_Data_Long.Rda"))


pdf(file.path(basedir, "All_Overlap.pdf"))
g = ggplot(long, aes(y=value, x=head, color=adder))
gbox = g + geom_boxplot()
gfacet = gbox + facet_wrap(~ val.type + cutoff)

gfacet

dice = long[ long$val.type == "dice",]

gdice = gbox + facet_wrap(~ cutoff)
gdice = gdice %+% dice
gdice + ggtitle("Dice Overlap")

dice01 = long[ long$val.type == "dice" & long$cutoff == 0.1,]

gdice01 = gbox  %+% dice01
gdice01 + ggtitle("Dice Overlap for -f 0.1")


### Switching up plotting - so you can compare methods next to each other
gg = ggplot(long, aes(y=value, x=adder, color=head))
ggbox = gg + geom_boxplot()
ggfacet = ggbox + facet_wrap(~ val.type + cutoff)
ggfacet

ggdice = ggbox + facet_wrap(~ cutoff)
ggdice = ggdice %+% dice
ggdice + ggtitle("Dice Overlap")

ggdice01 = ggbox  %+% dice01
ggdice01 + ggtitle("Dice Overlap for -f 0.1")

gv = ggplot(long, aes(y=value, x=adder, color=variable))
gvbox = gv + geom_boxplot()

## Looking at the effect of threshold
gc = ggplot(long, aes(y=value, x=adder, color=cutoff))
gcbox = gc + geom_boxplot()
gcfacet = gcbox + facet_wrap(~ val.type + head)
gcfacet

ggdice = ggbox + facet_wrap(~ cutoff)
ggdice = ggdice %+% dice
ggdice + ggtitle("Dice Overlap")

#_Raw_ - just try bet on the image from dcm2nii, no manipulation
#_Raw_Mask_ - same, just filled hte mask
#_SS_ - no adding - just thresholding
#_SS_Mask - same, but filled in mask
#_Human_ added zeroes, but no thresholding
#_Human_ added zeroes, but no thresholding


dev.off()