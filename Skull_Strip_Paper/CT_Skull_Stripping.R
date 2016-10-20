
## ----label=opts, results='hide', echo=FALSE, message = FALSE, warning=FALSE----
library(knitr)
knit_hooks$set(webgl = hook_webgl) 
opts_chunk$set(echo=FALSE, prompt=FALSE, message=FALSE, warning=FALSE, comment="", results='hide')


## ----label=setup, echo=FALSE---------------------------------------------
rm(list=ls())
library(cttools)
library(fslr)
library(oro.dicom)
library(bitops)
library(arules)
library(plyr)
library(reshape2)
library(ggplot2)
library(matrixStats)
library(gridExtra)
library(qdap)
options(matlab.path='/Applications/MATLAB_R2014b.app/bin')

# username <- Sys.info()["user"][[1]]
rootdir = path.expand("~/CT_Registration")

ROIformat = FALSE
study = "Original_Images"
if (ROIformat) {
  study = "ROI_images"
}

basedir = file.path(rootdir, "Final_Brain_Seg")
resdir = file.path(basedir, "results")
paperdir = file.path(basedir, "Skull_Strip_Paper")
figdir = file.path(paperdir, "figure")
progdir = file.path(basedir, "programs")

new.ids = readLines(file.path(progdir, "newid_list.txt"))

homedir <- file.path(basedir, study)

rdas = list.files(path=homedir, pattern=".*_CT_.*Header_Info.Rda", 
                  full.names = TRUE, recursive = TRUE)
gant = rdas[grepl("antry", rdas)]
gant = gsub("_ungantry", "", gant)
rdas = rdas[ ! (rdas %in% gant)]
# stopifnot(!any(grepl("antry", rdas)))
rdas = rdas[grepl("Sorted", rdas)]
rda = data.frame(rda=rdas, stringsAsFactors = FALSE)
rda$id = basename(rda$rda)
rda$id = gsub("(.*)_Header_Info.*", "\\1", rda$id)
rda$id = gsub("_ungantry", "", rda$id)

runold = TRUE
if (runold) rda = rda[ !(rda$id %in% new.ids), ]


get.val = function(rda, val){
  if ("dcmtables" %in% ls()) rm(list="dcmtables")
  ungant.rda = gsub("_Header_Info\\.Rda", 
    "_ungantry_Header_Info\\.Rda", 
    rda)
  if (file.exists(ungant.rda)) rda = ungant.rda
  load(rda)
  cn = colnames(dcmtables)
  n.slice = length(unique(dcmtables[, "0018-0050-SliceThickness"]))
  co.kern = unique(dcmtables[, val])
  co.kern$n.slice = n.slice
  stopifnot(nrow(co.kern) == 1)
  co.kern
}

fname = file.path(resdir, "Overlap_Statistics.Rda")
load(fname)

img.id = unique(basename(ddf$img))
img.id = nii.stub(img.id)
rda = rda[ rda$id %in% img.id, ]
rownames(rda) = NULL

##############################
# Gantry tilt numbers and manufacturer
##############################
img.data = ldply(.data=rda$rda,  get.val, 
  val=c("0018-1210-ConvolutionKernel", 
  "0008-0070-Manufacturer", 
  "0018-1120-GantryDetectorTilt"), 
.progress="text")

colnames(img.data) = c("kern", "man", "tilt", "nslices")
img.data$rda  = rda$rda
img.data$tilt = as.numeric(img.data$tilt)
# data$rda = df$rda

man.tab = sort(table(img.data$man), decreasing=TRUE)
stopifnot(length(man.tab) == 3)
manu= names(man.tab)
manu[manu == 'TOSHIBA'] = "Toshiba"
manu[manu == 'SIEMENS'] = "Siemens"

check.na = function(x){
  stopifnot(all(!is.na(x))) 
}
check.na(img.data$tilt)
n.gant = sum(img.data$tilt != 0)


x = sapply(rda$rda, function(x){
  load(x)
  st = dcmtables[, "0018-0050-SliceThickness"]
  ust = unique(st)
  lust = length(ust)
if (lust > 1){
  print(ust)
}
  lust
})

n.var.slice = sum(x > 1)

proper = function(mystr) {
  x= strsplit(mystr, " ")[[1]]
  paste(toupper(substr(x, 1, 1)), tolower(substring(x, 2)),
        sep= "", collapse=" ")
}
uid = unique(basename(ddf$img))
nscans = length(uid)
num_scans = proper(replace_number(nscans))

pid = gsub("(\\d\\d\\d-(\\d|)\\d\\d\\d).*", "\\1", uid)
pid = unique(pid)
npt = length(pid)
ctr = unique(gsub("(\\d\\d\\d)-.*", "\\1", uid))
n.ctr = length(ctr)

ddf = ddf[ !grepl("refill", ddf$ssimg), ]

cs =  sapply(ddf, class) == "list"
cs = names(cs)[cs]
for (icol in cs){
  ddf[, icol] = unlist(ddf[, icol])
}

d = ddf
d$truevol = d$estvol = NULL
makeint = function(data){
  data$scen = gsub(".*_SS_(.*)_Mask.*", "\\1", data$ssimg )
  data$smooth = !grepl("nopresmooth", data$scen)
  data$smooth = revalue(as.character(data$smooth), 
                     c("TRUE"="Smoothed", "FALSE"="Unsmoothed"))
  data$int = gsub("_nopresmooth", "", data$scen)
  data
}
ddf = makeint(ddf)
ddf$diffvol = (ddf$truevol - ddf$estvol) / 1000
ddf$absdiff = abs(ddf$diffvol)

long = melt(d, id.vars = c("id", "img", "rimg", 
	"ssimg"))

long = makeint(long)
long$id = as.numeric(factor(long$id))

runcols =  c("dice", "jaccard", "sens", "spec", "accur", "absdiff")
rc = runcols[ !runcols %in% c("absdiff")]


wmax = function(x){
	which(x == max(x))
}
x = ddf[ ddf$img == ddf$img[1], ]


res = ddply(ddf, .(img), function(x){
	print(x$id[1])
	xx = sapply(x[, rc], wmax)
	xx = x$scen[xx]
	print(xx)
	names(xx) = rc
	xx
})

results= sapply(res[, rc], table)
maxtab = sapply(results, function(x) {
	names(sort(x, decreasing=TRUE)[1])
})

res = ddply(ddf, .(scen), function(x){
	cmin = colMins(x[, runcols])
	cmax = colMaxs(x[, runcols])
	cmean = colMeans(x[, runcols])
	cmed = colMedians(as.matrix(x[, runcols]))
	xx = data.frame(t(cbind(cmin, cmax, cmean, cmed)))
	xx$run = c("min", "max", "mean", "median")
	xx
})


nospec = long[ long$variable %in% c("accur", "sens"),]

long = long[ long$variable != "jaccard", ]

long$variable = revalue(long$variable, c("sens" = "Sensitivity",
                         "spec" = "Specificity",
                         "accur" = "Accuracy", 
                         "dice" = "Dice Similarity Index"))





## ----tests---------------------------------------------------------------

mytest = function(...){
  wilcox.test(...)
}
all.smooth.tests = ddply(long, .(int), function(df){
  p.value= ddply(df, .(variable), function(x){
    stats = lapply(list(median, mean, sd), function(func){
      res = aggregate(value ~ smooth, func, data=x)
    })
    names(stats) = c("median", "mean", "sd")
    cn = stats[[1]]$smooth
    stats = lapply(stats, function(x){
      x$smooth = NULL
      x = c(t(x))
      names(x) = cn
      x
    })
    stats = unlist(stats)
    wt = wilcox.test(value ~ smooth, data=x, paired=TRUE)
    tt = t.test(value ~ smooth, data=x, paired=TRUE)
    return(c(wt.p.value=wt$p.value, tt.p.value = tt$p.value,  stats))
  })
}, .progress="text")


all.int.tests = ddply(long, .(smooth), function(df){
  df = df[df$int %in% c("0.01", "0.1"), ]
  p.value= ddply(df, .(variable), function(x){
    stats = lapply(list(median, mean, sd), function(func){
      res = aggregate(value ~ int, func, data=x)
    })
    names(stats) = c("median", "mean", "sd")
    cn = stats[[1]]$int
    stats = lapply(stats, function(x){
      x$int = NULL
      x = c(t(x))
      names(x) = cn
      x
    })
    stats = unlist(stats)
    wt = wilcox.test(value ~ int, data=x, paired=TRUE)
    tt = t.test(value ~ int, data=x, paired=TRUE)
    return(c(wt.p.value=wt$p.value, tt.p.value = tt$p.value,  stats))
  })
}, .progress="text")

smooth = all.int.tests[ all.int.tests$smooth == "Smoothed",]
pval <- function(pval) {
  x <- ifelse(pval < 0.001, "< 0.001", sprintf("= %03.3f", pval))
	return(x)
}
smooth$wt.p.value = pval(smooth$wt.p.value)
num = sapply(smooth, class) == "numeric"
smooth[, num] = round(smooth[, num], 4)
rownames(smooth) = smooth$variable
smooth$variable = smooth$smooth = NULL
smooth.dice = smooth["Dice Similarity Index",]


## ----check_p-------------------------------------------------------------
stopifnot(all(all.smooth.tests$wt.p.value < 0.01))


## ----figcap_CT_Skull_Stripping_Figure2-----------------------------------
CT_Skull_Stripping_Figure2 = paste0("{\\bf Performance Metric Distribution for Different Pipelines.} ", 
"Panel~\\protect\\subref*{unsmoothed} displays the boxplots for performance measures when running the pipeline with a different fractional intensity (FI), using smoothed data (top) or unsmoothed data (bottom).  Panel~\\protect\\subref*{smoothed} presents the smoothed data only, rescaled to show discrimination between the different FI.", " Overall, FI of $0.01$ and $0.1$ perform better than $0.35$ in all categories other than specificity.  Using smoothed data improves performance in all performance metrics, markedly when an FI of $0.35$ is used.  Panel~\\protect\\subref*{smoothed} demonstrates that using an FI of $0.01$ on smoothed data has high performance on all measures.  " )


## ----CT_Skull_Stripping_Figure2, fig.height=7, fig.width=7, dpi = 600, fig.dev="png", fig.cap=CT_Skull_Stripping_Figure2----

#g = qplot(x = id, y = value, facets = smooth ~ variable , data = long, 
#  colour=int)
#g
long$v2 = long$variable
long$v2 = revalue(long$v2, c("Dice Similarity Index" = "Dice Similarity\nIndex"))
tsize = 16
pngname = file.path(figdir, "CT_Skull_Stripping_Figure2.png")
png(pngname)
g = qplot(x = v2, y = value, data = long, facets = smooth~ .,
          colour=int, geom=c("boxplot")) + xlab("Metric") + ylab("Value") +
  scale_color_discrete("Fractional Intensity") + 
  ggtitle("Performance Metric Distribution for Different Pipelines") +
  theme(legend.position = c(.5, .75),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent", 
                                  color="transparent"),
        legend.text = element_text(size=tsize+2), 
        legend.title = element_text(size=tsize),
        title = element_text(size=tsize),
        strip.text = element_text(size = tsize+4),
        axis.text  = element_text(size=tsize-2))
d = data.frame(label="A", smooth="Unsmoothed")
g = g + geom_text(data=d, x = 4, y = 0.2, size=20,
                  aes(label=label), colour="black")

print(g)
dev.off()




## ----CT_Skull_Stripping_Figure2b, fig.height=7, fig.width=7, dpi = 600, fig.dev="png", fig.cap=CT_Skull_Stripping_Figure2----
pngname = file.path(figdir, "CT_Skull_Stripping_Figure2b.png")
png(pngname)
g2 = qplot(x = v2, y = value, data =long[ long$smooth == "Smoothed",],
          colour=int, geom=c("boxplot")) + xlab("Metric") + ylab("Value") +
  scale_color_discrete("Fractional Intensity") + 
  ggtitle("Performance Metric Distribution for Smoothed Pipelines") +
  theme(legend.position = c(.68, .65),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent", 
                                  color="transparent"),
        legend.text = element_text(size=tsize), 
        legend.title = element_text(size=tsize),
        title = element_text(size=tsize),
        plot.title = element_text(hjust = 0.8),
        strip.text = element_text(size = tsize + 4),
        axis.text  = element_text(size=tsize)) + 
  scale_y_continuous(limits=c(.95, 1))
d = data.frame(label="B", smooth="Unsmoothed")
g2 = g2 + geom_text(data=d, x = 4, y = 0.953, size=20,
                  aes(label=label), colour="black")
print(g2)
dev.off()


## ----newplots------------------------------------------------------------
long.st = all.smooth.tests
long.st = rename(long.st, c(variable="measure"))
long.st$wt.p.value = long.st$tt.p.value = NULL
long.st = melt(long.st, 
                         id.vars = c("int", "measure"))
long.st$type = gsub("(.*)\\.(.*)", "\\1", long.st$variable)
long.st$smooth = gsub("(.*)\\.(.*)", "\\2", long.st$variable)
# long.st$variable = NULL
long.st =  long.st[!long.st$type %in% c("sd"), ]
long.st =  long.st[!long.st$int %in% c("0.35"), ]

## ----noplot,eval=FALSE---------------------------------------------------
## g = ggplot(long.st, aes(x=int, y=value)) + facet_grid(smooth ~ measure) + geom_point(aes(colour=type), position=position_dodge(width=0.3))
## g
## 
## g = ggplot(long.st, aes(x=type, y=value)) + facet_grid(smooth ~ measure) + geom_point(aes(colour=int), position=position_dodge(width=0.3))
## g


## ----cache=TRUE----------------------------------------------------------
img.fname = file.path(basedir, "Test_Cases", "101-307", "101-307_20061110_1638_CT_5_RM_Head")
img = readNIfTI(img.fname, reorient=FALSE)
img.dim = pixdim(img)[2:4]
rm(list="img")
img.dim = round(img.dim, 2)


