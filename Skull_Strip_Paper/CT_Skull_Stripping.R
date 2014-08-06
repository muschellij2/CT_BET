
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
options(matlab.path='/Applications/MATLAB_R2013b.app/bin')

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

homedir <- file.path(basedir, study)
#basedir <- file.path("/Volumes/CT_Data/MISTIE")

fname = file.path(resdir, "Overlap_Statistics.Rda")
load(fname)

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



## ----figcap_CT_Skull_Stripping_Figure2-----------------------------------
CT_Skull_Stripping_Figure2 = paste0("{\\bf Performance Metric Distribution for Different Pipelines.} ", 
"Panel~\\protect\\subref*{smoothed} displays the boxplots for performance measures when running the pipeline with a different fractional intensity (FI), using smoothed data (top) or unsmoothed data (bottom).  Panel~\\protect\\subref*{unsmoothed} presents the smooothed data only, rescaled to show discrimination between the differernt FI.", "Overall, FI of $0.01$ and $0.1$ perform better than $0.35$ in all categories other than specificity.  Using smoothed data improves performance in all performance metrics, markedly when an FI of $0.35$ is used.  Panel~\\protect\\subref*{unsmoothed} demonstrates that using an FI of $0.01$ on smoothed data is the best pipeline.  " )


## ----CT_Skull_Stripping_Figure2, fig.height=7, fig.width=7, dpi = 600, fig.dev="png", fig.cap=CT_Skull_Stripping_Figure2----

#g = qplot(x = id, y = value, facets = smooth ~ variable , data = long, 
#  colour=int)
#g

pngname = file.path(figdir, "CT_Skull_Stripping_Figure2.png")
png(pngname)
g = qplot(x = variable, y = value, data = long, facets = smooth~ .,
          colour=int, geom=c("boxplot")) + xlab("Metric") + ylab("Value") +
  scale_color_discrete("Fractional Intensity") + 
  ggtitle("Performance Metric Distribution for Different Pipelines") +
  theme(legend.position = c(.5, .75),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent", 
                                  color="transparent"),
        legend.text = element_text(size=15), 
        legend.title = element_text(size=15),
        title = element_text(size=15),
        strip.text = element_text(size = 15),
        axis.text  = element_text(size=12))
print(g)
dev.off()




## ----CT_Skull_Stripping_Figure2b, fig.height=7, fig.width=7, dpi = 600, fig.dev="png", fig.cap=CT_Skull_Stripping_Figure2----
pngname = file.path(figdir, "CT_Skull_Stripping_Figure2b.png")
png(pngname)
g2 = qplot(x = variable, y = value, data =long[ long$smooth == "Smoothed",],
          colour=int, geom=c("boxplot")) + xlab("Metric") + ylab("Value") +
  scale_color_discrete("Fractional Intensity") + 
  ggtitle("Performance Metric Distribution for Smoothed Pipelines") +
  theme(legend.position = c(.68, .65),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent", 
                                  color="transparent"),
        legend.text = element_text(size=15), 
        legend.title = element_text(size=15),
        title = element_text(size=15),
        strip.text = element_text(size = 15),
        axis.text  = element_text(size=12)) + 
  scale_y_continuous(limits=c(.95, 1))
print(g2)
dev.off()


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
# colnames(all.wt)[1:3] = c("int", "variable", "p.value")

uint = unique(long$int)
# eg = expand.grid(int1= uint, int2 = uint, stringsAsFactors = FALSE)
eg = t(combn(uint, 2))
class(eg) = "numeric"
eg = data.frame(eg)
names(eg) = c("int1", "int2")
eg = eg[ eg$int1 != eg$int2, ]
rownames(eg) = NULL
smooth = long[ long$smooth == "Smoothed", ]

smooth.wt = apply(eg, 1, function(x){
  int1 = x["int1"]
  int2 = x["int2"]
  keep = smooth[ smooth$int %in% c(int1, int2), ]
  pvals = ddply(keep, .(variable), function(r){
    wt = mytest(value ~ int, data=r)
    wt$p.value
  })
})

smooth.wt = cbind(eg, t(sapply(smooth.wt, function(x) {
  xx = x$V1
  names(xx) = x$variable
  return(xx)
})))



