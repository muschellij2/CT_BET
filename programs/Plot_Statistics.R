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
homedir <- file.path(basedir, study)
#basedir <- file.path("/Volumes/CT_Data/MISTIE")

fname = file.path(resdir, "Overlap_Statistics.Rda")
load(fname)

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
# res = ddply(ddf, .(id), function(x){
# 	cmin = colMins(x[, runcols])
# 	cmax = colMaxs(x[, runcols])
# 	cmean = colMeans(x[, runcols])
# 	xx = data.frame(t(cbind(cmin, cmax, cmean)))
# 	xx$run = c("min", "max", "mean")
# 	xx
# })

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

g = qplot(x = id, y = value, facets = smooth ~ variable , data = long, 
	colour=int)
g

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

g = qplot(x = variable, y = value, data =long[ long$smooth == "Smoothed",],
          colour=int, geom=c("boxplot")) + xlab("Metric") + ylab("Value") +
  scale_color_discrete("Fractional Intensity") + 
  ggtitle("Performance Metric Distribution for Different Pipelines") +
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
print(g)

dd = ddf[ ddf$smooth == "Smoothed" & ddf$int %in% c("0.01", "0.1") ,]
g = qplot(x = 1-spec, y = sens, data = dd, facets = int ~ .,
          colour=id) + guides(colour=FALSE) +
          xlab("1 - Specificity") + ylab("Sensitivity") + 
  scale_y_continuous(limits=c(.95, 1))
g

t.test(dd$accur[dd$int== "0.01"], dd$accur[dd$int== "0.1"], paired=TRUE)
plot(dd$accur[dd$int== "0.01"], dd$accur[dd$int== "0.1"])
abline(a=0, b=1)

g = qplot(x = variable, y = value, data = long[ long$int == .1, ],
          geom=c("boxplot"), colour = smooth)
g


res[ res$run == "min",]
res[ res$run == "mean",]


g = qplot(x = value, facets = ~ variable, data = nospec, 
	fill=int, alpha = I(0.5), geom="histogram", position="identity")
g

g = qplot(x = value, facets = ~ variable, data = nospec, 
	colour=int, alpha = I(0.5), geom="line", stat="density")
g



g = qplot(y = sens, x=1-spec, facets = ~ int, data = ddf, 
	colour=id, alpha = I(0.5)) + guides(colour=FALSE)
g 