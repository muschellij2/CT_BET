

## ----label=setup, echo=FALSE---------------------------------------------
rm(list=ls())
library(cttools)
library(fslr)
library(plyr)
library(reshape2)
library(ggplot2)
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

fname = file.path(resdir, "Overlap_Statistics.Rda")
x = load(fname)

check.na = function(x){
  stopifnot(all(!is.na(x))) 
}


proper = function(mystr) {
  x= strsplit(mystr, " ")[[1]]
  paste(toupper(substr(x, 1, 1)), tolower(substring(x, 2)),
        sep= "", collapse=" ")
}

ddf = ddf[ !grepl("refill", ddf$ssimg), ]

cs =  sapply(ddf, class) == "list"
cs = names(cs)[cs]
for (icol in cs){
  ddf[, icol] = unlist(ddf[, icol])
}

d = ddf
N = length(unique(ddf$pid))
long = melt(d, id.vars = c("id", "img", "rimg", 
                           "ssimg"))

makeint = function(data){
  data$scen = gsub(".*_SS_(.*)_Mask.*", "\\1", data$ssimg )
  data$smooth = !grepl("nopresmooth", data$scen)
  data$smooth = revalue(as.character(data$smooth), 
                        c("TRUE"="Smoothed", "FALSE"="Unsmoothed"))
  data$int = gsub("_nopresmooth", "", data$scen)
  data
}
long = makeint(long)
long$id = as.numeric(factor(long$id))

nospec = long[ long$variable %in% c("accur", "sens"),]

long = long[ long$variable != "jaccard", ]
long = long[ long$variable %in% c("sens", "spec", "accur", "dice"),]

long$variable = revalue(long$variable, c("sens" = "Sensitivity",
                                         "spec" = "Specificity",
                                         "accur" = "Accuracy", 
                                         "dice" = "Dice Similarity Index"))
long$value = as.numeric(as.character(long$value))
xlong = long
## ----CT_Skull_Stripping_Figure2, fig.height=7, fig.width=7, dpi = 600, fig.dev="png", fig.cap=CT_Skull_Stripping_Figure2----

#g = qplot(x = id, y = value, facets = smooth ~ variable , data = long, 
#  colour=int)
#g
long = long[ !long$variable %in% c("truevol", "estvol"), ]
long$v2 = long$variable
slong = long[ long$v2 %in% c("Sensitivity", "Specificity"), ]

tsize = 16

pngname = file.path(figdir, "All_Outcomes_Figure2.png")
png(pngname, res=600, height=7, width=7, units= "in")

g = qplot(x = variable, y = value, 
          data = xlong[ xlong$smooth == "Smoothed" & long$int %in% 0.01, ], 
          geom=c("boxplot")) + 
  ggtitle(paste0(
    "Boxplot of Performance of \nAutomatic vs. Manual\nSkull Stripping (N = ", 
    N, ")")) +
  ylab("Value") + 
  xlab("Performance Measure") +
  theme(legend.position = c(.5, .5),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent", 
                                  color="transparent"),
        legend.text = element_text(size=tsize+2), 
        legend.title = element_text(size=tsize),
        title = element_text(size=tsize),
        strip.text = element_text(size = tsize+4),
        axis.text  = element_text(size=tsize-2)) + 
  scale_y_continuous(limits=c(0.95, 1))

# d = data.frame(label="A", smooth="Unsmoothed")
# g = g + geom_text(data=d, x = 4, y = 0.2, size=20,
#                   aes(label=label), colour="black")
print(g)
dev.off()

pngname = file.path(figdir, "All_Outcomes_Figure.png")

png(pngname, res=600, height=7, width=7, units= "in")

print({g + scale_y_continuous(limits=c(0, 1)) })
dev.off()

long = long[ long$v2 %in% "Dice Similarity Index", ]
# slong = long[ long$v2 %in% "Sensitivity", ]


long$v2 = revalue(long$v2, c("Dice Similarity Index" = 
                               "Dice Similarity\nIndex"))
pngname = file.path(figdir, "Figure2_0.01.png")
png(pngname, res=600, height=7, width=7, units= "in")
g = qplot(x = v2, y = value, data = long[ long$int == "0.01", ], 
          colour=smooth, geom=c("boxplot")) + 
  xlab("Performance Metric") + 
  ylab("Performance Metric Value") +
  scale_color_discrete("") + 
  ggtitle("Performance Metric Distribution for CT Skull Stripping (FI = 0.01)") +
  theme(legend.position = c(.7, .75),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent", 
                                  color="transparent"),
        legend.text = element_text(size=tsize+2), 
        legend.title = element_text(size=tsize),
        title = element_text(size=tsize),
        strip.text = element_text(size = tsize+4),
        axis.text  = element_text(size=tsize-2))
# d = data.frame(label="A", smooth="Unsmoothed")
# g = g + geom_text(data=d, x = 4, y = 0.2, size=20,
#                   aes(label=label), colour="black")
print(g)
dev.off()


pngname = file.path(figdir, "Unsmoothed_Figure2.png")
png(pngname, res=600, height=7, width=7, units= "in")
g = qplot(x = int, y = value, 
          data = long[ long$smooth == "Unsmoothed", ], 
          geom=c("boxplot")) + 
  xlab("Fractional Intensiity") + 
  ylab("Dice Similarity Index") +
  ggtitle("Dice Similarity Index for Unsmoothed Pipelines") +
  theme(legend.position = c(.5, .5),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent", 
                                  color="transparent"),
        legend.text = element_text(size=tsize+2), 
        legend.title = element_text(size=tsize),
        title = element_text(size=tsize),
        strip.text = element_text(size = tsize+4),
        axis.text  = element_text(size=tsize-2)) + 
  scale_y_continuous(limits=c(0, 1)) 
# d = data.frame(label="A", smooth="Unsmoothed")
# g = g + geom_text(data=d, x = 4, y = 0.2, size=20,
#                   aes(label=label), colour="black")
print(g)
dev.off()




g = qplot(x = int, y = value, 
          data = long[ long$smooth == "Smoothed", ], 
          geom=c("boxplot")) + 
  xlab("Fractional Intensiity") + 
  ylab("Dice Similarity Index") +
  ggtitle("Dice Similarity Index for Smoothed Pipelines") +
  theme(legend.position = c(.5, .5),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent", 
                                  color="transparent"),
        legend.text = element_text(size=tsize+2), 
        legend.title = element_text(size=tsize),
        title = element_text(size=tsize),
        strip.text = element_text(size = tsize+4),
        axis.text  = element_text(size=tsize-2))
# d = data.frame(label="A", smooth="Unsmoothed")
# g = g + geom_text(data=d, x = 4, y = 0.2, size=20,
#                   aes(label=label), colour="black")
pngname = file.path(figdir, "Smoothed_Figure2.png")
png(pngname, res=600, height=7, width=7, units= "in")
print(g + 
        scale_y_continuous(limits=c(0, 1))
      )
dev.off()

pngname = file.path(figdir, "Smoothed_Figure2_2.png")
png(pngname, res=600, height=7, width=7, units= "in")
print(g + 
        scale_y_continuous(limits=c(0.95, 1)))
dev.off()




pngname = file.path(figdir, "Smoothed_Figure2_Sens.png")
png(pngname, res=600, height=7, width=7, units= "in")
g = qplot(x = int, y = value, 
          data = slong[ slong$smooth == "Smoothed", ], 
          colour = v2,
          geom=c("boxplot")) + 
  xlab("Fractional Intensiity") + 
  ylab("") +
  scale_color_discrete("") + 
  ggtitle("Sensitivity and Specificity for Smoothed Pipelines") +
  theme(legend.position = c(.5, .5),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent", 
                                  color="transparent"),
        legend.text = element_text(size=tsize+2), 
        legend.title = element_text(size=tsize),
        title = element_text(size=tsize),
        strip.text = element_text(size = tsize+4),
        axis.text  = element_text(size=tsize-2)) + 
  scale_y_continuous(limits=c(.95, 1))
# d = data.frame(label="A", smooth="Unsmoothed")
# g = g + geom_text(data=d, x = 4, y = 0.2, size=20,
#                   aes(label=label), colour="black")
print(g)
dev.off()




