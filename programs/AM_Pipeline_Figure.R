
## ----label=opts, results='hide', echo=FALSE, message = FALSE, warning=FALSE----
library(knitr)
library(plyr); 
library(dplyr)
library(tidyr)
knit_hooks$set(webgl = hook_webgl) 
opts_chunk$set(echo=FALSE, prompt=FALSE, message=FALSE, warning=FALSE, comment="", results='hide')


## ----label=setup, echo=FALSE---------------------------------------------
rm(list=ls())
library(plyr)
library(reshape2)
library(ggplot2)
options(matlab.path='/Applications/MATLAB_R2014b.app/bin')

# username <- Sys.info()["user"][[1]]
rootdir = path.expand("~/CT_Registration")

basedir = file.path(rootdir, "Final_Brain_Seg")
resdir = file.path(basedir, "results")
paperdir = file.path(basedir, "Skull_Strip_Paper")
figdir = file.path(paperdir, "figure")
progdir = file.path(basedir, "programs")

iout = "Union"
 
for (iout in c("AM", "Intersection", "Union")){

  addon =switch(iout,
                "AM" = "Reader 2", 
                "Union" = "Reader 1/2 Union", 
                "Intersection" = "Reader 1/2 Intersection")
  addlet = switch(iout,
                 "AM" = "A", 
                 "Union" = "C", 
                 "Intersection" = "B")
  fname = file.path(resdir, paste0(iout, "_Overlap_Statistics.Rda"))
  load(fname)

  ddf = ddf[ !grepl("refill", ddf$ssimg), ]

  cs =  sapply(ddf, class) == "list"
  cs = names(cs)[cs]
  for (icol in cs){
    ddf[, icol] = unlist(ddf[, icol])
  }

  ddf$am_img = ddf$nat_img = NULL

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


  long = melt(d, id.vars = c("pid", "id", "img", "rimg", 
                             "ssimg", "hdr"))

  long = makeint(long)
  long$id = as.numeric(factor(long$id))

  runcols =  c("dice", "jaccard", "sens", "spec", "accur", "absdiff")
  rc = runcols[ !runcols %in% c("absdiff")]

  d = ddf[ ddf$int %in% c(0.01, 0.1) & ddf$smooth == "Smoothed", ]

  t.test(sens ~ factor(int), data=d, paired=TRUE)

  s = spread(d[, c("id", "int", "sens")], key=int, value=sens)
  s$diff = s$`0.01` - s$`0.1`
  t.test(s$diff)

  s = spread(d[, c("id", "int", "accur")], key=int, value=accur)
  s$diff = s$`0.01` - s$`0.1`
  t.test(s$diff)

  s = spread(d[, c("id", "int", "dice")], key=int, value=dice)
  s$diff = s$`0.01` - s$`0.1`
  t.test(s$diff)


  nospec = long[ long$variable %in% c("accur", "sens"),]



  long = long[ long$variable != "jaccard", ]

  long$variable = revalue(long$variable, c("sens" = "Sensitivity",
                                           "spec" = "Specificity",
                                           "accur" = "Accuracy", 
                                           "dice" = "Dice Similarity Index"))



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
  pngname = file.path(figdir, 
    paste0(iout, "_CT_Skull_Stripping_Figure2.png"))
  png(pngname, res=600, units = "in", height=7, width=7)
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


  pngname = file.path(figdir, 
    paste0(iout, "_CT_Skull_Stripping_Figure2b.png"))
  png(pngname, res=600, units = "in", height=7, width=7)
  stopifnot(all(long[ long$smooth == "Smoothed","value"] > 0.95))
  g2 = qplot(x = v2, y = value, data =long[ long$smooth == "Smoothed",],
             colour=int, geom=c("boxplot")) + xlab("Metric") + ylab("Value") +
    scale_color_discrete("Fractional Intensity") + 
    ggtitle(paste0("Performance Metric Distribution for ", addon)) +
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
  d = data.frame(label=addlet, smooth="Unsmoothed")
  g2 = g2 + geom_text(data=d, x = 4, y = 0.953, size=20,
                      aes(label=label), colour="black")
  print(g2)
  dev.off()
  
  ##################
  # testing
  ##################
  
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
  
  df = long[ long$smooth == "Smoothed", ]
  df = df[df$int %in% c("0.01", "0.1"), ]
  stopifnot(all(df$value > .95))

  
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
  
  all.med.diffs = ddply(long, .(smooth), function(df){
    df = df[df$int %in% c("0.01", "0.1"), ]
    df = df[ order(df$id, df$variable, df$int), ]
    diffs= ddply(df, .(variable), function(x){
      #     x = df[ df$variable == "Sensitivity", ]
      runvar = x$variable[1]
      x = x[, c("value", "img", "int")]
      x = reshape(x, direction="wide", idvar = "img", timevar="int")
      d = x$value.0.01 - x$value.0.1
      check = wilcox.test(d)
      tt.check = t.test(d)
      return(c(median=median(d), mean=mean(d), sd=sd(d), wt.p.value = check$p.value,
               tt.p.value = tt.check$p.value))
    })
  }, .progress="text")
  
  stopifnot(all.med.diffs$wt.p.value == all.int.tests$wt.p.value)
  stopifnot(all.med.diffs$tt.p.value == all.int.tests$tt.p.value)
  
  all.int.tests = all.int.tests[ all.int.tests$smooth == "Smoothed", 
                                 c("variable", "median.0.01", "median.0.1",
                                   "wt.p.value")]
  all.int.tests$better.0.01 = all.int.tests$median.0.01 > all.int.tests$median.0.1
  all.int.tests$wt.p.value = round(all.int.tests$wt.p.value, 4)
  all.int.tests$median.0.01 = round(all.int.tests$median.0.01, 4)
  all.int.tests$median.0.1 = round(all.int.tests$median.0.1, 4)
  
  print(all.int.tests)
  print(iout)
}