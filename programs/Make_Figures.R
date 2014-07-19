rm(list=ls())
library(oro.dicom)
library(bitops)
library(arules)
library(ggplot2)

username <- Sys.info()["user"][[1]]

basedir <- file.path("~/Dropbox/CTR/DHanley/CT_Registration/Final_Brain_Seg/")
setwd(basedir)


dat <- read.csv(file=file.path(basedir, "Overlap_Statistics.csv"))
 
ss <- strsplit( as.character(dat$ss), split= .Platform$file.sep  )
ss <- sapply(ss, function(x) x[9])
dat$id <- factor(ss)

pdf(file=file.path(basedir, "Overlap_Meausres.pdf"))
for (icol in cols){
  fp <- paste("first_pass", icol, sep="_")
  ss2 <- paste("ss2", icol, sep="_")
  ss <- paste("ss", icol, sep="_")
  par(mfrow = c(1, 2))
  lims <- c(min(dat[, ss], dat[, ss2], dat[, fp], 0.9, na.rm=TRUE), 1)
  plot(y=dat[, ss], x=dat[, ss2], xlab="SS Human Extraction - 2 passes", ylab="SS - Human Extraction", xlim=lims, ylim=lims)
  abline(a=0, b=1)
  plot(y=dat[, ss], x=dat[, fp], xlab="SS No Human Extraction", ylab="SS - Human Extraction", xlim=lims, ylim=lims)
  abline(a=0, b=1)
  title(main=c(rep("", 3), icol), outer=TRUE)	
}
dev.off()

xdat <- dat



dat <- xdat
ids <- data.frame(id=1:nrow(dat))
ids$trueid <- xdat$id

dat <- dat[, colnames(dat) != "roi" ]
colnames(dat) <- gsub("first_pass", "firstpass", colnames(dat))

cdd <- colnames(dat) 
cdd <- colnames(dat) <- sapply(strsplit(cdd, "_"), function(x) paste0(rev(x), collapse='.'))
addtype <- grepl(".", cdd, fixed=TRUE) | grepl("id", cdd, fixed=TRUE)
dat <- dat[, addtype]
#colnames(dat)[addtype] <- paste0("type.", cdd[addtype])

ss <- strsplit(colnames(dat), "\\.")
metrics <- sort(unique(sapply(ss, function(x) x[1])))
metrics <- metrics[metrics != "id"]
run <- sort(unique(sapply(ss, function(x) x[2])))

dat <- dat[, c("id", apply(expand.grid(metrics, run), 1, paste, collapse="."))]

dat$id <- 1:nrow(dat)
cn <- colnames(dat)
ddat <- reshape(dat, direction="long", varying=cn[cn!="id"], times=metrics, timevar="type", idvar="id")

ddat$type <- factor(ddat$type)

ddat <- merge(ddat, ids, by="id")

# ddat$id <- NULL 
# ddat$id <- ddat$id

cn <- colnames(ddat)
cn <- cn[!cn %in% c("type", "id", "trueid")]

rownames(ddat) <- NULL
ddat$id <- paste(ddat$id, ddat$type, sep="-")
long <- reshape(ddat, direction="long", varying=cn, v.names="value", times=cn, timevar="metric", idvar="id")

long$id <- sapply(strsplit(long$id, "-"), function(x) x[1])
rownames(long) <- NULL

write.csv(x=long, row.names=FALSE, file=file.path(basedir,  "Overlap_Statistics_Long.csv"))


l <- long[long$type %in% "firstpass", ]
pdf(file=file.path(basedir, "box1.pdf"))
g <- ggplot(data=l, aes(x=metric, y=value, color=metric)) + 
  geom_boxplot() + scale_y_continuous(limits=c(.5, 1))
print(g)
dev.off()
pdf(file=file.path(basedir, "box2.pdf"))
g <- ggplot(data=l, aes(x=metric, y=value, color=metric)) + 
  geom_boxplot() 
print(g)
dev.off()
g <- ggplot(data=long, aes(x=type, y=value, color=metric)) + geom_boxplot()
g2 <- ggplot(data=ddat) + geom_boxplot(aes(x=id, y=spec)) 