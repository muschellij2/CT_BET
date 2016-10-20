rm(list=ls())
library(cttools)
library(fslr)
library(oro.dicom)
library(bitops)
library(arules)
options(matlab.path='/Applications/MATLAB_R2014b.app/bin')
options(fsl.path='/usr/local/fsl')

# username <- Sys.info()["user"][[1]]
rootdir = path.expand("~/Dropbox/CTR/DHanley/CT_Registration")

gantry.ids = c("102-331", "102-331", "102-331", "102-374", "102-374", "152-302", 
               "152-302", "157-329", "157-329", "173-404", "173-404", "179-395", 
               "179-395", "225-503", "225-503", "225-515", "225-515", "230-352", 
               "230-352", "232-512", "232-512")
ROIformat = TRUE
if (ROIformat) {
  study = "AM_ROI_images"
}

homedir <- file.path(rootdir, "Final_Brain_Seg", study)
#basedir <- file.path("/Volumes/CT_Data/MISTIE")

setwd(homedir)

ids = list.dirs(homedir, recursive=FALSE, full.names=FALSE)
ids = basename(ids)
ids = grep("\\d\\d\\d-(\\d|)\\d\\d\\d", ids, value=TRUE)
length(ids)

iid = 1
# 13

for (iid in seq_along(ids)){

  id <- ids[iid]
  basedir = file.path(homedir, id)
  x = list.files(path=basedir, pattern="[.]nii")
  if (length(x) > 0){
    print(x)
  }

}

for (iid in seq_along(ids)){
  
  id <- ids[iid]
  gt_correct = FALSE
  if (id %in% gantry.ids) {
    gt_correct = TRUE
  }
  basedir = file.path(homedir, id)
  ssdir = file.path(basedir, "Skull_Stripped")
  if (!file.exists(ssdir)){
    dir.create(ssdir)
  }
  print(id)

  ### time for convertsion
  contime <- NULL
  gf = getfiles(basedir)
  # t = iconv("UTF-8","UTF-8//IGNORE",$t);

  verbose =TRUE
  untar = FALSE
  convert <- TRUE
  skullstrip <- TRUE
  plotss = TRUE
  regantry <- FALSE
  untgantry <- FALSE
  runall <- TRUE
  useRdcmsort= TRUE
  useRdcm2nii= FALSE
  removeDups = TRUE
  isSorted = NULL
  if (ROIformat) isSorted = FALSE
  dcm2niicmd = "dcm2nii_2009"
  if ( length(gf$files) > 0 | untar){
    contime <- system.time(convert_DICOM(basedir, 
                            verbose=verbose, untar=untar, 
                            useRdcmsort= useRdcmsort, 
                            useRdcm2nii= useRdcm2nii,
                            id = id, 
                            isSorted = isSorted,
                            removeDups=removeDups,
                            dcmsortopt=dcmsortopt, 
                            ROIformat = ROIformat,
                            gt_correct = gt_correct,
                            dcm2niicmd=dcm2niicmd))
#     gt_correct=TRUE,
#     add.img.dir=FALSE
  }

}