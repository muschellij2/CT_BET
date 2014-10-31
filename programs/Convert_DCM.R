rm(list=ls())
library(cttools)
library(fslr)
library(oro.dicom)
library(bitops)
library(arules)
options(matlab.path='/Applications/MATLAB_R2013b.app/bin')
options(fsl.path='/usr/local/fsl')

# username <- Sys.info()["user"][[1]]
rootdir = path.expand("~/Dropbox/CTR/DHanley/CT_Registration")

ROIformat = TRUE
study = "Original_Images"
if (ROIformat) {
  study = "ROI_images"
}

homedir <- file.path(rootdir, "Final_Brain_Seg", study)
#basedir <- file.path("/Volumes/CT_Data/MISTIE")

setwd(homedir)

ids = list.dirs(homedir, recursive=FALSE, full.names=FALSE)
ids = basename(ids)
ids = grep("\\d\\d\\d-(\\d|)\\d\\d\\d", ids, value=TRUE)
length(ids)

iid = 13
# 13

for (iid in seq_along(ids)){
  
  id <- ids[iid]

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
                            dcm2niicmd=dcm2niicmd))
#     gt_correct=TRUE,
#     add.img.dir=FALSE
  }

  mid.folder = function(x, folname = ""){
    d = dirname(x)
    b = basename(x)
    file.path(d, folname, b)
  }



  if (!ROIformat){
    if (skullstrip){

      imgs = list.files(basedir, pattern = "\\.nii", recursive=FALSE,
          full.names=TRUE)
      iimg = 1
      
      scen = expand.grid(int=c("0.01", "0.1", "0.35"),
                         presmooth=c(TRUE, FALSE),
                         refill = c(TRUE, FALSE))
      rownames(scen)= NULL
      w = which(!scen$presmooth & scen$refill)
      scen = scen[-w, ]
            
      for (iimg in seq_along(imgs)){

        img = imgs[iimg]
        ofile = nii.stub(img)
        ofile = mid.folder(ofile, "Skull_Stripped")
        ofile = paste0(ofile, "_SS")
        
        iscen = 1
        
        for (iscen in seq(nrow(scen))){
          
          int = scen$int[iscen]
          presmooth = scen$presmooth[iscen]
          refill = scen$refill[iscen]
          
          app = "_nopresmooth"
          if (presmooth) app = ""
          
          
          re_app = ""
          if (refill) re_app = "_refill"
          

          
          outfile = paste0(ofile, "_", int, app, re_app)
          x = CT_Skull_Strip(img = img, 
                             outfile = outfile, 
                             retimg=FALSE, verbose=TRUE, 
                             opts=paste0("-f ", int, " -v"),
                             inskull_mesh = FALSE,
                             refill = refill,
                             refill.thresh = .75, # used 0.5
                             presmooth=presmooth)   
          

        }
#         x = CT_Skull_Strip(img = img, 
#                            outfile = paste0(outfile, "_0.1_smooth"), 
#                            retimg=FALSE, verbose=TRUE, opts="-f 0.1 -v", 
#                            inskull_mesh = TRUE,
#                            presmooth=TRUE)        
# 
#         x = CT_Skull_Strip(img = img, 
#           outfile = paste0(outfile, "_0.01"), 
#           retimg=FALSE, verbose=TRUE, opts="-f 0.01 -v")
# 
#         x = CT_Skull_Strip(img = img, 
#           outfile = paste0(outfile, "_0.35"), 
#           retimg=FALSE, verbose=TRUE, opts="-f 0.35 -v")  
      }
        # opts="-f 0.01 -v"
        # retimg=FALSE
        # outfile = paste0(outfile, "_0.01")
        # keepmask = TRUE
        # maskfile = NULL
        # inskull_mesh = TRUE
        # reorient = FALSE
        # intern = TRUE



        # system.time(Skull_Strip(basedir, CTonly=TRUE, 
        #   opts="-f 0.1 -b", 
        #   verbose=verbose))


        # system.time(Skull_Strip(basedir, CTonly=TRUE, 
        #   opts="-f 0.01 -b", 
        #   verbose=verbose))

        # system.time(Skull_Strip(basedir, CTonly=TRUE, 
        #   opts="-f 0.35 -b", 
        #   verbose=verbose))
    }
  }

}