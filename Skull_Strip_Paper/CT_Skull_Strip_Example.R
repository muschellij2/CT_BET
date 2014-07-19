
#' @title CT Skull Stripping within R
#' @description Skull Stripping (using FSL's BET) a CT file using \code{fslr}
#' functions
#' @param img (character) File to be skull stripped or object of class
#' nifti
#' @param outfile (character) output filename
#' @param keepmask (logical) Should we keep the mask?
#' @param maskfile (character) Filename for mask (if \code{keepmask = TRUE}).
#' If \code{NULL}, then will do \code{paste0(outfile, "_Mask")}.
#' @param inskull_mesh (logical) Create inskull_mesh file from bet? 
#' (Warning - will take longer) This an exterior surface of the brain. (experimental)
#' Also, if \code{outfile} is \code{NULL}, then this will be created in
#' a temporary directory and not be retrieved.
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}. 
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) additional options to \code{\link{fslbet}}
#' @param presmooth (logical) indicator if pre-smoothing should be
#' done before BET 
#' @param sigma (integer) size of Gaussian kernel passed to 
#' \code{\link{fslsmooth}} if \code{presmooth} is \code{TRUE}
#' @param verbose (logical) Should diagnostic output be printed?
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return character or logical depending on intern
#' @export
CT_Skull_Strip <- function(
  img, 
  outfile = NULL,
  keepmask = TRUE,
  maskfile = NULL,
  inskull_mesh = FALSE,
  retimg = FALSE,
  reorient = FALSE,                 
  intern=TRUE,
  opts = "-f 0.01 -v", 
  presmooth = TRUE,
  sigma = 1,
  verbose=TRUE,
  ...
){
  require(fslr)
  if (retimg){
    if (is.null(outfile)) {
      outfile = tempfile()
    }
  } else {
    stopifnot(!is.null(outfile))
  }
  if (verbose){
    cat("Thresholding Image to 0-100\n")
  }
  
  outfile = nii.stub(outfile)
  run = fslthresh(img, thresh=0, uthresh = 100, 
                  outfile = outfile,
                  retimg = FALSE,
                  intern=FALSE)
  if (verbose){
    cat(paste0("Thresholding return: ", run, "\n"))
  } 
  
  if (verbose){
    cat(paste0("Creating 0 - 100 mask to remask after filling\n"))
  }     
  
  bonemask = fslbin(outfile, retimg = TRUE, intern=FALSE) 
  
  bonemask = fslfill(file = bonemask, bin=TRUE, 
                     retimg=TRUE,
                     intern=FALSE)
  
  ### Must prefill for the presmooth - not REALLY necessary, but if you're
  ### smoothing, you likely have noisy data.
  
  #   if (isTRUE(presmooth)){
  #     if (!isTRUE(prefill)){
  #       warning("if presmooth, must have prefill = TRUE")
  #       prefill= TRUE
  #     }
  #   }
  #   #### prefill will mask the img 
  #   if (isTRUE(prefill)){
  #     if (verbose){
  #       cat(paste0("Remasking 0 - 100 after filling holes\n"))
  #     }    
  #     run = fslmask(img, 
  #                   mask = bonemask,
  #                   outfile = outfile, 
  #                   retimg = FALSE,
  #                   intern = FALSE)  
  #   }
  
  
  if (isTRUE(presmooth)){
    
    if (verbose){
      cat(paste0("Presmoothing image\n"))
    }     
    run = fslsmooth(outfile, 
                    outfile = outfile, 
                    sigma = sigma,                  
                    retimg = FALSE,
                    intern = FALSE) 
    if (verbose){
      cat(paste0("Pre Smoothing Diagnostic: ", run, "\n"))
    }   
    
    if (verbose){
      cat(paste0("Remasking Smoothed Image\n"))
    }    
    run = fslmask(outfile, 
                  mask = bonemask,
                  outfile = outfile, 
                  retimg = FALSE,
                  intern = FALSE) 
    
    if (verbose){
      cat(paste0("Pre Smoothing Diagnostic: ", run, "\n"))
    }   
    
  }
  
  #### Different options for bet
  betcmd = "bet2"
  if (inskull_mesh) {
    opts = paste0(opts, " -A")
    betcmd = "bet"
  }
  betintern = TRUE
  if (verbose){
    cat(paste0("Running ", betcmd, "\n"))
    betintern = FALSE
  }  
  runbet = fslbet(infile = outfile, 
                  outfile=outfile, 
                  retimg = FALSE, intern=betintern,
                  opts = opts, betcmd = betcmd, ...)
  
  if (verbose){
    cat(paste0(betcmd, " output: ", runbet, "\n"))
  } 
  ############### 
  # Cleanup
  ##############
  if (isTRUE(inskull_mesh)){
    if (verbose){
      cat("Deleting extraneous files\n")
    } 
    ext = get.imgext()
    end.names = c("inskull_mask", "outskin_mask", "outskin_mesh",
                  "outskull_mask", "outskull_mesh",
                  "skull_mask")
    end.names = paste0(end.names, ext)
    end.names= c(end.names, 
                 "inskull_mesh.off", 
                 "outskin_mesh.off",
                 "outskull_mesh.off",
                 "mesh.vtk")
    end.names = paste(outfile, end.names, sep="_")
    rr = file.remove(end.names)
  }
  
  #####################
  # Filling in holes of the mask (like choroid plexis and CSF)
  #####################
  if (verbose){
    cat("Using fslfill to fill in any holes in mask \n")
  }
  if (is.null(maskfile)){
    maskfile = nii.stub(outfile)
    maskfile = paste0(outfile, "_Mask")
  }
  stopifnot(inherits(maskfile, "character"))
  # outmask = paste(outfile, "inskull_mask", sep="_")
  # outmask = paste0(outmask, ext)
  # file.rename(outmask, paste0(maskfile, ext))
  
  runmask = fslfill(file = outfile, bin=TRUE, 
                    outfile=maskfile, 
                    retimg=FALSE,
                    intern=FALSE)
  
  if (verbose){
    cat(paste0("fslfill output: ", runmask, "\n"))
  }  
  
  if (verbose){
    cat("Using the filled mask to mask original image\n")
  }
  res = fslmask(file=img, 
                mask=maskfile, 
                outfile=outfile, 
                retimg = retimg, 
                intern= intern)
  
  if (!keepmask){
    if (verbose){
      cat("Removing Mask File\n")
    }    
    maskfile = nii.stub(maskfile)
    maskfile = paste0(maskfile, ext)
    file.remove(maskfile)
  }
  return(res)
}


require(devtools)
install_github("muschellij2/fslr")
# CT_Skull_Strip(filename, outfile=brain_extracted_filename)