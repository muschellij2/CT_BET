#!/bin/bash
# infile="Head_Image_1.nii.gz"
# usage: ct_ss infile outfile
function ct_ss() {
    infile="${1}"
    intensity=0.01
    outfile="${2}"
    if [[ -z "${outfile}" ]]; then
        outfile=`echo "${infile}" | awk '{ gsub(/\\.nii/, "_SS.nii"); print }';`
    fi

    tmpfile=`mktemp`

    # Thresholding Image to 0-100
    fslmaths "${infile}" -thr 0.000000 -uthr 100.000000  "${outfile}" 
    # Creating 0 - 100 mask to remask after filling
    fslmaths "${outfile}" -bin "${tmpfile}"; 
    fslmaths "${tmpfile}.nii.gz" -bin -fillh "${tmpfile}" 
    # Presmoothing image
    fslmaths "${outfile}"  -s 1 "${outfile}"; 
    # Remasking Smoothed Image
    fslmaths "${outfile}" -mas "${tmpfile}"  "${outfile}" 
    # Running bet2
    bet2 "${outfile}" "${outfile}" -f ${intensity} -v 
    # Using fslfill to fill in any holes in mask 
    fslmaths "${outfile}" -bin -fillh "${outfile}_Mask" 
    # Using the filled mask to mask original image
    fslmaths "${infile}" -mas "${outfile}_Mask"  "${outfile}" 
    echo "${outfile}"
}

### new implementation (coming soon)
# CT Skull Stripping with FSL

# Usage:
#   ct_ss --infile=<infile> [--outfile=<outfile>]
#   ct_ss -i <infile> [-o <outfile>]
#   ct_ss -h | --help

# Options:
#   -h --help     Show this screen.
#   -i --infile=<infile>  Input filename
#   -o --outfile=<outfile>      Output filename
#   