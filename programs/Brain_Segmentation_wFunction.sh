rootdir="/Users/$USER/Dropbox/CTR/DHanley/CT_Registration"
basedir="${rootdir}/Brain_Seg_Rerun"
progdir="${rootdir}/programs"


listnotcontains() {
  for word in $1; do
    [[ $word = $2 ]] && return 1
  done
  return 0
}
list="programs Sorted ROI_Images Original_Images results Skull_Strip_Paper"


cd "$basedir"
id="100-4003"
for id in `find . -maxdepth 1 -mindepth 1 -type d -exec basename '{}' \;`; 
 do
    refdir="$basedir/$id"
    cd "$refdir"
    if listnotcontains "$list" "$id";
    then 
        mkdir "$refdir/Skull_Stripped"

        ###f="205-519_20110630_0633_3.nii.gz"
        #f="205-509_20100418_1312_2.nii.gz"
        # shopt -s extglob;
        ## exclude ROIs
        for f in *.nii.gz; 
        do
        case "$f" in
           *ROI* ) continue;;
           *Zeroed* ) continue;;
        esac;
           # exit 1;
        # sh "${progdir}/Brain_Seg_Function.sh" -i "$f" -o "$refdir/Skull_Stripped" -f 0.1 -a 1 
        # sh "${progdir}/Brain_Seg_Function.sh" -i "$f" -o "$refdir/Skull_Stripped" -f 0.1 -r
        sh "${progdir}/Brain_Seg_Function.sh" -i "$f" -o "$refdir/Skull_Stripped" -f 0.35 -a 1 -r
        # sh "${progdir}/Brain_Seg_Function.sh" -i "$f" -o "$refdir/Skull_Stripped" -f 0.35 -r
        sh "${progdir}/Brain_Seg_Function.sh" -i "$f" -o "$refdir/Skull_Stripped" -f 0.7 -a 1 -r
        # sh "${progdir}/Brain_Seg_Function.sh" -i "$f" -o "$refdir/Skull_Stripped" -f 0.7 -r
        done;
    fi;
done;


