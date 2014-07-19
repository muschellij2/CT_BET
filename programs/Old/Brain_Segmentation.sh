basedir="/Users/$USER/Dropbox/CTR/DHanley/CT_Registration/Brain_Seg_Rerun"

cd "$basedir"
id="232-521"
# for id in `find . -maxdepth 1 -mindepth 1 -type d -exec basename '{}' \;`; 
# do
	refdir="$basedir/$id"
	cd "$refdir"
	if [ "$id" != "programs" ] && [ "$id" != "Sorted" ] && [ "$id" != "ROI_Images" ] && [ "$id" != "Original_Images" ]  
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
   		esac;
   		# exit 1;

			###### First pass is if we didn't do ``human extraction'' and just went straight to betting
			###### Human extraction - run bet on original range - no constraint on 0-100 HU - gets rid of ``room''
			###### SS - threshold human extraction to 0-100 (keeping offset of 1024) and then BET
			###### SS2 - what happens when you run bet again?
			###### No1024 - don't translate the histogram - what happens?
			human=`echo $f | awk '{ sub(/\.nii\.gz/, "_Human\.nii\.gz"); print }'`
			zeroed=`echo $f | awk '{ sub(/\.nii\.gz/, "_Zeroed\.nii\.gz"); print }'`
			j=`echo $f | awk '{ sub(/\.nii\.gz/, "_SS\.nii\.gz"); print }'`
			bbet=`echo $f | awk '{ sub(/\.nii\.gz/, "_SS2\.nii\.gz"); print }'`
			h=`echo $f | awk '{ sub(/\.nii\.gz/, "_SS_First_Pass\.nii\.gz"); print }'`
			raw=`echo $f | awk '{ sub(/\.nii\.gz/, "_SS_No1024\.nii\.gz"); print }'`
			cd "$refdir"


			### need this because then you can reorient them if you need to
			sform=`fslorient -getsformcode $f`	

			minmax=`fslstats $f -R`
			min=`echo "$minmax" | cut -d ' ' -f 1`
			max=`echo "$minmax" | cut -d ' ' -f 2`


			echo "No 1024 Bet $f file..";
			fslmaths $f -thr 0 -uthr 100 "$refdir/Skull_Stripped/$raw"

			echo "Bet 1 Running $j"
			bet2 "$refdir/Skull_Stripped/$raw" "$refdir/Skull_Stripped/$raw" -f 0.1		
				
			result=`echo "($min < 0)" | bc`
			
			if [ $result ] 
			then
				echo "Rescaling so histograms are 'zeroed'"
				### need to add this so that rest works - can't have pre-subtracted data.  Could adapt this to be anything other than 1024
				fslmaths $f -add 1024 $zeroed
				f="$zeroed" 
			fi 


			echo "No Human extraction $f file..";
			fslmaths $f -thr 1024 -uthr 1124 "$refdir/Skull_Stripped/$h"
			
			echo "Brain from Room extraction $h file..";
			bet2 "$refdir/Skull_Stripped/$h" "$refdir/Skull_Stripped/$h" -f 0.1
			fslmaths "$refdir/Skull_Stripped/$h" -thr 1024 -bin "$refdir/Skull_Stripped/$h"
			
			echo "Human Extraction $human file..";
			bet2 $f "$refdir/Skull_Stripped/$human" -f 0.1
			
			cd "$refdir/Skull_Stripped"
			echo "Thresholding to range of 1024-1124 $human file..";
			fslmaths $human -thr 1024 -uthr 1124 $human
			
			echo "Bet 1 Running $j"
			bet2 $human $j -f 0.1
			
			echo "Bet 2 Running $j"
			### this is if we want meshes
		#	bet $j $j -f 0.1 -A
			bet2 $j $bbet -f 0.1

		#	echo "Translating to 0-100 range"
			echo "Making Binary Image"
			fslmaths "$j" -thr 1024 -bin "$j"
		#	echo "Making Binary Image"
		#	fslmaths "$j" -thr 0 "$j"

			fslmaths "$bbet" -thr 1024 -bin "$bbet"
		#	fslmaths "$bbet" -thr 0 "$bbet"

			if [ $result ]
			then
				cd "$refdir"
				rm "$zeroed"
			fi 
		done;
	fi;
# done;
