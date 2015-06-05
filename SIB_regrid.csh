#!/bin/csh

###### STEP 1 make ioapi file. 
#make clean -f KettletoIOAPI.mk
#make -f KettletoIOAPI.mk
#	    	set infile = "kettle_GPP_raw.nc"
#	  	set outfile = "kettleflux_unit_all_and_dms.nc"

#cat > tmp.ini <<EOF
#&control
# input='./$infile'
#&end
#EOF
#${cwd}/namecut tmp.ini emis.ini
#rm -f tmp.ini 
#
#setenv OUTPUT $outfile
#if (-e $outfile) rm -f $outfile
#./KettletoIOAPI.x

############# STEP 2:calculating math http://www.cmascenter.org/ioapi/documentation/3.1/html/MTXCALC.html
######## GRID DESCRIPTIONS FILE contains description on input and output grid. See: http://www.cmascenter.org/ioapi/documentation/3.1/html/GRIDS.html
setenv GRIDDESC ${cwd}/GRIDDESC_SIB.txt

# MATRIX FILENAME FOR REGRIDDING MATH
setenv MATRIX SIB_matrix
#if(-e $MATRIX) rm -f $MATRIX

setenv  MATTXT SIB_mat_txt

#if(-e $MATTXT) rm -f $MATTXT
# GRID DIVISION SIZES FOR REGRIDDING ALGORITHM
setenv COL_REFINEMENT 1000
setenv ROW_REFINEMENT 1000

if ( (! -e $MATRIX) ||  (! -e $MATTXT)) then

${cwd}/mtxcalc << DONE
Y
SIB_raw
ARCNAGRID

DONE
#
endif

###### STEP 3: clipping to North America
# #NOTE - the blank lines between m3wndw and DONE are crucial - each one
# #seems to tell m2wndw to use the default value for something it
# #prompts the user for.  Therefore the blanks are necessary for
# #non-interactive use. ---TWH: http://www.cmascenter.org/ioapi/documentation/3.1/html/MTXCPLE.html
setenv INFILE SIB_IOAPI_RAW_1.nc
setenv OUTFILE SIB_IOAPI_RAW_CALNEX.nc
${cwd}/m3wndw << DONE




KGPPNA_EI

DONE


setenv  MATRIX_FILE $MATRIX
setenv  FRACTIONS  $MATTXT
setenv IN_DATA SIB_IOAPI_RAW_CALNEX.nc
setenv OUT_DATA SIB_IOAPI_CALNEX_R-309x201.nc
${cwd}/mtxcple << DONE
Y
NONE








DONE

###### STEP 4
# #create an the hourly GPP file from the regridded monthly data
# make clean -f interp_2_hourly.mk
# make -f interp_2_hourly.mk
# setenv OUTPUT gurney_coal_124x124_hrly.nc
# setenv INPUT gurney_coal_124x124.nc
# if (-e $OUTPUT) rm -f $OUTPUT
# ./interp_2_hourly.x

###### STEP 5
#convert GPP from per gridcell to per m2 in a new file 
#make clean -f Kettle_gpp_per_m2.mk
#make -f Kettle_gpp_per_m2.mk
#setenv INPUT GPPkettle_COS_124x124_pergrid_highAZ.nc
#setenv OUTPUT GPPkettle_GPP_124x124_m2.nc
#if (-e $OUTPUT) rm -f $OUTPUT
#./Kettle_gpp_per_m2.x
#
####### STEP 6
##calculate fCOS file from the hourly GPP
#make clean -f Kettle_gpp_2_fcos.mk
#make -f Kettle_gpp_2_fcos.mk
#setenv INPUT GPPkettle_GPP_124x124_m2.nc
#setenv OUTPUT kettle_fcos_124x124_LRU1.61_m2_az.nc
#if (-e $OUTPUT) rm -f $OUTPUT
#./Kettle_gpp_2_fcos.x

