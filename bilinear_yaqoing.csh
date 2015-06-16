#PATH=/home/thilton/bin:/usr/local/bin:/opt/pgi/linux86-64/2013/bin:/home/thilton/bin:/usr/local/bin:/opt/pgi/linux86-64/2013/bin:/home/thilton/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games
echo "creating object file"
pgf90 -O2 -c -I/home/ecampbell_lab/thilton/local/include -c -o bilinear_yaqoing.o bilinear_yaqoing.f90
export EXECUTION_ID=1000b
export outname="bilinear_yaqoing.nc"
export input="/home/ecampbell_lab/WRFOUT/wrfout_d03_2015-05-04_00:00:00"
rm $outname
echo "creating xcutable file"
pgf90 -o bilinear_yaqoing.x bilinear_yaqoing.o -L/home/ecampbell_lab/thilton/local/lib -lioapi -lnetcdf -ldatetime
read -n1 -r -p "Press y to continue...    " key
./bilinear_yaqoing.x

