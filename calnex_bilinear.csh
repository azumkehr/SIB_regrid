#PATH=/home/thilton/bin:/usr/local/bin:/opt/pgi/linux86-64/2013/bin:/home/thilton/bin:/usr/local/bin:/opt/pgi/linux86-64/2013/bin:/home/thilton/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games
echo "creating object file"
pgf90 -O2 -c -I/home/ecampbell_lab/thilton/local/include -c -o bilinear_calnex_days_only.o bilinear_calnex_days_only.f90
export EXECUTION_ID=1000b
export outname="calnex_bilinear.nc"
rm $outname
echo "creating xcutable file"
pgf90 -o bilinear_calnex_days_only.x bilinear_calnex_days_only.o -L/home/ecampbell_lab/thilton/local/lib -lioapi -lnetcdf -ldatetime
read -n1 -r -p "Press y to continue...    " key
./bilinear_calnex_days_only.x

