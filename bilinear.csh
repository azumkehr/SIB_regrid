#PATH=/home/thilton/bin:/usr/local/bin:/opt/pgi/linux86-64/2013/bin:/home/thilton/bin:/usr/local/bin:/opt/pgi/linux86-64/2013/bin:/home/thilton/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games
echo "creating object file"
pgf90 -O2 -c -I/home/ecampbell_lab/thilton/local/include -c -o bilinear_regrid.o bilinear_regrid.f90

echo "creating executable file"
pgf90 -o bilinear_regrid.x bilinear_regrid.o -L/home/ecampbell_lab/thilton/local/lib -lioapi -lnetcdf -ldatetime
read -n1 -r -p "Press y to continue...    " key
./bilinear_regrid.x

