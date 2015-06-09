
      program read_nc_var

         use netcdf
         implicit none
         include 'netcdf.inc'
         include 'PARMS3.EXT'
         include 'FDESC3.EXT'
         include 'IODECL3.EXT'
         character (len=*), parameter::&
           FILENAME = "/home/ecampbell_lab/ftpanon.al.noaa.gov/&
                      wrfout_d03_2010-05-25_00_R23_fpcut.nc ",&
           VARNAME1 = "XLAT",&
           VARNAME2 = "XLONG"
         INTEGER, PARAMETER::&
           NLATS=181,&
           NLONS=288,&
           NSTEPS=0
         INTEGER::ncid,varid1,varid2
         
         real,dimension(nlats,nlons)::data1,data2
         print*,"open",FILENAME
         call check(nf90_open(FILENAME,NF90_NOWRITE,ncid))
         print*,"find variable",VARNAME1
         call check(nf90_inq_varid(ncid,VARNAME1,varid1))
         print*,"find variable",VARNAME2
         call check(nf90_inq_varid(ncid,VARNAME2,varid2))
         print*,"load variable",VARNAME1
         call check(nf90_get_var(ncid,varid1,data1))
         print*,"load variable",VARNAME2
         call check(nf90_get_var(ncid,varid2,data2))
         

         PRINT*, "data1 shape:",SHAPE(data1)
         PRINT*, "data2 shape:",SHAPE(data2)
         PRINT*, 'data1 min',MINVAL(data1),'data1 max',MAXVAL(data1)
         PRINT*, 'data2 min',MINVAL(data2),'data1 max',MAXVAL(data2)


         call check(nf90_close(ncid))


         contains
          subroutine check(status)
           integer, intent ( in) :: status

           if(status /= nf90_noerr) then
            print *, trim(nf90_strerror(status))
            stop "Stopped"
           end if
          end subroutine check

          
         
      end program read_nc_var

