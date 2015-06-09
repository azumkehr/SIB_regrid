      program bilinear_interpolation
        implicit none
        INTEGER, PARAMETER::&
         NROWS= 181,&
         NCOLS= 288,&
         NSTEPS= 744,&
         NLANDPOINTS=12246,&
         OUT_NROWS = 309,&
         OUT_NCOLS = 201 ,&
         OUT_NSTEPS = NSTEPS 
        real, dimension(ncols,nrows,nsteps)::VAR_DATA
        real, dimension(OUT_NCOLS,OUT_NROWS,OUT_NSTEPS)::OUTGRID
        real, dimension(ncols,nrows)::inlats, inlons !2d arrays of lats and lons
        real, dimension(out_ncols,out_nrows)::latgrid, longrid !2d arrays of lats and lons
        integer::i,j  

        character(len=255)::FILENAME
        character(len=255)::VARNAME



        FILENAME="/home/ecampbell_lab/SIB/flux_hourly_201005p001.nc"
        VARNAME="OCS_flux"
        !call bilinear_interp(2.,3.,2.,3.,2.,3.,2.,3.,5.,6.,8.,9.,2.5,&
        !     2.5,testval)
       call get_sib_variable(FILENAME,VARNAME,VAR_DATA,inlats,inlons)  
       print*,"SHAPE: ",shape(VAR_DATA)
       print*,"SUM: ", sum(VAR_DATA)
       print*,"MIN: ", minval(VAR_DATA)
       print*,"MAX: ", maxval(VAR_DATA)
       print*,"minlat",minval(inlats),"maxlat",maxval(inlats)
       print*,"minlon",minval(inlons),"maxlon",maxval(inlons)


       call make_out_grids(outgrid,latgrid,longrid,NSTEPS)


      end program bilinear_interpolation

      
      ! computes linear interpolation
      subroutine linear_interp(x0,x1,y0,y1,x,y)
       implicit none
       real::&
         x0,&! first position
         x1,&! second position
         x,&! desired position
         y0,&!value at position x0
         y1,&!value at position x1
         y!desired value
       print*,x0,x1,x,y0,y1
       y = y0+(y1-y0)*((x-x0)/(x1-x0))
       print*,'interpanswer:',y
       return      

      end subroutine linear_interp
     
      ! Computes a bilinear interplotion
      subroutine bilinear_interp(x0,x1,x2,x3,y0,y1,y2,y3,z0,z1,z2,z3,&
          x,y,z)
       implicit none
       real::&
         x0,&! first position
         x1,&! second position
         x2,&! ithird position
         x3,&! fourth position
         x,&!  desired position
         y0,&! y pos 0
         y1,&! y osition 1
         y2,&! y  position 2
         y3,&! y position 4
         temp_z1,&! intermediate interpolation value 1
         temp_z2,&! intermediate interpolation value 2
         y,&!desired yposition
         z0,& !value at x0,y0
         z1,& !value at x1,y1
         z2,& !value at x2,y2
         z3,& !value at x3,y3
         yave1,&! average of y values 0,2
         yave2,&! average of y values 1,3
         ztemp,&! final value
         z
       z = 0.0
       yave1 = (y0+y2)/2.0
       yave2 = (y1+y3)/2.0
       print*, 'y',y
       print*,'yave1',yave1
       print*,'yave2',yave2
       call linear_interp(x0,x1,z0,z1,x,temp_z1)
       print*,'temp_z1',temp_z1
       call linear_interp(x2,x3,z2,z3,x,temp_z2)
       print*,'temp_z2',temp_z2
       print*,'y',y
       call linear_interp(yave1,yave2,temp_z1,temp_z2,y,ztemp)
       print*, ztemp
       z = ztemp
       return

      end subroutine bilinear_interp
      
      ! Gets the data of VARNAME from a net cdf file. Requires that VAR_DATA
      ! Is of the correct dimensions. must know dimensions 
      subroutine get_sib_variable(FILENAME,VARNAME,VAR_DATA,inlats,&
                 inlons)
       use netcdf
       implicit NONE
       include 'netcdf.inc'
       include 'PARMS3.EXT'
       include 'FDESC3.EXT'
       include 'IODECL3.EXT'

       INTEGER, PARAMETER::&
         NROWS= 181,&
         NCOLS= 288,&
         NSTEPS= 744,&
         NLANDPOINTS=12246 
       REAL, PARAMETER::&
         XCELL = 1.25,&
         YCELL = 1.0

       character (len=255)::FILENAME! NC PATH/NAME
       character (len=255)::VARNAME! Desired Variable Name
       character (len=*),parameter::&
         latvar="latindex",&
         lat_name="latitude",&
         lonvar="lonindex",&
         lon_name="longitude"
       integer::i,j,t! indexes for iterating
       integer::ncid,varid,latid,lonid,lonid2,latid2!id for nc reading
       real, dimension(ncols,nrows,nsteps)::VAR_DATA !data array
       real, dimension(NLANDPOINTS,NSTEPS)::buffer
       integer, dimension(nlandpoints)::latindex
       real,dimension(181)::latitude
       integer, dimension(nlandpoints)::lonindex
       real,dimension(288)::longitude
       real,dimension(ncols,nrows)::inlats,inlons
       real::templat,templon
       
       print*,"Open: ",FILENAME
       call check(nf90_open(FILENAME,NF90_NOWRITE,ncid))

       ! Read in index data
       print*,"Find variable: ",latvar
       call check(nf90_inq_varid(ncid,latvar,latid))
       print*,"Load variable: ",latvar
       call check(nf90_get_var(ncid,latid,latindex))
       print*,"Find variable: ",lonvar
       call check(nf90_inq_varid(ncid,lonvar,lonid))
       print*,"Load variable: ",lonvar
       call check(nf90_get_var(ncid,lonid,lonindex))

       !read latitude/longitude data
       print*,"Find variable: ",lat_name
       call check(nf90_inq_varid(ncid,lat_name,latid2))
       print*,"Load variable: ",lat_name
       call check(nf90_get_var(ncid,latid2,latitude))
       print*,"Find variable: ",lon_name
       call check(nf90_inq_varid(ncid,lon_name,lonid2))
       print*,"Load variable: ",lon_name
       call check(nf90_get_var(ncid,lonid2,longitude))
       
       ! Read actual data
       print*,"Find variable: ",VARNAME
       call check(nf90_inq_varid(ncid,VARNAME,varid))
       print*,"Load variable: ",VARNAME
       call check(nf90_get_var(ncid,varid,buffer))

       call check(nf90_close(ncid))
       print*,"File closed."
 
       ! load longitude and latitude arrays
       templat = -90.
       templon = -180.
       do i = nrows, 1,-1
        do j = 1, ncols
         inlats(j,i) = templat
         inlons(j,i) = templon
         templon=templon+xcell
        enddo
        templat=templat+ycell
        templon = -180.
       enddo
       
!       do i=1,ncols
!        print*,inlats(i,nrows)
!        inlats(i,nrows)=-90.
!       enddo
       !Zero the input array:
       do i=1,ncols
        do j=1,nrows
         do t = 1,nsteps
          VAR_DATA(j,i,t) = 0.0
         enddo
        enddo
       enddo

       !reshape the data from the buffer into VAR_DATA
       do i=1,nlandpoints
        do t=1,nsteps
         !landsum = landsum+data_in_OCS_flux(i,t)
         VAR_DATA(lonindex(i),latindex(i),t) = BUFFER(i,t)
        enddo
       enddo
       

       
       
       return
      end subroutine get_sib_variable   

      subroutine check(status)
       use netcdf
       integer, intent ( in) :: status

       if(status /= nf90_noerr) then
        print *, trim(nf90_strerror(status))
        stop "Stopped"
       end if
      end subroutine check
      
 
      ! Creates an output grid of zeros, makes lat and lon grids from nc.
      subroutine make_out_grids(outgrid,latgrid,longrid,NSTEPS)
       use netcdf
       implicit none
       INTEGER, PARAMETER::&
        NROWS = 309,&
        NCOLS = 201
       integer::nsteps,ncid,latid,lonid,i,j,t

       real, dimension(ncols,nrows,nsteps)::outgrid
       real, dimension(ncols,nrows)::latgrid,longrid
       do i=1,ncols
        do j= 1,nrows
         do t = 1,NSTEPS
          outgrid(i,j,t) = 0
         enddo
        enddo
       enddo

       call check(nf90_open("/home/ecampbell_lab/ftpanon.al.noaa.gov/&
            wrfout_d03_2010-05-25_00_R23_fpcut.nc",NF90_NOWRITE,ncid))
       print*,"Load variable: ","XLON"
       call check(nf90_inq_varid(ncid,"XLONG",lonid))
       call check(nf90_inq_varid(ncid,"XLAT",latid))
       call check(nf90_get_var(ncid,lonid,latgrid))
       call check(nf90_get_var(ncid,latid,longrid))
 

       call check(nf90_close(ncid))
       print*, latgrid
       print*, longrid
      end subroutine make_out_grids

       

