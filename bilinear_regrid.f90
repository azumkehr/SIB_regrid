      program bilinear_interpolation
        implicit none
        include 'netcdf.inc'
        include 'PARMS3.EXT'
        include 'FDESC3.EXT'
        include 'IODECL3.EXT'

        INTEGER, PARAMETER::&
         NROWS= 181,&
         NCOLS= 288,&
         NSTEPS= 744,&
         NLANDPOINTS=12246,&
         OUT_NROWS = 309,&
         OUT_NCOLS = 201 ,&
         OUT_NSTEPS = NSTEPS 
!        real, dimension(ncols,nrows,nsteps)::in_cos_flux
        real, dimension(ncols,nrows,nsteps)::in_cos_flux
        real, dimension(OUT_NCOLS,OUT_NROWS,nsteps)::OCS_FLUX
        real, dimension(ncols,nrows)::inlats, inlons !2d arrays of lats and lons
        real, dimension(out_ncols,out_nrows)::latgrid, longrid !2d arrays of lats and lons
        real,dimension(out_ncols,out_nrows)::write_buffer
        integer::i,j,t,i1,i2,i3,i4,j1,j2,j3,j4,LOGDEV,day,hour,jdate,jtime
        real::x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
        real::d,lat,lon,z! calculated distance between 2 coordinates.
        character(len=255)::FILENAME
        character(len=255)::VARNAME
        character(len=255)::outname
        


        FILENAME="/home/ecampbell_lab/SIB/flux_hourly_201005p001.nc"
        VARNAME="OCS_flux"
!        call bilinear_interp(2.,3.,2.,3.,2.,3.,2.,3.,5.,6.,8.,9.,2.5,&
!             2.5,z)
!        print*,z
        PRINT*,"STEP 1"
        call get_sib_variable(FILENAME,VARNAME,in_cos_flux,inlats,inlons)  
        print*,"in_cos_flux",sum(in_cos_flux),shape(in_cos_flux)
        print*,"lats",minval(inlats),maxval(inlats)
        print*,"lons",minval(inlons),maxval(inlons)
!        call make_out_grids(OCS_FLUX,latgrid,longrid,NSTEPS)
        PRINT*,"STEP 2"
        call make_out_grids(OCS_FLUX,latgrid,longrid,nsteps)
        call find_nearest4(43.5,-120.5,inlats,inlons,nrows,ncols,i1,i2,i3,i4,j1,j2,j3,j4)
        print*,i1,j1,i3,i4,j1,j2,j3,j4
        print*,""
        print*,"input: ",-120.5,43.5
        print*,"closest: ",inlons(i1,j1),inlats(i1,j1)
        print*,"closest: ",inlons(i2,j2),inlats(i2,j2)
        print*,"closest: ",inlons(i3,j3),inlats(i3,j3)
        print*,"closest: ",inlons(i4,j4),inlats(i4,j4)
        ! Loop through output grid and calculate bilinear values
        ! loop through time also because input data has different time values.
        PRINT*,"STEP 3"
        print*,"shape of ocs flux",shape(ocs_flux)
        do i = 1, OUT_NCOLS
         do j = 1, OUT_NROWS
           lat = latgrid(i,j)
           lon = longrid(i,j)
          call find_nearest4(lat,lon,inlats,inlons,nrows,ncols,&
                i1,i2,i3,i4,j1,j2,j3,j4)
          do t = 1, NSTEPS
!           PRINT*, "STEP 4"
           ! Get longitudes for interpolation 
           x1 = inlons(i1,j1)
           x2 = inlons(i2,j2)
           x3 = inlons(i3,j3)
           x4 = inlons(i4,j4)
           ! Get latitudes for interpolation
           y1 = inlats(i1,j1)
           y2 = inlats(i2,j2)
           y3 = inlats(i3,j3)
           y4 = inlats(i4,j4)
           ! GEt data for interpolation
           z1 = in_cos_flux(i1,j1,t)
           z2 = in_cos_flux(i2,j2,t)
           z3 = in_cos_flux(i3,j3,t)
           z4 = in_cos_flux(i4,j4,t)
         !  if (z1.ne.0.0) then
         !   print*,"testing interp input", x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
         !  endif
           ! Perform interpolation to calculate value of OCS_FLUX as 'z'
           call bilinear_interp(x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,&
             lon,lat,z)
           OCS_FLUX(i,j,t)=z
          enddo
         enddo
        enddo
!        print*,"first layer",OCS_FLUX
        print*,"max",maxval(OCS_FLUX),",min",minval(ocs_flux)

      LOGDEV = INIT3()




      PRINT*,'STEP4'
      ! Write to ioapi
      nvars3d=1                 ! number of variables
      ftype3d=GRDDED3           ! file is in grided, Global dobson file header
      gdtyp3d= 2!Lambert                !
!      p_alp3d=90.              !unused in lat-lon
!      p_bet3d=90.              !unused in lat-lon
      xcent3d=36.94589                !unused in lat-lon
      ycent3d=-119.6242                !unused in lat-lon
      xorig3d=-122.0788
      yorig3d=30.65546
      xcell3d=4000.
      ycell3d=4000.
      ncols3d=out_ncols
      nrows3d=out_nrows
      nlays3d=1                ! documentation is vague on this maybe vertical levs
      vgtyp3d=VGSGPN3           !  non-hydrostatic sigma-p vertical coordinate
      vgtop3d=1.                ! domain top in meter
      nthik3d=1
      vglvs3d(1)=1.             ! levels in meter
      units3d(1)='umol/m2/sec'
      vname3d(1) = 'OCS_FLUX'
      vdesc3d(1) = 'OCS FLUXES REGRIDDED FROM SIB'
      vtype3d(1)=m3real

      sdate3d = 2010121
      stime3d = 000000
      tstep3d = 010000

       
      print*, 'Attempting to open file...'
      if(.not.OPEN3('outname',FSRDWR3,'gen_bdv')) then ! output file does not exit
         print*, 'File does not exist, attempting file creation...'
         if(.not.OPEN3('outname',FSCREA3,'gen_bdv')) then ! FSCREA3 FSUNKN3
            print*, 'Error opening output file'
            stop
         endif
      else
         print*,'Reading from previously created file!'
         if (.not. DESC3('outname') ) then ! if exit, get information
            print*, 'Error getting info from OUTPUT nc'
            stop
         endif
      endif

! DATA WRITE SECTION
      ! Setup time and iteration information.
      day = 121
      hour = 0
      jdate =sdate3d
      jtime = stime3d
      print*,jdate,jtime
      ! Attempt an I/O api data write. For whatever reason the convention is to
      !     loop through time and write one 2d grid at a time.
      do t=1,nsteps
!       do i = 1,out_ncols
!        do j = 1, out_nrows
!          write_buffer(i,j) = OCS_FLUX(i,j,t)
!        enddo
!       enddo
       !print*,"sum",sum(write_buffer),sum(OCS_FLUX),i,j,t
        print*,"shape of ocs flux",shape(ocs_flux(:,:,1))
       if(.not.write3('outname',vname3d(1),jdate,jtime,OCS_FLUX(:,:,t))) then
          print*,'writing error'
          stop
       endif
       print*, jdate,jtime
       call nexttime(jdate,jtime,tstep3d, day, hour)
      enddo


      print*, 'SHUT3()=',SHUT3()

      end program bilinear_interpolation

      subroutine nexttime(jdate,jtime,tstep3d,day,hour)
       implicit none
       integer::jtime,jdate,tstep3d,day,hour
       hour = hour+1
       if (hour.eq.24)then
        day = day+1
        hour = 0
       endif
       jtime = hour*10000  
       jdate = 2010*1000+day
       print*, "new jtime", jtime
       print*, "new jdate", jdate
       return
      end subroutine nexttime

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
       if (x0.eq.x1) then
        y = (y0+y1)/2.
       else
        y = y0+(y1-y0)*((x-x0)/(x1-x0))
       endif
       !print*,'interpanswer:',y
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
!       print*, "enter bilinear_interp()"
!       if ((z0.ne.0.0).and.(z1.ne.0.0).and.(z2.ne.0.0).and.(z3.ne.0.0))then
!        print*,"input: ",z0,z1,z2,z3
!        print*,"answer: ",z
!       endif
       z = 0.0
       !print*,"y's",y0,y1,y2,y3
       !yave1 = (y0+y2)/2.0
       !yave2 = (y1+y3)/2.0
       call linear_interp(x0,x1,z0,z1,x,temp_z1)
       call linear_interp(x2,x3,z2,z3,x,temp_z2)
       call linear_interp(y0,y2,temp_z1,temp_z2,y,ztemp)
       
       z = ztemp
!       print*,"exit bilinear_interp()"
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
!       print*,"enter get_sib_variables()"
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
       print*, "cells xy",xcell,ycell
       do i = 1, ncols
        do j = 1 ,nrows
         inlats(i,j) = templat
         inlons(i,j) = templon
         templat=templat+ycell
        enddo
        templon=templon+xcell
        templat = -90.
       enddo
      
       !Zero the input array:
       do i=1,ncols
        do j=1,nrows
         do t = 1,nsteps
          VAR_DATA(i,j,t) = 0.0
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
       print*, minval(latindex),maxval(latindex)
       print*, minval(lonindex),maxval(lonindex)
       do i=36,174
         inlats(lonindex(i),latindex(i)) = latitude(latindex(i))
       enddo
       do i = 1,288
         inlons(lonindex(i),latindex(i)) = longitude(lonindex(i))
       enddo
       print*,minval(lonindex),maxval(lonindex)
       print*,minval(latindex),maxval(latindex) 
       print*,"inlat range",minval(inlats),maxval(inlats)
       print*,"inlon range",minval(inlons),maxval(inlons)
       print*,"latrange",minval(latitude),maxval(latitude)
       print*,"lonrange",minval(longitude),maxval(longitude)
       print*, longitude(2),inlons(2,2)
       print*, latitude(2),inlats(2,2)
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
       print*, "enter make_out_grids()"
       do i=1,ncols
        do j= 1,nrows
         do t = 1,NSTEPS
          outgrid(i,j,t) = 0
         enddo
        enddo
       enddo
       ! read lat lon information from the original nc file.
       call check(nf90_open("/home/ecampbell_lab/ftpanon.al.noaa.gov/&
            wrfout_d03_2010-05-25_00_R23_fpcut.nc",NF90_NOWRITE,ncid))
       print*,"Load variable: ","XLON"
       call check(nf90_inq_varid(ncid,"XLONG",lonid))
       print*,"Load variable: ","XLAT"
       call check(nf90_inq_varid(ncid,"XLAT",latid))
       call check(nf90_get_var(ncid,latid,latgrid))
       call check(nf90_get_var(ncid,lonid,longrid))
       print*,"cat",latgrid(1,1)
       print*,"cat",latgrid(2,2)
       print*,"dog",longrid(1,1)
       print*,"dog",longrid(2,2)
       print*,"variables loaded successfully"
       call check(nf90_close(ncid))
      end subroutine make_out_grids

      !Calulate distance 
      subroutine haversine(lat1,lon1,lat2,lon2,d)
       implicit none
       !INPUT: 
       !  lat1, lon1 = coordinates of first point.
       !  lat2, lon2 = coordinates of second point.
       !  distance = varable for returning calculated distance.
       !OUTPUT:
       !  Distance in km
       real,intent(IN)::lat1,lon1,lat2,lon2
       real,radlat1,radlon1,radlat2,radlon2
       real::d
       real::dLat,dLon,c,a! intermediate values
       real,parameter::R = 6371.0!readius of the erath km
       ! Convert to radians
       radlat1 = lat1 * 0.0174532925
       radlat2 = lat2 * 0.0174532925
       radlon1 = lon1 * 0.0174532925
       radlon2 = lon2 * 0.0174532925

       
       dLat = (radlat2-radlat1)
       dLon = (radlon2-radlon1)
       a=(sin(dLat/2.)**2. + cos(radlat1)*cos(radlat2)*(sin(dLon/2.))**2.)
       c = 2.*atan2(sqrt(a),sqrt(1.-a))
       d = R*c
!       print*,"dlat", dLat
!       print*,"dLon",dLon
!       print*,"a",a
!       print*,"c",c
!       print*,"d",d
       return
      end subroutine haversine
      
      ! finds the indecies of the 4 nesrest points to lat,lon
      subroutine find_nearest4(lat,lon,inlats,inlons,nrows,ncols,&
                 i1,i2,i3,i4,j1,j2,j3,j4)
       implicit none
       real::lat,lon !input lat and lon
       integer::tempi,tempj,i1,i2,i3,i4,j1,j2,j3,j4!output index. i for hoizontal j for vert.
       real::ld,d
       real,dimension(ncols,nrows)::inlats,inlons!lat and lon grids of input files.
       integer::i,j,nrows, ncols,case_number
!       print*,"enter find_nearest4()"
       ld = 9999999.0
       do i = 1, ncols
!        if (abs(inlons(i,1)-lon).lt.10.0)then
        do j = 1, nrows
          call haversine(lat,lon,inlats(i,j),inlons(i,j),d)
          if (d.lt.ld) then
           ld = d
           tempi = i
           tempj = j
          endif
         enddo
!        endif
       enddo
       !note that the lats from inlat are flipped vertically so j values are intuitively backwards.
       if ((lat<inlats(tempi,tempj)).and.(lon<inlons(tempi,tempj))) then
          case_number = 1
          i1 = tempi - 1
          i2 = tempi
          i3 = tempi - 1
          i4 = tempi
          j1 = tempj
          j2 = tempj
          j3 = tempj - 1
          j4 = tempj - 1
       else if ((lat>inlats(tempi,tempj)).and.(lon<inlons(tempi,tempj))) then
          case_number = 2
          i1 = tempi - 1 
          i2 = tempi
          i3 = tempi - 1
          i4 = tempi
          j1 = tempj + 1
          j2 = tempj + 1 
          j3 = tempj
          j4 = tempj
       else if ((lat<inlats(tempi,tempj)).and.(lon>inlons(tempi,tempj))) then
          case_number = 3
          i1 = tempi  
          i2 = tempi + 1
          i3 = tempi 
          i4 = tempi + 1
          j1 = tempj 
          j2 = tempj  
          j3 = tempj - 1
          j4 = tempj - 1
       else if((lat>inlats(tempi,tempj)).and.(lon>inlons(tempi,tempj))) then
          case_number = 4
          i1 = tempi  
          i2 = tempi + 1
          i3 = tempi 
          i4 = tempi + 1
          j1 = tempj + 1
          j2 = tempj + 1 
          j3 = tempj 
          j4 = tempj 
       else
          case_number = 5
          i1 = tempi - 1
          i2 = tempi
          i3 = tempi - 1
          i4 = tempi
          j1 = tempj
          j2 = tempj
          j3 = tempj - 1
          j4 = tempj - 1
          
       endif
       !print*,"case",case_number
      return
      end subroutine find_nearest4
