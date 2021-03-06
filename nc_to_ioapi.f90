      program netcdf_to_ioapi

         use netcdf
         implicit none
         include 'netcdf.inc'
         include 'PARMS3.EXT'
         include 'FDESC3.EXT'
         include 'IODECL3.EXT'

          
       !!!!!!!! SETUP VARIABLES 
         ! File to be converted
         character (len= *), parameter:: &
            FILE_NAME = &
            "/home/ecampbell_lab/SIB/flux_hourly_201005p001.nc",&
            VAR_NAME = "OCS_flux",&
            VAR_NAME2 = "lonindex",&
            VAR_NAME3 = "latindex",&
            VAR_NAME4 = "longitude",&
            VAR_NAME5 = "latitude",&
            VAR_NAME6 = "GPP",&
            VAR_NAME7 = "Rtotal",&
            VAR_NAME8 = "OCS_gpp",&
            VAR_NAME9 = "OCS_soil"
         ! Grid dimensions
         integer, parameter:: &
           nlons=288, &
           nlats=181, &
           steps=744, &
           landpoints = 12246 
         ! ID variables AND index variables
         integer::ncid,varid,varid2,varid3,varid4,varid5,varid6,varid7,&
                  varid8,varid9,i,j,t,nrows,ncols,LOGDEV,jdate,jtime
         
         ! Input Grid
         real, dimension(landpoints,steps)::data_in_ocs_flux
         real, dimension(landpoints,steps)::data_in_ocs_gpp
         real, dimension(landpoints,steps)::data_in_ocs_soil
         real, dimension(landpoints,steps)::data_in_Rtotal  
         real, dimension(landpoints,steps)::data_in_GPP     
         real, dimension(landpoints)::latindex
         real, dimension(landpoints)::lonindex
         real, dimension(nlons)::longitude
         real, dimension(nlats)::latitude
         REAL, DIMENSION(:,:,:), ALLOCATABLE :: OCS_flux
         REAL, DIMENSION(:,:,:), ALLOCATABLE :: GPP 
         REAL, DIMENSION(:,:,:), ALLOCATABLE :: OCS_gpp 
         REAL, DIMENSION(:,:,:), ALLOCATABLE :: OCS_soil
         REAL, DIMENSION(:,:,:), ALLOCATABLE :: Rtotal  
!         REAL, DIMENSION(:,:), ALLOCATABLE :: latgrid 
!         REAL, DIMENSION(:,:), ALLOCATABLE :: longrid
         REAL, landsum
         landsum = 0.0
       !!!!!!!! END VARIABLES

       !!!!!!!! READ DATA
         ! Open
         print*,"Opening file..."
         call check(nf90_open(FILE_NAME,NF90_NOWRITE,ncid))
         ! Get var id
         print*,"Getting variable info..."
         call check(nf90_inq_varid(ncid,VAR_NAME,varid))
         call check(nf90_inq_varid(ncid,VAR_NAME2,varid2))
         call check(nf90_inq_varid(ncid,VAR_NAME3,varid3))
         call check(nf90_inq_varid(ncid,VAR_NAME4,varid4))
         call check(nf90_inq_varid(ncid,VAR_NAME5,varid5))
         call check(nf90_inq_varid(ncid,VAR_NAME6,varid6))
         call check(nf90_inq_varid(ncid,VAR_NAME7,varid7))
         call check(nf90_inq_varid(ncid,VAR_NAME8,varid8))
         call check(nf90_inq_varid(ncid,VAR_NAME9,varid9))
         ! Read the data
         print*,"Reading data..."
         call check(nf90_get_var(ncid,varid,data_in_OCS_flux))
         call check(nf90_get_var(ncid,varid2,lonindex))
         call check(nf90_get_var(ncid,varid3,latindex))
         call check(nf90_get_var(ncid,varid4,longitude))
         call check(nf90_get_var(ncid,varid5,latitude))
         call check(nf90_get_var(ncid,varid6,data_in_GPP))
         call check(nf90_get_var(ncid,varid7,data_in_Rtotal))
         call check(nf90_get_var(ncid,varid8,data_in_OCS_gpp))
         call check(nf90_get_var(ncid,varid9,data_in_OCS_soil))
         ! Check the data
         print*,"Checking data..."
         print*,"shape of data",shape(data_in_OCS_flux)
         print*,"shape of lonindex",shape(lonindex)
         allocate(OCS_flux(nlons,nlats,steps))
         allocate(Rtotal(nlons,nlats,steps))
         allocate(OCS_gpp(nlons,nlats,steps))
         allocate(OCS_soil(nlons,nlats,steps))
         allocate(GPP(nlons,nlats,steps))
!         allocate(latgrid(nlons,nlats))
!         allocate(longrid(nlons,nlats))
         
         ! set grids to zeros
         do i=1,nlons
          do j=1,nlats
           do t = 1,steps
            OCS_flux(i,j,t) = 0.0
            Rtotal(i,j,t) = 0.0
            OCS_gpp(i,j,t) = 0.0
            OCS_soil(i,j,t) = 0.0
            GPP(i,j,t) = 0.0
           enddo
          enddo
         enddo
         !read in lon grids
!         do i=1,landpoints          
!           latgrid(lonindex(i),latindex(i))=latitude(latindex(i)) 
!           longrid(lonindex(i),latindex(i))=longitude(lonindex(i)) 
!         enddo
         ! read data grid
         do i=1,landpoints
          do t=1,steps
           landsum = landsum+data_in_OCS_flux(i,t)
           OCS_flux(lonindex(i),latindex(i),t) = data_in_ocs_flux(i,t)
           OCS_soil(lonindex(i),latindex(i),t) = data_in_ocs_soil(i,t)
           OCS_gpp(lonindex(i),latindex(i),t) = data_in_ocs_gpp(i,t)
           GPP(lonindex(i),latindex(i),t) = data_in_GPP(i,t)
           Rtotal(lonindex(i),latindex(i),t) = data_in_Rtotal(i,t)
          enddo
         enddo
         print*,"lanssum",landsum
         print*,"sum of grid:",sum(OCS_flux),"sum of input",&
                sum(data_in_OCS_flux)
         print*,"Closing file..."
         call check(nf90_close(ncid))
     !!!!!!!! WRITE TO IOAPI FILE 
       ! SETUP HEADER INFO
      LOGDEV = INIT3()


      ! setup the variables used to write the I/O api header
      nvars3d=5                 ! number of variables
      ftype3d=GRDDED3           ! file is in grided, Global dobson file header
      gdtyp3d= 1!Lambert                !
!      p_alp3d=90.              !unused in lat-lon
!      p_bet3d=90.              !unused in lat-lon
!      xcent3d=36.94589                !unused in lat-lon
!      ycent3d=-119.6242                !unused in lat-lon
      xorig3d=-190
      yorig3d=-90
      xcell3d=1.25
      ycell3d=1
      ncols3d=nlons
      nrows3d=nlats
      nlays3d=1                ! documentation is vague on this maybe vertical levs
      vgtyp3d=VGSGPN3           !  non-hydrostatic sigma-p vertical coordinate
      vgtop3d=1.                ! domain top in meter
      nthik3d=1
      vglvs3d(1)=1.             ! levels in meter
      units3d(1)='umol/m2/sec'
      vname3d(1) = VAR_NAME6
      vdesc3d(1) = 'GPP'
      vtype3d(1)=m3real
      vglvs3d(2)=1.             ! levels in meter
      units3d(2)='umol/m2/sec'
      vname3d(2) = VAR_NAME7
      vdesc3d(2) = 'Rtotal'
      vtype3d(2)=m3real
      vglvs3d(3)=1.             ! levels in meter
      units3d(3)='umol/m2/sec'
      vname3d(3) = VAR_NAME8
      vdesc3d(3) = 'OCS GPP'
      vtype3d(3)=m3real
      vglvs3d(4)=1.             ! levels in meter
      units3d(4)='umol/m2/sec'
      vname3d(4) = VAR_NAME9
      vdesc3d(4) = 'OCS soil flux'
      vtype3d(4)=m3real
      vglvs3d(5)=1.             ! levels in meter
      units3d(5)='umol/m2/sec'
      vname3d(5) = VAR_NAME
      vdesc3d(5) = 'OCS surface flux'
      vtype3d(5)=m3real

!      vglvs3d(2)=1.             ! levels in meter
!      units3d(2)='lon'
!      vname3d(2) = VAR_NAME4 
!      vdesc3d(2) = 'Grid longitudes'
!      vtype3d(2)=m3real
!      
!      vglvs3d(3)=1.             ! levels in meter
!      units3d(3)='lat'
!      vname3d(3) = VAR_NAME5
!      vdesc3d(3) = 'Grid latitudes'
!      vtype3d(3)=m3real
      sdate3d = 2010121
      stime3d = 000000
      tstep3d = 10000
 
! FILE CREATION SECTION:
      ! attempt to open the I/O api file. Create one if it does not exist
      print*, 'Attempting to open file...'
      if(.not.OPEN3('OUTPUT',FSRDWR3,'netcdf_to_ioap')) then ! output file does not exit
         print*, 'File does not exist, attempting file creation...'
         if(.not.OPEN3('OUTPUT',FSCREA3,'netcdf_to_ioap')) then ! FSCREA3 FSUNKN3
            print*, 'Error opening output file'
            stop
         endif
      else
         print*,'Reading from previously created file!'
         if (.not. DESC3('OUTPUT') ) then ! if exit, get information
            print*, 'Error getting info from OUTPUT nc'
            stop
         endif
      endif

! END FILE CREATION SECTION
!
! DATA WRITE SECTION
      ! Setup time and iteration information.
      jdate =sdate3d
      jtime = stime3d
      print*,jdate,jtime
      ! Attempt an I/O api data write. For whatever reason the convention is to
      !     loop through time and write one 2d grid at a time.
      do t=1,steps
       if(.not.write3('OUTPUT',vname3d(1),jdate,jtime,GPP(:,:,t))) then
          print*,'writing error'
          stop
       endif
       if(.not.write3('OUTPUT',vname3d(2),jdate,jtime,Rtotal(:,:,t))) then
          print*,'writing error'
          stop
       endif
       if(.not.write3('OUTPUT',vname3d(3),jdate,jtime,OCS_gpp(:,:,t))) then
          print*,'writing error'
          stop
       endif
       if(.not.write3('OUTPUT',vname3d(4),jdate,jtime,OCS_soil(:,:,t))) then
          print*,'writing error'
          stop
       endif
       if(.not.write3('OUTPUT',vname3d(5),jdate,jtime,OCS_flux(:,:,t))) then
          print*,'writing error'
          stop
       endif
       print*, jdate,jtime
       jtime=jtime+tstep3d
       if (jtime.eq.24000) then
         jtime = 000000
         jdate = jdate+1
       endif
      enddo

! Close IOAPI
      print*, 'SHUT3()=',SHUT3()
!!!!!!!!!!!end ioapi

       !!!!!!!! Checks for errors when reading netcdf
         contains
          subroutine check(status)
           integer, intent ( in) :: status
    
           if(status /= nf90_noerr) then 
            print *, trim(nf90_strerror(status))
            stop "Stopped"
           end if
          end subroutine check 
      end program netcdf_to_ioapi

      subroutine sumgrid(grid,nlats,nlons,steps)
       implicit none
       integer::nlats,nlons,steps,i,j,t
       REAL, DIMENSION(nlons,nlats,steps):: grid
       real::grid_sum
       grid_sum = 0.0
       do i=1,nlons
        do j=1,nlats
         do t=1,steps
          grid_sum = grid_sum+grid(i,j,t)
         enddo
        enddo
       enddo 
       print*,"grid_sum:",grid_sum
      end subroutine sumgrid



