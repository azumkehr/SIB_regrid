      program bilinear_interpolation
        implicit none
        real testval
        testval = 0.
        print*, 'testval',testval
        call bilinear_interp(2.,3.,2.,3.,2.,3.,2.,3.,5.,6.,8.,9.,2.5,&
             2.5,testval)
        print*,'testval', testval
      end program bilinear_interpolation

      

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
