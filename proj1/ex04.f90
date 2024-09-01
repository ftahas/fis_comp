      program vector
      
      implicit none
      real(8)::v(2),r,theta,phi

      write(*,*) 'Enter x coordinate of the vector'
      read(*,*) v(1)
      write(*,*) 'Enter y coordinate of the vector'
      read(*,*) v(2)
      
 
      r=sqrt(v(1)**2 + v(2)**2)
      theta=atan(v(2)/v(1))

      v(1)=r; v(2)=theta 
      print*,'The vector written in polar coordinates is',v

      write(*,*) 'Enter an rotation angle in radians'
      read(*,*) phi

      theta=theta+phi
      v(1)=r*cos(theta);v(2)=r*sin(theta)
 
      print*,'The new vector in cartesian coordinates is',v
      end program 
