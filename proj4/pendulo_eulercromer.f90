!
      program pendulo_euler
!
      implicit none
!
      integer i,nt
      real*8 t,tmin,tmax,delt,m,g,l,theta,omega,energia,r,st,pi
!
      open(10,file='pendulo.in') 
!
      read(10,*)tmin,tmax,delt
      read(10,*)m,l,g
      read(10,*)theta
!
      close(10) 
!
      nt=int(tmax/delt)
!
      open(10,file='angulo_eulercromer.dat')    
      open(20,file='energia_eulercromer.dat') 
!
      pi=DACOS(-1.0d0)
      theta=pi*theta/180.0d0
      omega=0.0d0
!
      t=tmin
!
      write(10,*)t,theta*180.0d0/pi
      write(20,*)t,energia(m,g,l,omega,theta)     
!
      r=g/l
!
      do i=1,nt
!
         st=DSIN(theta)
!
         omega = omega - r*st*delt
         theta = theta + omega*delt
!
         t=t + delt
!
         write(10,*)t,theta*180.0d0/pi
         write(20,*)t,energia(m,g,l,omega,theta)    
!
      enddo
!
!
!
      close(10) 
      close(20) 
!
      end ! Programa termina aqui !
!
!*************************************************************************
!
      real*8 function energia(m,g,l,omega,theta)
!
      real*8 m,g,l,omega,theta
!
      energia=0.5d0*m*(l**2)*(omega**2) - m*g*l*DCOS(theta)
!
      return
      end
!
!
!
!*************************************************************************

