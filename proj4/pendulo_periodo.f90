!
      program pendulo_periodo
!
      implicit none
!
      integer i,j,nt,n
      real*8 t,tmin,tmax,delt,m,g,l,theta,theta0,omega,energia,r,st,pi
!
      open(10,file='pendulo.in') 
!
      read(10,*)tmin,tmax,delt
      read(10,*)m,l,g
      read(10,*)theta0
!
      close(10) 
!
      nt=int(tmax/delt)
!
      open(10,file='periodo_theta.dat')
!
      pi=DACOS(-1.0d0)
!
      do j=1,90
!
      theta0=dfloat(j)
!
      theta=pi*theta0/180.0d0
      omega=0.0d0
!
      t=tmin
!
      r=g/l
!
      do while (omega.le.0.0d0)
!
         st=DSIN(theta)
!
         omega = omega - r*st*delt
         theta = theta + omega*delt
!
         t=t + delt
!
      enddo
!
      write(10,*)j,2.0d0*(t-delt)
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

