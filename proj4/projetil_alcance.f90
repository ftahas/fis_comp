!
      program projetil_alcance
!
      implicit none
!
      integer i,j,n,nt
      real*8 delt,v0,g,gamma2,theta,a,x,y,vx,vy,v,tvoo,t,pi
!
      open(10,file='projetil.in') 
!
      read(10,*)delt
      read(10,*)v0,g,gamma2
      read(10,*)a,n
!
      close(10) 
!
      open(10,file='projetil_alcance_livre.dat')    
      open(20,file='projetil_alcance_atrito.dat') 
!
      pi=DACOS(-1.0d0)
!
      do i=1,n
!
!
!
      theta=pi*a/180.0d0
!
      x=0.0d0
      y=0.0d0
      vx=v0*dcos(theta)
      vy=v0*dsin(theta)
!
      do while (y.ge.0.0d0)
         x = x + vx*delt
         vx = vx
         y = y + vy*delt
         vy = vy - g*delt
      enddo
!
      write(10,"(f5.2,f8.2)")a,x/1.0d3
!
      x=0.0d0
      y=0.0d0
      vx=v0*dcos(theta)
      vy=v0*dsin(theta)
!
      do while (y.ge.0.0d0)
         v=dsqrt(vx**2 + vy**2)
         x = x + vx*delt
         vx = vx - gamma2*v*vx*delt
         y = y + vy*delt
         vy = vy - g*delt - gamma2*v*vy*delt
      enddo
!
      write(20,"(f5.2,f8.2)")a,x/1.0d3
!
!
!
      a=a+1
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

