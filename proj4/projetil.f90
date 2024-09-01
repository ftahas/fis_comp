!
      program projetil
!
      implicit none
!
      integer i,nt
      real*8 delt,v0,g,gamma2,theta,x,y,vx,vy,v,tvoo,t,pi
!
      open(10,file='projetil.in') 
!
      read(10,*)delt
      read(10,*)v0,g,gamma2
      read(10,*)theta
!
      close(10) 
!
      open(10,file='projetil_livre.dat')    
      open(20,file='projetil_atrito.dat') 
!
      pi=DACOS(-1.0d0)
      theta=pi*theta/180.0d0
      tvoo=2.0d0*v0*dsin(theta)/g
!
      write(6,*)'Tempo de voo sem resistencia (s)'
      write(6,"(f10.3)")tvoo
!
      nt=int(tvoo/delt)
!
      x=0.0d0
      y=0.0d0
      vx=v0*dcos(theta)
      vy=v0*dsin(theta)
!
      write(10,*)x,y
!
      do i=1,nt
         x = x + vx*delt
         vx = vx
         y = y + vy*delt
         vy = vy - g*delt
         write(10,*)x/1.0d3,y/1.0d3
      enddo
!
      write(6,*)'Alcance sem resistencia (km)'
      write(6,"(f8.2)")x/1.0d3
!
      x=0.0d0
      y=0.0d0
      vx=v0*dcos(theta)
      vy=v0*dsin(theta)
!
      write(20,*)x,y
!
      do while (y.ge.0.0d0)
         v=dsqrt(vx**2 + vy**2)
         x = x + vx*delt
         vx = vx - gamma2*v*vx*delt
         y = y + vy*delt
         vy = vy - g*delt - gamma2*v*vy*delt
         write(20,*)x/1.0d3,y/1.0d3
      enddo
!
      write(6,*)'Alcance com resistencia (km)'
      write(6,"(f8.2)")x/1.0d3
!
!
!
      close(10) 
      close(20) 
!
      end ! Programa termina aqui !
!
!*************************************************************************

