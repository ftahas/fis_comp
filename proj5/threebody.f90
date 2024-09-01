!
      program threebody
!
      implicit none
!
      integer i,j,k,nt,nb
      real*8 delt,tmin,tmax,t,pi,tau,r12,r13,r23,gm(3),x(3),y(3),vx(3),vy(3)
!
      open(10,file='threebody.in') 
!
      read(10,*)tmin,tmax,delt
      read(10,*)gm(1),gm(2),gm(3)
!
      close(10) 
!
      open(10,file='posicao1.dat') 
      open(20,file='posicao2.dat')
! 
      pi=DACOS(-1.0d0)
!
      gm=gm*4*(pi**2)
!
      do i=1,3
         vx(i)=0.0d0
         vy(i)=0.0d0
         x(i)=0.0d0
         y(i)=0.0d0
      enddo
!
!     Órbitas circulares
!
! Terra
      x(1)=1.00d0
      vy(1)=dsqrt(gm(3)/x(1))
! Júpiter
      x(2)=5.20d0
      vy(2)=dsqrt(gm(3)/x(2))
!
      nt=int((tmax-tmin)/delt)
!
      nb=3
!
      t=0.0d0
      tau=0.0d0
!
      do i=1,nt
!
         write(10,*)x(1),y(1)
         write(20,*)x(2),y(2)
!
         r12=dsqrt((x(1)-x(2))**2 + (y(1)-y(2))**2)
         r13=dsqrt((x(1)-x(3))**2 + (y(1)-y(3))**2)
         r23=dsqrt((x(2)-x(3))**2 + (y(2)-y(3))**2)
!
         r12=r12**3
         r13=r13**3
         r23=r23**3
!
!	 O Sol, partícula 3, fica parado no centro. 
!
         vx(1)=vx(1) - delt*gm(2)*(x(1)-x(2))/r12 - delt*gm(3)*(x(1)-x(3))/r13
         vx(2)=vx(2) - delt*gm(1)*(x(2)-x(1))/r12 - delt*gm(3)*(x(2)-x(3))/r23
!         vx(3)=vx(3) - delt*gm*(x(3)-x(1))/r13 - delt*gm*(x(3)-x(2))/r23
!
         vy(1)=vy(1) - delt*gm(2)*(y(1)-y(2))/r12 - delt*gm(3)*(y(1)-y(3))/r13
         vy(2)=vy(2) - delt*gm(1)*(y(2)-y(1))/r12 - delt*gm(3)*(y(2)-y(3))/r23
!         vy(3)=vy(3) - delt*gm*(y(3)-y(1))/r13 - delt*gm*(y(3)-y(2))/r23
!
         do j=1,2!3
            x(j)=x(j) + delt*vx(j)
            y(j)=y(j) + delt*vy(j)
         enddo
!
         t=t+delt
      enddo
!
!
!
      close(10) 
      close(20) 
!      close(30) 
!
      end ! Programa termina aqui !
!
!*************************************************************************

