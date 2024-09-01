!
      program twobody
!
      implicit none
!
      integer i,j,nt
      real*8 delt,tmin,tmax,t,gms,x,y,r,u,vx,vy,x0,y0,vx0,vy0,eps,pi,tau,e,a
!
      open(10,file='twobody.in') 
!
      read(10,*)tmin,tmax,delt                                                            
!
      close(10) 
!
      open(10,file='mercurio_pos_circular.dat')    
      open(20,file='mercurio_periodo_circular.dat') 
      open(30,file='mercurio_pos_eliptica.dat')    
      open(40,file='mercurio_periodo_eliptica.dat') 
!
      pi=DACOS(-1.0d0)
!
      gms=4.0d0*(pi**2)
!
      nt=int((tmax-tmin)/delt)
!
!     Semieixo maior e excentricidade
!
      a=0.390d0
      e=0.206d0
!
!     Movimento circular primeiro
!
      t=0.0d0
      tau=0.0d0
      j=0
!
      x0=a
      y0=0.0d0
      vx0=0.0d0
      vy0=dsqrt(gms/a)
!
      x=x0
      y=y0
      vx=vx0
      vy=vy0
!
      write(10,*)x,y
!
      do i=1,nt
         u=dsqrt(x**2 + y**2)
         u=u**3
!
         vx = vx - gms*x*delt/u
         x = x + vx*delt
!
         vy = vy - gms*y*delt/u
         y = y + vy*delt
!
         if ((y.lt.0.0d0).and.(j.eq.0)) then
            tau=2.0d0*t
            j=j+1
         endif
!
         t=t+delt
         write(10,*)x,y
      enddo
!
      write(20,*)tau
      write(20,*)(tau**2)/(a**3)
!
!     Movimento eliptico
!
      t=0.0d0
      tau=0.0d0
      j=0
!
      x0=(1.0d0 + e)*a
      y0=0.0d0
      vx0=0.0d0
      vy0=dsqrt(gms/a)*dsqrt((1.0d0 - e)/(1.0 + e))
!
      x=x0
      y=y0
      vx=vx0
      vy=vy0
!
      write(30,*)x,y
!
      do i=1,nt
         u=dsqrt(x**2 + y**2)
         u=u**3
!
         vx = vx - gms*x*delt/u
         x = x + vx*delt
!
         vy = vy - gms*y*delt/u
         y = y + vy*delt
!
         if ((y.lt.0.0d0).and.(j.eq.0)) then
            tau=2.0d0*t
            j=j+1
         endif
!
         t=t+delt
         write(30,*)x,y
      enddo
!
      write(40,*)tau
      write(40,*)(tau**2)/(a**3)
!
!
!
      close(10) 
      close(20) 
      close(30) 
      close(40) 
!
      end ! Programa termina aqui !
!
!*************************************************************************

