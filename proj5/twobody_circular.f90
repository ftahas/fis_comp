!
      program twobody
!
      implicit none
!
      integer i,j,nt,n,k
      parameter (n=8)
      real*8 delt,tmin,tmax,t,gms,x,y,r,u,vx,vy,x0,y0,vx0,vy0,eps,pi,tau,tauexato
      real*8 raio(n)
!
      open(10,file='twobody.in') 
!
      read(10,*)tmin,tmax,delt
!
      close(10) 
!
      open(10,file='periodo_circular.dat')    
      open(20,file='kepler3_circular.dat') 
!
      pi=DACOS(-1.0d0)
!
      gms=4.0d0*(pi**2)
!
      nt=int((tmax-tmin)/delt)
!
!     raio médio da tabela I.
!
      raio(1)=0.39d0
      raio(2)=0.72d0
      raio(3)=1.00d0
      raio(4)=1.52d0
      raio(5)=5.20d0
      raio(6)=9.24d0
      raio(7)=19.19d0
      raio(8)=30.06d0
!
!     Calculamos a velocidade da orbita assumindo uma orbita circular com o raio médio da tabela I.
!
      do k=1,n
!
      t=0.0d0
      tau=0.0d0
      j=0
!
      x0=raio(k)
      y0=0.0d0
      vx0=0.0d0
      vy0=dsqrt(gms/x0)
!
      tauexato=2.0d0*pi*x0/vy0
!
      x=x0
      y=y0
      vx=vx0
      vy=vy0
!
      do while (y >= 0.0d0)
         u=dsqrt(x**2 + y**2)
         u=u**3
!
         vx = vx - gms*x*delt/u
         x = x + vx*delt
!
         vy = vy - gms*y*delt/u
         y = y + vy*delt
!
         t=t+delt
      enddo
!
!     Determine o período
!
      tau=2.0d0*(t-delt)
!
      write(10,*)k,tau,tauexato
      write(20,*)k,(tau**2)/(x0**3)

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

