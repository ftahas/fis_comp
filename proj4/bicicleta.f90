!
      program bicicleta
!
      implicit none
!
      integer i,nt
      real*8 vterminal,v0,v,vnew,vold,delt,p,m,rho,a,t,s
      real*8, allocatable :: xv(:)
!
      open(10,file='bicicleta.in') 
!
      read(10,*)nt,delt
      read(10,*)v0,p,m,rho,a
!
      close(10) 
!
      open(10,file='bicicleta_livre.dat')    
      open(20,file='bicicleta_atrito.dat') 
!
      allocate(xv(nt+1))
!
      vold=v0
      t=0.0d0
      write(10,*)t,v0
      xv(1)=v0
!
      do i=1,nt
         v=vnew(vold,delt,p,m,0.0d0,a)
         vold=v
         t=delt*dfloat(i)
         write(10,*)t,v
         xv(i+1)=v
      enddo
!
      call simp(0.0d0,300.0d0,nt+1,xv,s)
      write(6,*)'Distancia percorrida sem atrito (km)'
      write(6,"(f8.2)")s/1000.0d0
!
!
!
      vold=v0
      t=0.0d0
      write(20,*)t,v0
      xv(1)=v0
!
      do i=1,nt
         v=vnew(vold,delt,p,m,rho,a)
         vold=v
         t=delt*dfloat(i)
         write(20,*)t,v
         xv(i+1)=v
      enddo
!
      call simp(0.0d0,300.0d0,nt+1,xv,s)
      write(6,*)'Distancia percorrida com atrito (km)'
      write(6,"(f8.2)")s/1000.0d0
!
      vterminal=(2.0d0*p/(rho*a))**(1.0d0/3.0d0)
      write(6,*)'Velocidade terminal (m/s)'
      write(6,"(f8.2)")vterminal
!
      deallocate(xv)
!
      close(10) 
      close(20) 
!
      end ! Programa termina aqui !
!
!*************************************************************************
!
      real*8 function vnew(vold,delt,p,m,rho,a)
!
      real*8 vold,delt,p,m,rho,a
!
      vnew=vold + delt*(p/(m*vold) - 0.5d0*rho*a*(vold**2)/m)
!
      return
      end
!
!
!
!*************************************************************************
!
      subroutine simp(a,b,n,f,s)
!
      integer i,n
      real*8 a,b,s,h,w,f(n)
!
      h=(b-a)/dfloat(n)
!
      fa=f(1)
      fb=f(n)
!
      s=fa + fb
!
      do i=1,n-1
         if (mod(i,2) == 0) then
            w=2.0d0
         else
            w=4.0d0
         endif
         s=s + w*f(i)
      enddo
!
      s=h*s/3.0d0
!
      return
      end
!
!
!
!*************************************************************************

