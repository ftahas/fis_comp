!
      program integra
!
      implicit none
!
      integer i,m,n
      real*8 a,b,dn,ts,ss,y
!
      open(10,file='integra.in') 
!
      read(10,*)a,b
      read(10,*)m
      read(10,*)y
!
      close(10) 
!
      open(10,file='trapezio.dat') 
      open(20,file='simpson.dat')  
!
      do i=1,m
         n=2**i
         dn=dfloat(n)
!
         call trap(a,b,n,ts)
         write(10,*)DLOG10(dn),DLOG10(DABS(ts-y))
!
         call simp(a,b,n,ss)
         write(20,*)DLOG10(dn),DLOG10(DABS(ss-y))
!
      enddo
!
      close(10)
      close(20)  
!
      end ! Prgrama termina aqui !
!
!*************************************************************************
!
      real*8 function func(x)
!
      real*8 x
!
      func=DEXP(2.0d0*x)*DCOS(x/4)
!
      return
      end
!
!
!
!*************************************************************************
!
      subroutine trap(a,b,n,s)
!
      integer i,n
      real*8 a,b,s,h,x,func
!
      h=(b-a)/dfloat(n)
!
      fa=func(a)
      fb=func(b)
!
      s=0.5d0*(fa + fb)
!
      do i=1,n-1
         x=a + i*h
         s=s + func(x)
      enddo
!
      s=h*s
!
      return
      end
!
!
!
!*************************************************************************
!
      subroutine simp(a,b,n,s)
!
      integer i,n
      real*8 a,b,s,h,x,w,func
!
      h=(b-a)/dfloat(n)
!
      fa=func(a)
      fb=func(b)
!
      s=fa + fb
!
      do i=1,n-1
         x=a + i*h
         if (mod(i,2) == 0) then
            w=2.0d0
         else
            w=4.0d0
         endif
         s=s + w*func(x)
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
