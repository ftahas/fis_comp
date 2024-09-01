!
      program deriva
!
      implicit none
!
      integer i,id,nh
      real*8 h,hmin,hmax,dh,func,func1,func2,f1,f2,x,f1f,f1s,f2s,y
!
      open(10,file='deriva.in') 
!
      read(10,*)x
      read(10,*)nh,hmin,hmax
!
      close(10) 
!
      dh=(hmax-hmin)/dfloat(nh)
!
      open(10,file='deriva_1f.dat')    
      open(20,file='deriva_1s.dat') 
      open(30,file='deriva_2s.dat')       
!
      f1=func1(x)
      f2=func2(x)
!
      h=hmax
      do i=1,nh+1
         y=DLOG10(h)
         write(10,*)y,DLOG10(DABS(f1-f1f(x,h)))
         write(20,*)y,DLOG10(DABS(f1-f1s(x,h)))
         write(30,*)y,DLOG10(DABS(f2-f2s(x,h)))
         h=h-dh
      enddo
!
      close(10) 
      close(20) 
      close(30) 
!
      end ! Prgrama termina aqui !
!
!*************************************************************************
!
      real*8 function func(x)
!
      real*8 x
!
      func=DEXP(2.0d0*x)*DSIN(x)
!
      return
      end
!
!
!
!*************************************************************************
!
      real*8 function func1(x)
!
      real*8 x
!
      func1=DEXP(2.0d0*x)*(2.0d0*DSIN(x) + DCOS(x))
!
      return
      end
!
!
!
!*************************************************************************
!
      real*8 function func2(x)
!
      real*8 x
!
      func2=DEXP(2.0d0*x)*(3.0d0*DSIN(x) + 4.0d0*DCOS(x))
!
      return
      end
!
!
!
!*************************************************************************
!
      real*8 function f1f(x,h)
!
      real*8 x,h,func
!
      f1f=(func(x+h) - func(x))/h
!
      return
      end
!
!
!
!*************************************************************************
!
      real*8 function f1s(x,h)
!
      real*8 x,h,func
!
      f1s=(func(x+h) - func(x-h))/(2.0d0*h)
!
      return
      end
!
!
!
!*************************************************************************
!
      real*8 function f2s(x,h)
!
      real*8 x,h,func
!
      f2s=(func(x+h) - 2.0d0*func(x) + func(x-h))/(h*h)
!
      return
      end
!
!
!
!*************************************************************************
