      program series
      implicit none
  
      integer n,i,m,j
      real(16) incremento,precisao,x,dev,taylor      

      precisao=10d0**(-6d0)
      write(*,*) 'Enter x'
      read(*,*) x

      taylor=0d0
      !dev=abs(sin(x)-taylor)/abs(sin(x))
      !dev=1d0
      j=0
      incremento=1d0   
      print*,fac(13),fac(14),fac(15),fac(16)      
      do while (abs(incremento).gt.precisao)
            j=j+1
            m=2*j-1
            print*,'order=',m
            incremento=((-1)**(j+1))*(x**m)/(fac(m))
            taylor=taylor+incremento
            print*,'taylor=',taylor
            !dev=abs(sin(x)-taylor)/abs(sin(x))
            print*,'incremento=',incremento
      end do
     
      print*, 'sin(x)=',sin(x),'expansion=',taylor,&
              'incremento=',incremento,'order=',m
            
      contains

      function fac(m)
      integer i,m
      real(16) mult,fac
      
      mult=1
      do i=1,m
         mult=i*mult
      end do

      fac=mult
      end function fac

      end program
