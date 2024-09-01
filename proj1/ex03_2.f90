      program seno
      implicit none
  
      integer n,i,j 
      real(8) precisao,fac
      real(8), allocatable::x(:),inc(:),taylor(:)
      integer, allocatable::m(:)      

      precisao=10d0**(-6d0)

      write(*,*) 'Enter the number of values youd like &
      to calculate'
      read(*,*) n
      allocate(x(n),inc(n),taylor(n),m(n))

      do i=1,n
         write(*,*) 'Enter a value'
         read(*,*) x(i)
      end do

      taylor=0
      inc=1
      do i=1,n
         j=1
         do while (inc(i)>precisao)
            m(i)=2*j-1
            inc(i)=((-1)**(j-1))*(x(i)**m(i))/(fac(m(i)))
            taylor(i)=taylor(i)+inc(i)
            j=j+1
         end do
      end do  
     
      print*, 'x=',x
      print*,'sin(x)=',sin(x)
      print*,'taylor=',taylor
      print*,'incremento=',inc
      print*,'order=',m
 
      end program

      function fac(m)
      implicit none
      integer i,m
      real(8) mult,fac
      
      mult=1
      do i=1,m
         mult=i*mult
      end do

      fac=mult
      end function
