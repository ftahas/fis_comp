      program celsius !begins the program
      implicit none !the program will not use any implicit type

      integer*4 i,F !integer variables
      real*8 C,C_hat,Dev !real variables
      
      open(1, file='FahrenheitToCelsius.dat') !create file
      do i=1,11 !begins loop
          F=10*(i-1) 
          C=(F-32)*5/9 
          C_hat=(F-30)/2 
          Dev=abs(C-C_hat)/abs(C)
          write(1,*) F,C,C_hat,Dev !write in file
      end do
      close(1)

      end program

      
          
