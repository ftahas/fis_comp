      program average
      
      implicit none
      real(8)::Geo, Arit, Dev, Soma, Prod, Soma2
      real(8), allocatable::x(:)
      integer(4)::N,i
   
      print*, 'Enter N'
      read*, N
 
      allocate(x(N))
           
      do i=1,N
         print*, 'Enter a number'
         read*, x(i)
      end do
 
      Soma=0
      do i=1, N
         Soma = x(i)+Soma
      end do
       
      Arit = Soma/N
      
      Prod=1
      do i=1,N
         Prod = x(i)*Prod
      end do

      Geo = Prod**(1d0/N)

      Soma2=0
      do i=1,N
         Soma2=(x(i)-Arit)**2d0
      end do
      
      Dev = sqrt(Soma2/(N-1)) 

      print*, 'Media aritmetica=', Arit
      print*, 'Media geometrica=', Geo
      print*, 'Desvio padrao=', Dev


 
      end program
