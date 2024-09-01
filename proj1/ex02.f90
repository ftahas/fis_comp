      program stirling
      implicit none
    
      integer(8)::n,Fac
      real(16)::Fac_hat,Dev,pi,x=1.d0
      pi = 4.d0*atan(1.d0)
    
      open(1, file='ex02.dat')
      Fac=1

      do n=1,20
      Fac=n*Fac
      Fac_hat=sqrt(2.d0*pi*n)*(n/exp(x))**n
      Dev=(Fac-Fac_hat)/Fac
      write(1,*) n,Fac,Fac_hat,Dev
      end do
   
      close(1)

      end program
