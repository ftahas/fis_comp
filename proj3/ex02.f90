   program ex02
 
     implicit none
      real(8) h,f,a,b,integral,trapezio,simpson,hmax,hmin,dh,table(14,2),hh(14)
      integer i,j,N,nh,k,nnh,NN(14)

       hmax=1.d-1;hmin=1.d-5;nh=10000;nnh=14
 
       dh=(hmax-hmin)/nh

       a=0;b=1

      open(10,file='integral.dat')
      open(1,file='trapezio.dat');open(2,file='simpson.dat')

      do i=1,13
        N=2**i
        h=1/dfloat(N)
        write(10,*)DABS(integral(a,b)-trapezio(a,b,N)),DABS(integral(a,b)-simpson(a,b,N))
        write(1,*)DLOG10(h),DLOG10(DABS(integral(a,b)-trapezio(a,b,N)))
        write(2,*)DLOG10(h),DLOG10(DABS(integral(a,b)-simpson(a,b,N)))
      end do                    


     !h=hmax
     !N=(b-a)/h
     !do i=1,nh
      !  write(1,*)DLOG10(h),DLOG10(DABS(integral(a,b)-trapezio(a,b,N)))
       ! write(2,*)DLOG10(h),DLOG10(DABS(integral(a,b)-simpson(a,b,N)))
        !h=h-dh
        !N=(b-a)/h     
     !end do     

  close(10);close(1);close(2)

  end program



   real(8) function f(x)
   real(8) x

   f=DEXP(2*x)*DCOS(x/4)

   end function



   real(8) function integral(a,b)
     
      real(8) a,b,f

      integral = 32*(f(b)-f(a))/65 + 4*(DEXP(2*b)*DSIN(b/4)-DEXP(2*a)*DSIN(a/4))/65
   end function



  real(8) function trapezio(a,b,N)
   
   real(8) a,b,h,soma,f
   integer i,N
     
     h=(b-a)/N
     
     soma=0
     do i=1,N-1
      soma = soma+f(a+i*h)
     end do  
   
     trapezio=h*(0.5*f(a)+soma+0.5*f(b))
      
   end function



  real(8) function simpson(a,b,N)
   
   real(8) a,b,h,m,soma,f
   integer i,N
     
     h=(b-a)/N
     
     soma=0
     do i=1,N-1
       if(mod(i,2).ne.0)m=4
       if(mod(i,2).eq.0)m=2
      soma = soma+m*f(a+i*h)
     end do  
   
     simpson=h*(f(a)+soma+f(b))/3
      
   end function
