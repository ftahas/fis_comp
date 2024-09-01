program ex01
   implicit none
   integer i,j,nh,k,nnh
   real(8) f,dff,dft,df3s,df5s,ddf3s,ddf5s,hh(14)
   real(8) x,h,df,d2f,hmin,hmax,dh,table(14,6)
 
   x=1.d0
   hmin=1.0d-8
   hmax=1.0d-1
   nh=10000

   nnh=14
   
   dh=(hmax-hmin)/dfloat(nh)
   
 
   open(10,file='h.in')
   read(10,*)(hh(i),i=1,14)  
   close(10)

   open(20,file='table.out')

   do i=1,14
     write(20,*) hh(i), dabs(df(x)-dff(x,hh(i))), dabs(df(x)-dft(x,hh(i))), dabs(df(x)-df3s(x,hh(i))),&
                dabs(df(x)-df5s(x,hh(i))), dabs(d2f(x)-ddf3s(x,hh(i))), dabs(d2f(x)-ddf5s(x,hh(i)))
   end do

   close(20)

        do i=1,nnh
             table(i,1)=dabs(df(x)-dff(x,hh(i)))
             table(i,2)=dabs(df(x)-dft(x,hh(i)))
             table(i,3)=dabs(df(x)-df3s(x,hh(i)))
             table(i,4)=dabs(df(x)-df5s(x,hh(i)))
             table(i,5)=dabs(d2f(x)-ddf3s(x,hh(i)))
             table(i,6)=dabs(d2f(x)-ddf5s(x,hh(i)))
           end do
           

       do k=1,6
         do i=1,nnh
           do j=1,nnh
             if (dabs(table(i,k))>dabs(table(j,k)) .and. table(j,k)/=0) table(i,k)=0
           end do
         end do
       end do 
 

        do i=1,nnh
          if (table(i,1)/=0) print*,'h ótimo para derivada para frente de 2 pontos:',hh(i)       
        end do

        do i=1,nnh
          if (table(i,2)/=0) print*,'h ótimo para derivada para trás de 2 pontos:',hh(i)       
        end do

        do i=1,nnh
          if (table(i,3)/=0) print*,'h ótimo para derivada simétrica de 3 pontos:',hh(i)       
        end do

        do i=1,nnh
          if (table(i,4)/=0) print*,'h ótimo para derivada simétrica de 5 pontos:',hh(i)       
        end do

        do i=1,nnh
          if (table(i,5)/=0) print*,'h ótimo para segunda derivada simétrica de 3 pontos:',hh(i)       
        end do

        do i=1,nnh
          if (table(i,6)/=0) print*,'h ótimo para segunda derivada simétrica de 5 pontos:',hh(i)       
        end do

 
 do i=1,4
  open(i)
 end do
 
 h=hmin
 do i=1,nh
  write(1,*)DLOG10(h),DLOG10(DABS(df(x)-dff(x,h)))
  write(2,*)DLOG10(h),DLOG10(DABS(df(x)-df3s(x,h)))
  write(3,*)DLOG10(h),DLOG10(DABS(df(x)-df5s(x,h)))
  write(4,*)DLOG10(h),DLOG10(DABS(d2f(x)-ddf3s(x,h)))
  h=h+dh
 end do

 do i=1,4
 close(i)
 end do

end program

function df(x)
   implicit none
   real(8) df,x

   df=2d0*dexp(2d0*x)*dsin(x)+dexp(2d0*x)*dcos(x) 
end function

function d2f(x)
  implicit none
  real(8) d2f,x

  d2f=3d0*dexp(2d0*x)*dsin(x)+4d0*dexp(2d0*x)*dcos(x)
end function

function f(x)
   implicit none
   real(8) f,x

   f=dexp(2.d0*x)*dsin(x)     
end function

function dff(x,h)
   implicit none
   real(8) f,dff,x,h

   dff=(f(x+h)-f(x))/h
end function

function dft(x,h)
   implicit none
   real(8) f,dft,x,h
   
   dft=(f(x)-f(x-h))/h
end function

function df3s(x,h)
   implicit none
   real(8) f,df3s,x,h
 
   df3s=(f(x+h)-f(x-h))/(2d0*h)
end function


function df5s(x,h)
   implicit none
   real(8) f,df5s,x,h

   df5s=(f(x-2d0*h)-8d0*f(x-h)+8d0*f(x+h)-f(x+2d0*h))/(12d0*h)
end function

function ddf3s(x,h)
   implicit none
   real(8) ddf3s,f,x,h

   ddf3s=(f(x+h)-2d0*f(x)+f(x-h))/(h**2d0)
end function

function ddf5s(x,h)
   implicit none
   real(8) ddf5s,f,x,h

   ddf5s=(-f(x-2d0*h)+16*f(x-h)-30*f(x)+16*f(x+h)-f(x+2d0*h))/(12d0*(h**2d0))
end function
