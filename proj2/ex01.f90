program ex01c
implicit none
integer j,Nseq
real(8) a,c,m,x_av,dev
real(8),allocatable::I(:)

a=7**5;c=0;m=(2d0**31)-1

print*,'Enter the size of the sequence'
read*,Nseq

allocate(I(Nseq))

print*,'enter I(0)'
read*,I(1)

I(1)=mod(a*I(1)+c,m)

do j=1,Nseq-1
 I(j+1)=a*I(j)+c
 I(j+1)=mod(I(j+1),m)
end do

I=I/m

open(10,file='sequencia.dat')
write(10,*)I
 close(10)

call average(Nseq,I,x_av,dev)

print*,'Media =',x_av
print*,'Desvio padrao =',dev

end program

subroutine average(N,x,x_av,dev)
      implicit none
      integer i
      integer, intent(in)::N
      real(8) soma,x2_av
      real(8),intent(in)::x(N)
      real(8),intent(out)::x_av,dev
 
      soma=0
      do i=1, N
         soma = x(i)+soma
      end do
       
      x_av = soma/N

      soma=0
      do i=1,N
         soma=soma+x(i)**2
      end do
  
      x2_av=soma/N
      
      dev = sqrt(x2_av-x_av**2) 

end subroutine
