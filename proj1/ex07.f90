      program gauss
      implicit none

      real(8), allocatable::A(:,:,:),X(:,:,:),B(:,:,:)
      integer N,i,j,m

      print*,'Enter N'
      read*, N
   
      allocate(A(N,N,N),X(N,1,1),B(N,1,N))

      A=0
      B=0
      print*,'Enter the matrix A'
      read*,((A(i,j,1),i=1,N),j=1,N)
      print*,'Enter the matrix B'
      read*, (B(i,1,1),i=1,N)

      do m=2,N
        A(:,:,m)=A(:,:,1)
        B(:,1,m)=B(:,1,1)
      end do

      do m=2,N
        A(:,:,m)=A(:,:,m-1)
        B(:,1,m)=B(:,1,m-1)
        do i=m,N
          do j=1,N
            A(i,j,m)=A(i,j,m-1)-A(i,m-1,m-1)*A(m-1,j,m-1)/A(m-1,m-1,m-1)
            B(i,1,m)=B(i,1,m-1)-A(i,m-1,m-1)*B(m-1,1,m-1)/A(m-1,m-1,m-1)
          end do
        end do
      end do
            
      call sumgauss(N,A,X,B)

      print*,X

      end program

      subroutine sumgauss(N,A,X,B)
      implicit none
      integer N,i,j
      real(8) soma,A(N,N,N),X(N,1,1),B(N,1,N)
      
       X(N,1,1)=B(N,1,N)/A(N,N,N)
      do i=N-1,1,-1
        soma=0
        do j=i+1,N
          soma=soma+A(i,j,N)*X(j,1,1)
        end do
        X(i,1,1)=(B(i,1,N)-soma)/A(i,i,N)
      end do

      end subroutine
