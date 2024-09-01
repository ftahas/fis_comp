      program ex05

         implicit none
         integer i,j,N,M
         real, allocatable::v1(:),v2(:),v3(:)
         real aux
         
         print*,'Enter N and M'
         read*, N,M
      
         allocate(v1(N),v2(M),v3(N-M))
 
         print*,'Enter the vector entries'
         read*, (v1(i),i=1,N)

         do i=1,M
            v2(i)=v1(i)
         end do
 
         do i=M+1,N
            v3(i-M)=v1(i)
         end do
     
         do i=1,M
           do j=1,N-M
             if (v2(i)>v3(j)) then
               aux=v2(i)
               v2(i)=v3(j)
               v3(j)=aux
             end if
           end do
         end do

         do i=1,M
           do j=i+1,M
             if (v2(i)>v2(j)) then
               aux=v2(i)
               v2(i)=v2(j)
               v2(j)=aux
             end if
           end do
         end do

        open(1,file='ex05.dat')
        write(1,*) v2
        close(1)   

      end program     
