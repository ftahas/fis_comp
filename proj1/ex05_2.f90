      program lista
    
         implicit none
         integer::i,N,M
         real, allocatable::v(:)

         print*, 'Enter the size of your list'
         read*, N
     
         allocate(v(N))

         print*, 'Enter the numbers of the list'
         read*, (v(i), i=1, N)
         print*, 'Input list:',v

         print*, 'Enter M'
         read*, M
 
         !print*, findmin(v,1,N)

          !do i=1,N-1
           !loc_min=findmin(v,i,N)
           !call troca(v(i),v(loc_min))
         !enddo
      
         call sort(v,M)
         print*,'Ordered list:',v       
    
         contains
       
         integer function findmin(x,comeco,fim)
         integer comeco,fim,loc_min
         real x(1:),minimum
         
         minimum=x(comeco)
         loc_min=comeco
         do i=comeco+1,fim
             if (x(i)<minimum) then
                minimum=x(i)
                loc_min=i
             endif
         enddo
             
         findmin=loc_min
         end function
         
         subroutine troca(a,b)
           real a,b,c
           c=b
           b=a
           a=c
         end subroutine

         subroutine sort(x,sizee)
         implicit none
         integer loc_min,i,sizee
         real x(1:)

          do i=1,sizee-1
           loc_min=findmin(x,i,sizee)
           call troca(x(i),v(loc_min))
         enddo
         
         end subroutine

      end program
