program ex1
implicit none
real(8) dr,r,x0,e,lambda
integer i,j,n,periodo

print*, 'enter r,x0, n, e and periodo'
read(*,*) r,x0,n,e,periodo

!item b
!do i=1,3
  !call mapa(r,i,x0,n,periodo)
 
! x0=x0+0.25d0
!end do

!item c
open(99,file='distancia.dat')
call distancia(r,x0,e,n,periodo)
 close(99)

!item d
print*, lambda(r,x0,n)

r=0d0;dr=1d-4
call lyapunov(r,dr,x0,n)

end program

subroutine mapa(r,k,x0,n,periodo)
implicit none
real(8) x,r,x0,G,xi
integer i,j,n,periodo,k

! open(1)

 x=x0

  do i=0,n
    !if(i==0) write(k,*) x,0
    !if(i>0) write(k,*) x,x
    write(k,*) i,x
    xi=x
    do j=1,periodo
      x=G(r,x)
    end do
   ! write(k,*) xi,x
  end do

! close(1)

end subroutine 


real(8) function G(r,x)
real(8) r,x
G=r*x*(1-x)
end function

real(8) function Gp(r,x)
real(8) r,x
Gp=r*(1-2*x)
end function


subroutine distancia(r,x0,e,n,periodo)
implicit none
real(8) x1,x2,x0,e,G,r
integer j, i,n,periodo

x1=x0+e;x2=x0

do i=0,n
  write(99,*) i,dabs(x1-x2)
 do j=1,periodo
  x1=G(r,x1)
  x2=G(r,x2)
 end do
end do

end subroutine

real(8) function lambda(r,x0,n)
real(8) r,x,x0,G,Gp
integer i,n

x=x0;lambda=0

do i=0,n-1
  lambda=lambda+dlog(dabs(Gp(r,x)))
  x=G(r,x)
end do

lambda=lambda/n

end function

subroutine lyapunov(r,dr,x0,n)
implicit none
real(8) r,x0,dr,lambda
integer n

 open(46,file='lyapunov.dat')

do while(r<4d0)
  write(46,*) r,lambda(r,x0,n)
  r=r+dr
end do

 close(46)

end subroutine
