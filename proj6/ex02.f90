program ex2
implicit none
real(8) r,x0,e,lambda,dr,G
integer i,N,j,periodo,k

!item b
!r=3.5d0;dr=1d-2;x0=9d-1
!N=1e4;e=0.01;i=0
!periodo=1

!do while(i.ne.N)
  !call mapa(r,x0,i,N,periodo)
  !r=r+dr
!end do

!print*, r


!item c
!r=3.8d0;x0=1d-1;i=0;N=100;periodo=1

!do k=1,4
  !open(k); call mapa(r,x0,i,N,periodo,k); close(k)
  !x0=x0+2d-1
!end do


!item d
r=0d0;dr=1d-4;N=30
call bifurca(r,dr,N)


end program

subroutine bifurca(r,dr,N)
implicit none
real(8) r,dr,xi,x,G,x0
integer l,i,N,periodo,j,k

open(2,file='bifurca.dat')

periodo=1

do while(r<4d0)
10  do l=1,5
      x0=rand();x=x0;xi=x0+1d0
      i=0
      do while(dabs(x-xi)>1d-4 .and. i<N)
        xi=x
        do j=1,periodo
          x=G(r,x)
        end do
        i=i+1
      end do
      if(i==N .and. r<3.569946d0) then
        periodo=periodo*2
        go to 10
      else
        write(2,*) r,x
      end if
    end do
  r=r+dr
end do

close(2)

end subroutine


subroutine mapa(r,x0,i,N,periodo,k)
implicit none
real(8) x,r,G,xi,x0
integer i,N,periodo,j,k

x=x0;xi=1d0;i=0

do while(dabs((x-xi)/x)>1d-4 .and. i<N)
  write(k,*) i,x
  xi=x
  do j=1,periodo
    x=G(r,x)
  end do
  i=i+1
end do

end subroutine 

real(8) function G(r,x)
real(8) r,x
G=r*x*(1-x)
end function

real(8) function Gp(r,x)
real(8) r,x
Gp=r*(1-2*x)
end function

subroutine distancia(r,x0,e,n)
implicit none
real(8) x1,x2,x0,e,G,r
integer i,n

x1=x0+e;x2=x0

 open(1)

do i=0,n
  write(1,*) i,dabs(x1-x2)
  x1=G(r,x1)
  x2=G(r,x2)
end do

 close(1)

return
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

return
end function
