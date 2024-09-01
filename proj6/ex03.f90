program predadorpresa
implicit none
real*8 a,b,c,d,x,y,xi,t,dt,tmax
integer i

a=2d0/3d0;b=4d0/3d0;c=1d0;d=1d0

dt=1d-3;tmax=1d2

x=1d-1;y=1d-1

t=0d0
open(1,file='predador_presa.dat')
open(2,file='xvst')
open(3,file='yvst')

do while(t<tmax)
  write(1,*) x,y
  write(2,*) t,x
  write(3,*) t,y
  
  xi=x
  x=x+a*x*dt-b*x*y*dt
  y=y-c*y*dt+d*xi*y*dt
  
  t=t+dt
end do

do i=1,3
close(i)
end do

end program
