program ex02
implicit none

real(8) pi,gam,theta,angulo_max,x
character(3) wr

pi=dacos(-1d0)

wr='yes'

!!!!!!!!!!!!!!!!!!!item a, trajetórias!!!!!!!!!!!!!!
gam=0

open(1,file='trajetorias_itemA.out')

theta=0
do while (theta<=pi/2)
  call projetil(gam,theta,wr,x)
  theta=theta+pi/36
end do

 close(1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!item a, alcance máximo!!!!!!!!!!!!!!!!!!!!!
open(1,file='alcanceA.out')
call alcance(gam,angulo_max)
  print*,'Alcance máximo sem resistência =', angulo_max
 close(1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!!!!!!!!!!!!!!!!!!!!!!!!item b, trajetórias!!!!!!!!!!!!!!!!!!!!!
gam=4e-5

open(1,file='trajetorias_itemB.out')

theta=0
do while (theta<=pi/2)
  call projetil(gam,theta,wr,x)
  theta=theta+pi/36
end do

 close(1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!item b, alcance máximo!!!!!!!!!!!!!!!!!!!!!!!!!!!
 open(1,file='alcanceB.out')
 call alcance(gam,angulo_max)
   print*,'Alcance máximo com resistência =', angulo_max
 close(1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!item c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
gam=0;theta=pi/6;open(1,file='ang30NoResis.out')
call projetil(gam,theta,wr,x);close(1)

theta=pi/3;open(1,file='ang60NoResis.out')
call projetil(gam,theta,wr,x);close(1)

gam=4e-5;theta=pi/6;open(1,file='ang30Resis.out')
call projetil(gam,theta,wr,x);close(1)

theta=pi/3;open(1,file='ang60Resis.out')
call projetil(gam,theta,wr,x);close(1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


end program



subroutine projetil(gam,theta,wr,x)
implicit none

real(8) gam,theta,v,dt,g
real(8) vx,vy,x,y,v0
character(3) wr

v0=700;dt=0.01;g=9.8
x=0;y=0;vx=v0*dcos(theta);vy=v0*dsin(theta)

     do while (y.ge.0)
       if (wr=='yes') write(1,*) x,y
       v=dsqrt(vx**2+vy**2)
       x=x+vx*dt
       vx=vx-gam*v*vx*dt
       y=y+vy*dt
       vy=vy-g*dt-gam*v*vy*dt
     end do 

end subroutine



subroutine alcance(gam,angulo_max)
implicit none

real(8) gam,theta,v,dt,g,angulo_max
real(8) vx,vy,x,y,pi,x_ant,v0
character(3) wr

v0=700;dt=0.01;g=9.8;theta=0

pi=dacos(-1d0) 


   wr='noo'
  
   x_ant=0
 
   do while (theta.lt.pi/2)
     x_ant=x
     call projetil(gam,theta,wr,x)
     if (x>x_ant) angulo_max=theta*180/pi
     write(1,*) theta*180/pi,x
     theta=theta+pi/180
   end do

end subroutine
