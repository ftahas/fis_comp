program ex03
implicit none
real(8) theta,pi,theta0,dtheta,dt,T,simpson

pi=dacos(-1d0);theta0=pi/6

!item a
call euler(theta0)

!item b
call eulercromer(theta0)


!item c
open(44,file='periodo_eulercromer.out')
open(45,file='periodo_integral.out')

theta0=pi/180;dtheta=pi/180;dt=0.00001

do while (theta0.le.pi/2)
  call periodo(theta0,dt,T)
  write(44,*) theta0*180/pi,2*T
  write(45,*) theta0*180/pi,simpson(theta0)
  theta0=theta0+dtheta
end do

 close(44);close(45)

end program



real(8) function EM(omega,theta)
implicit none
real(8) omega,theta,m,g,L

m=1;g=10;L=1

EM=0.5*m*(L**2)*(omega**2)-m*g*L*(1-dcos(theta))
end function


subroutine euler(theta0)
implicit none
real(8) seno,omega,EM,m,g,L,tmax,dt,t,pi,theta,theta0

pi=dacos(-1d0)

m=1;g=10;L=1;tmax=20;dt=0.005

omega=0;t=0;theta=theta0

open(11,file='angulo_euler.out')
open(12,file='energia_euler.out')

do while (t.le.tmax+dt)
  write(11,*) t,theta*180/pi
  write(12,*) t,EM(omega,theta)
  seno=dsin(theta)
  theta=theta+omega*dt
  omega=omega-g*seno*dt/L
  t=t+dt 
end do

 close(11);close(12)

end subroutine


subroutine eulercromer(theta0)
implicit none
real(8) omega,EM,m,g,L,tmax,dt,t,pi,theta,theta0

m=1;g=10;L=1;tmax=20;dt=0.005
pi=dacos(-1d0)

omega=0;t=0;theta=theta0

open(21,file='angulo_eulercromer.out')
open(22,file='energia_eulercromer.out')

do while (t.le.tmax+dt)
  write(21,*) t,theta*180/pi
  write(22,*) t,EM(omega,theta)
  omega=omega-g*dsin(theta)*dt/L
  theta=theta+omega*dt
  t=t+dt
end do

 close(21);close(22)

end subroutine


   real(8) function f(theta,theta0)
   implicit none
   real(8) L,g,theta,theta0
   L=1;g=10

   f=dsqrt(2*L/g)/(dsqrt(dcos(theta)-dcos(theta0)))

   end function


  real(8) function simpson(theta0)
   implicit none
   real(8) theta0,h,m,soma,f
   integer i,N

     N=2**12
     
     h=2*theta0/N
     
     soma=0
     do i=1,N-1
       if(mod(i,2).ne.0)m=4
       if(mod(i,2).eq.0)m=2
      soma = soma+m*f(-theta0+i*h,theta0)
     end do  
   
     simpson=h*soma/3
      
   end function


subroutine periodo(theta0,dt,T)
implicit none
real(8) pi,omega,theta,m,g,L,dt,T,theta0,dtheta
m=1;g=10;L=1

pi=dacos(-1d0)

omega=0;theta=theta0;T=0

  do while (omega.le.0)
    omega=omega-g*dsin(theta)*dt/L
    theta=theta+omega*dt
    T=T+dt
  end do

end subroutine
