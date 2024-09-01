program ex01
implicit none

character(2) item


item='a'
call bike(item)

item='b1'
call  bike(item)

item='b2'
call bike(item)


item='c'
call bike(item)

end program


subroutine bike(item)
implicit none

character(2) item
real(8) rho,A,dt,t,tmax,m,P,v0,v,x,d
real(8) v_terminal,t_terminal,vi

m=70;P=400;tmax=300;dt=0.1;v0=4


t=0;v=v0;vi=0;x=0

if (item=='a') then

   rho=0;A=0.333

   open(1,file='bike_noresis.out')

   do while (t<=tmax)
     write(1,*) t,v
     v=v+P*dt/(m*v)-rho*A*v**2*dt/(2*m)
     x=x+v*dt
     t=t+dt
   end do

   close(1)

   d=m*v0**3*(1+2*P*tmax/(m*v0**2))**1.5/(3*P)

   print*,'Distância percorrida numericamente =',x
   print*,'Distância percorrida analiticamente =',d

else if (item=='b1') then

 
   rho=1.3;A=0.333

   open(2,file='bike_yesresis.out')

   do while (t<=tmax)
     write(2,*) t,v
     v=v+P*dt/(m*v)-rho*A*v**2*dt/(2*m)
     x=x+v*dt
     t=t+dt
   end do

   close(2)

   print*,'Distância percorrida =',x

else if (item=='b2') then

   rho=1.3;A=0.333

   do while (vi/=v)
    vi=v
    v=v+P*dt/(m*v)-rho*A*v**2*dt/(2*m)
    t=t+dt
   end do

   t_terminal=t
 
   v_terminal=(2*P/(rho*A))**0.333

  print*,'Velocidade terminal numérica =',v
  print*,'Velocidade terminal analítica =',v_terminal
  print*,'Tempo para atingir a velocidade terminal =',t_terminal


else if (item=='c') then
   
    rho=1.3;A=0

   open(3,file='bike_itemc.out')

  do while (A<=5)
     t=0;v=v0
     do while (t<=tmax)
       write(3,*) t,v,A
       v=v+P*dt/(m*v)-rho*A*v**2*dt/(2*m)
       t=t+dt
     end do
     A=A+0.2
  end do

   close(3)

end if


end subroutine
