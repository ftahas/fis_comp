program ex02

implicit none
integer(8) k,i,j,n_t,R,I0,N0,N,t_hist
real(8) P,tau,dt,tmax,time,randseq
real(8), allocatable::vecN(:),vecN2(:),dev(:)

N0=1000;R=1000;tmax=10.d0;dt=0.01d0;tau=1.d0

t_hist=20
P=dt/tau
n_t=tmax/dt

allocate(vecN(n_t),vecN2(n_t),dev(n_t))

open(22,file='decay_histogram.dat')

vecN=0
vecN2=0
do j=1,R
  N=N0
  time=0
  I0=88+3*j
  if(j<=10) open(j+7)
  do k=1,n_t
    vecN(k)=vecN(k)+N
    vecN2(k)=vecN2(k)+N**2
    do i=1,N
      call random_generator(I0,randseq)
      if(randseq<=P) N=N-1
      if(N<0) N=0
    end do
    time=time+dt
    if(k==t_hist) write(22,*)N
    if(j<=10) write(j+7,*)time,N
  end do
 if(j<=10) close(j+7)
end do

 close(22)

vecN=vecN/R
vecN2=vecN2/R
dev=sqrt(vecN2-vecN**2)

time=0 
  open(49,file='N_average.out')
  open(50,file='deviation.out')
  open(51,file='deviation2.out')
  do i=1,n_t
    write(49,*)time,vecN(i)
    write(50,*)time,dev(i)
    write(51,*)time,(dev(i)**2)/vecN(i)
    time=time+dt
  end do

  do i=49,51
    close(i)
  end do

end program

subroutine random_generator(I0,randseq)
implicit none
integer(8) j,a,c,m,I0
real(8) randseq

a=16807;c=0;m=2147483647

I0=mod(a*I0+c,m)
randseq=dfloat(I0)/dfloat(m)

return
end subroutine
