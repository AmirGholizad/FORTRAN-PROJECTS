! Authors: Amir M. Gholizad, Kosar Rashedi

program THE_PROJECTILE_PROBLEM

	implicit none

	!Defining constants and variables
  	double precision :: pi = 3.14159265359
  	real :: alpha
  	real :: t, v0, s = 3, g = 9.8
    real, dimension(5) :: Theta
	integer :: i, j

  	!Defining initial velocity v0 
  	v0 = 0.5 * s * 1/(sqrt(sqrt(g)))
	
	!Reading theta variables and saiving them into Theta array
	open (unit = 2, file = "result.txt")
	open (unit = 1, file = "data.txt")
        	
            do i = 1,5
            	read(1,*) Theta(i)
            end do
    !Calculating t variable and writing into result.txt       
			do j = 1,5
            	alpha = Theta(j) * (pi / 180)
  				t = 2 * (v0 / g) * sin(alpha)
            	write(2,*) "angle =", Theta(j), ',' ,"t =", t
            end do
    close (1)
    close (2)

end program THE_PROJECTILE_PROBLEM

