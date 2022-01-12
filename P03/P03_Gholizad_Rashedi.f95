! Authors: Amir M. Gholizad, Kosar Rashedi

program WHAT_IS_THE_LOCATION_

	implicit none

	! define constants and variables
  	real :: x, y, length
  	real :: x0 = 10, y0 = 10, r = 2.5
    integer :: i, count = 0
	
	! open the txt files
	open (unit = 2, file = "result.txt")
	open (unit = 1, file = "coordinates.txt")
        	
	! prepare result.txt file
    write(2,100) "X", "Y", "Area" 
    100  format (12x,A1,15x,A1,3x,A4)
    write(2,*) "------------------------------------"
    	
	
    	do i = 1, 10000
              
            ! read the coordinates
           	read(1,*) x, y
                
            ! calculate the length
            length = sqrt( x**2 + y**2 )
                
            ! apply the conditiones
            if (length <= r) then
              write(2,*) x, y, "R1"
            else if ((abs(x) > x0) .or. (abs(y) > y0)) then
              write(2,*) x, y, "R3"
            else
              write(2,*) x, y, "R2"
              count = count + 1
            end if
        end do
        ! found the locations :)
    
    close (1)
    close (2)

    write(6,*) "The number of locations inside R2 was: ", count


end program WHAT_IS_THE_LOCATION_

