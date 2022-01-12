! Authors: Amir M. Gholizad, Kosar Rashedi

program MATRIX_PRODUCT

	implicit none

	! define constants and variables
    real :: pi
    real :: d1, d2, d3
    integer :: i, j, k, count
  	real, dimension(4,7) :: C = 0
	real, dimension(5,7) :: B
  	real, dimension(4,5) :: A
    
    
	! define pi
    pi = acos(-1.0)
    
	! open the txt files
	open (unit = 2, file = "C.txt")
	open (unit = 1, file = "A.txt")
    	
		
		! fill the matrix A
    	do i = 1, 4
     
           	read(1,*) (A(i,j), j = 1, 5)
            
        end do

        ! fill the matrix B
        do i = 1, 5
        	do j = 1, 7

    			d1 = (-1)**(i+j)
                d2 = 0.1 + cos( i*(pi/3) - j*(pi/4) + (-i)*(-j)*(pi/2) )
                d3 = 1 - (2/3)*sin( (i-j)*(pi/6) )
                
                B(i,j) = d1 * d2 / d3
            
            end do
        end do


		i=0
        j=0
        k=0
		
		! calculate the A*B ( I used the matrix multipliation definition instead of mathmul(A,B) function )
        do i = 1, 4
          do j = 1, 7
            do k = 1, 5         
              
				C(i,j) = C(i,j) + A(i,k)*B(k,j)
          
            end do
          end do
        end do

        i = 0
        j = 0
        k = 0

        ! write the C into C.txt
        do i = 1, 4
        
          write(2,*) (C(i,j), j = 1, 7)
		
		end do
        
		i = 0
        j = 0
        k = 0
        count = 0

        ! check if A(i,j)/13 = 1 for positive entries
		do i = 1, 4
          do j = 1, 5
            if ( A(i,j) >= 0 .and. mod(A(i, j),13.0) == 0 ) THEN
              count = count + 1
            end if
          end do
        end do
          
		write(6,100) "The number of positive entries inside the matrix A that are divisible by 13 is =", count
        100 format(A80,1x,I1)
    	
		close (1)
    	close (2)

end program MATRIX_PRODUCT

