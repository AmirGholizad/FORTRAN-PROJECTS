! Authors: Amir M. Gholizad, Kosar Rashedi

 
!------------------------------------------------------------
subroutine H(A, hermitian)

    implicit none     
    real, dimension(25,25), intent(in) :: A
    integer :: m, n
    logical, intent(out) :: hermitian
        
    do m = 1, 25
        do n = 1, 25
            
        	if ( A(m,n) .eq. A(n,m) ) then
               	hermitian = .true.
            else
                hermitian = .false.
            end if

        end do
    end do	
    
    return 
    end subroutine H
!------------------------------------------------------------
        
program SUB_ROUTINE

	implicit none

	! define constants and variables
    real :: mu = (5.0/4.0), sigma = (4.0/5.0), pi, x
    double precision :: d1, d2, d3, d4, g
    integer :: i, j
    logical :: hermitian
  	real, dimension(25,25) :: A
        
	! define pi
    pi = acos(-1.0)
    
	! open the txt files
	open (unit = 2, file = "g(x).txt")
	open (unit = 1, file = "A.txt")
    	
		
		! fill the matrix A
    	do i = 1, 25
     
           	read(1,*) (A(i,j), j = 1, 25)
            
        end do

        
		! run the Hermitian-Check subroutine!
		call H(A, hermitian)
        
        if ( hermitian ) then
          write(6,*) "The Matrix A is Hermitian"
        else
          write(6,*) "The Matrix A is NOT Hermitian"
        end if

		! write the g(x).txt
        write(2,100) "x", "g(x)"
        100 format(A10, 14x ,A10)
        write(2,*) ("-", i = 1, 42)
		i=0
        j=0

        do j = 1, 25
          
          x = A(1,j)
          d1 = sigma * sqrt(2*pi)
    	  d2 = (x-mu)**2
          d3 = 2*(sigma**2)
          d4 = exp( -d2 / d3 )
    	  g = d4 / d1
          write(2,*) x, g

        end do
    	
		close (1)
    	close (2)

end program SUB_ROUTINE

