! Authors: Amirmohammad Gholizad, Kosar Rashedi
program P01_AreaCalculator

  implicit none
  real :: R, A
  double precision, parameter :: PI = 3.14159265359

  print *, "This program calculates the area inside a circle."
  print *, "Please enter the radius: "
  read *, R

  A = PI * (R**2)

  print *, "The calculated area is: ", A

end program P01_AreaCalculator
