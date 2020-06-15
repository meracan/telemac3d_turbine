
program my_code

USE CheckModule, ONLY:Check

integer, parameter      :: N = 1000
integer, dimension(1:N) :: ArrayA, ArrayB

ArrayA=1
ArrayB=2

IF(Check(ArrayA,ArrayB)) print *,"Not equal"

ArrayB=1
IF(.NOT. Check(ArrayA,ArrayB)) print *,"Not equal"

end program my_code