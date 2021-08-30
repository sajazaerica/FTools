!Program Test
!	Implicit None	
!	Double Precision, Dimension(:, :), Allocatable :: A
!	Integer :: n
!	Double Precision :: ans
!	n = 4
!	Allocate(A(n,n))
!	A(1, :) = (/1, -1, 1, 1/)
!	A(2, :) = (/1, -1, 1, 1/)
!	A(3, :) = (/-3, 0, 1, -2/)
!	A(4, :) = (/1, -4, 0, 6/)	
!	Call d_det(A, n, ans)
!	Write(*,*) ans
!End Program Test

Subroutine d_det(A, n, ans)
	Implicit None
	
	Integer, Intent(In) :: n
	Double Precision, Dimension(n, n), Intent(InOut) :: A
	Double Precision, Intent(Out) :: ans
	Double Precision, Dimension(n, n):: tmp, main
	Integer :: i, j, k
	
	main = A
	tmp = A
	Do k = 1, n-1
		Do i = n, k+1, -1
			Do j = k, n
				tmp(i, j) = main(i, j) - main(i, k) * main(k, j) / main(k, k)
			End Do
		End Do
		main = tmp
	End Do
	
	ans = 1.0
	Do i = 1, n
		ans = ans * tmp(i, i)
	End Do
	
	If (IsNan(ans)) ans = 0.0
			
End Subroutine d_det
