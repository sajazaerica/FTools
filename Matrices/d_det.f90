Subroutine d_det(A, n, ans)
	Implicit None
	
	Integer, Intent(In) :: n
	Double Precision, Dimension(n, n), Intent(InOut) :: A
	Double Precision, Intent(Out) :: ans
	Double Precision, Dimension(n, n):: tmp
	Integer :: i
	
	tmp = A
	Call triangulate(tmp, n)
	
	ans = 1.0
	Do i = 1, n
		ans = ans * tmp(i, i)
	End Do
	
	If (IsNan(ans)) ans = 0.0
			
End Subroutine d_det
