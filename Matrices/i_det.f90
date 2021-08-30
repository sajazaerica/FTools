Subroutine i_det(A, n, ans)
	Implicit None
	
	Integer, Intent(In) :: n
	Integer, Dimension(n, n), Intent(InOut) :: A
	Integer, Intent(Out) :: ans
	Integer, Dimension(n, n):: tmp
	Integer :: i
	
	tmp = A
	Call i_triangulate(tmp, n)
	
	ans = 1
	Do i = 1, n
		ans = ans * tmp(i, i)
	End Do
	
	If (IsNan(ans)) ans = 0
			
End Subroutine i_det