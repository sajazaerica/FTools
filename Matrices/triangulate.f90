Subroutine triangulate(A, n)
	Implicit None
	
	Integer, Intent(In) :: n
	Double Precision, Dimension(n, n), Intent(InOut) :: A
	
	Double Precision, Dimension(n, n):: tmp
	Integer :: i, j, k
	
	tmp = A
	Do k = 1, n-1
		Do i = n, k+1, -1
			Do j = k, n
				tmp(i, j) = A(i, j) - A(i, k) * A(k, j) / A(k, k)
			End Do
		End Do
		A = tmp
	End Do
	
End Subroutine triangulate