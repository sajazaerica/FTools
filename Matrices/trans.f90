Subroutine trans(A, n, ans)
	Implicit None
	
	Integer, Intent(In) :: n
	Double Precision, Dimension(n, n), Intent(InOut) :: A
	Double Precision, Dimension(n, n), Intent(Out) :: ans
	
	Integer :: i, j
	
	Do i = 1, n
		Do j = 1, n
			ans(i, j) = A(j, i)
		End Do
	End Do
End Subroutine trans