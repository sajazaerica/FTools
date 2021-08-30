Subroutine minors(A, n, ans)
	Implicit None
	
	Integer, Intent(In) :: n
	Double Precision, Dimension(n, n), Intent(InOut) :: A
	Double Precision, Dimension(n, n), Intent(Out) :: ans
	Double Precision, Dimension(n-1, n-1):: tmp
	Integer :: i, j, p, q
	
	Do i = 1, n
		Do j = 1, n
			tmp = A([(p, p=1,i-1), (p, p=i+1,n)], [(p, p=1,j-1), (p, p=j+1,n)])
			Call det(tmp, n-1, ans(i, j))
		End Do
	End Do
	
End Subroutine minors