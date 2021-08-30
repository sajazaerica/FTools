Subroutine inv(A, n, ans, ierr)
	Implicit None
	
	Integer, Intent(In) :: n
	Double Precision, Dimension(n, n), Intent(InOut) :: A
	Double Precision, Dimension(n, n), Intent(Out) :: ans
	Integer, Intent(Out) :: ierr
	Double Precision, Dimension(n, n) :: tmp, tmp2
	Double Precision :: tmp_det
	
	Integer :: i, j, p, q
	
	ierr = 0
	
	Call det(A, n, tmp_det)
	If (tmp_det .EQ. 0.0) Then
		ierr = 1
		ans = 0.0
		Return
	End If
	
	Call minors(A, n, tmp)
	Call trans(tmp, n, tmp2)		
	
	Do i = 1, n
		Do j = 1, n
			ans(i, j) = (-1) ** (i+j) * tmp2(i, j) / tmp_det
		End Do
	End Do
End Subroutine inv