Program Test
	Implicit None	
	Double Precision, Dimension(:, :), Allocatable :: A
	Double Precision, Dimension(:, :), Allocatable :: ans
	Integer :: n, i, ierr
	
	n = 4
	Allocate(A(n,n))
	Allocate(ans(n,n))
	A(1, :) = (/4, 3, 1, 9/)
	A(2, :) = (/5, 6, 7, 3/)
	A(3, :) = (/1, 2, 6, 12/)
	A(4, :) = (/3, 8, 7, 4/)	
	Call inv(A, n, ans, ierr)
	Do i = 1, n
		Write(*,'(4F8.4)') ans(i, :)
	End Do
	
End Program Test

Subroutine det(A, n, ans)
	Implicit None
	
	Integer, Intent(In) :: n
	Double Precision, Dimension(n, n), Intent(InOut) :: A
	Double Precision, Intent(Out) :: ans
	Double Precision, Dimension(n, n):: tmp, main
	Integer :: i, j, k
	
	tmp = A
	Call triangulate(tmp, n)
	
	ans = 1.0
	Do i = 1, n
		ans = ans * tmp(i, i)
	End Do
	
	If (IsNan(ans)) ans = 0.0
			
End Subroutine det


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