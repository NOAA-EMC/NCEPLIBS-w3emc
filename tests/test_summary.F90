! This is a test in the NCEPLIBS-w3emc project.
!
! Test the summary subroutine.
!
! Kyle Gerheiser
program test_summary
  implicit none
  integer :: i, j, k, returnval
  real :: x, y, z
  
  print *, "Testing summary()..."
  
  ! Test 1: Basic call to start() and summary()
  print *, "Test 1: Basic start/summary cycle..."
  call start()
  call summary()
  print *, "Test 1 passed"
  
  ! Test 2: Multiple start/summary cycles
  print *, "Test 2: Multiple cycles..."
  do i = 1, 3
    print *, "  Cycle ", i
    call start()
    ! Do some simple work to accumulate CPU time
    do j = 1, 100
      x = real(j)
      y = sqrt(x)
      z = y * y
    end do
    call summary()
  end do
  print *, "Test 2 passed"
  
  ! Test 3: Longer computation between start and summary
  print *, "Test 3: Longer computation..."
  call start()
  do i = 1, 1000
    x = real(i)
    y = sqrt(x)
    z = sin(y)
    do j = 1, 10
      x = x + y
      y = y + cos(z)
    end do
  end do
  call summary()
  print *, "Test 3 passed"
  
  ! Test 4: Very quick start/summary (minimal work)
  print *, "Test 4: Minimal work..."
  call start()
  x = 1.0
  y = x + 1.0
  call summary()
  print *, "Test 4 passed"
  
  ! Test 5: Nested-like calls (multiple start/summary sequences)
  print *, "Test 5: Sequential sequences..."
  call start()
  do i = 1, 50
    x = real(i)
    y = sqrt(x)
  end do
  call summary()
  
  call start()
  do i = 1, 100
    x = real(i)
    z = sin(x)
  end do
  call summary()
  print *, "Test 5 passed"
  
  ! Test 6: Heavy computation
  print *, "Test 6: Heavy computation..."
  call start()
  do i = 1, 500
    do j = 1, 500
      x = real(i) * real(j)
      y = sqrt(abs(x))
      z = sin(y) + cos(y)
    end do
  end do
  call summary()
  print *, "Test 6 passed"
  
  print *, "SUCCESS"
end program test_summary

