program test_w3fa09
  implicit none
  real :: W3FA09
  real :: tk1, tk2, vp1, vp2
  integer :: errors
  real, parameter :: epsilon = 1.0e-5

  errors = 0
  print *, "Testing w3fa09..."

  ! Test case 1: tk < 223.16 (should be clamped to 223.16)
  tk1 = 200.0
  tk2 = 223.16
  vp1 = W3FA09(tk1)
  vp2 = W3FA09(tk2)
  if (abs(vp1 - vp2) > epsilon) then
     print *, "Test 1 failed (clamping below 223.16). vp(200.0)=", vp1, " vp(223.16)=", vp2
     errors = errors + 1
  end if

  ! Test case 2: 223.16 <= tk <= 323.16
  tk1 = 273.16
  vp1 = W3FA09(tk1)
  ! At 273.16 K, VP should be approx 0.6112 kPa
  if (abs(vp1 - 0.6112) > 1.0e-3) then
     print *, "Test 2 failed (normal evaluation). vp(273.16)=", vp1, " expected ~0.6112"
     errors = errors + 1
  end if

  ! Test case 3: tk > 323.16 (should be clamped to 323.16)
  tk1 = 350.0
  tk2 = 323.16
  vp1 = W3FA09(tk1)
  vp2 = W3FA09(tk2)
  if (abs(vp1 - vp2) > epsilon) then
     print *, "Test 3 failed (clamping above 323.16). vp(350.0)=", vp1, " vp(323.16)=", vp2
     errors = errors + 1
  end if

  if (errors > 0) then
     print *, "TEST FAILED. Total errors:", errors
     stop 1
  else
     print *, "SUCCESS"
  end if

end program test_w3fa09
