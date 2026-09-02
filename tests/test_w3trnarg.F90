! Unit tests for w3trnarg() covering default and TIMLIM parsing paths,
! including argument validation and EOF handling.
program test_w3trnarg
  implicit none

  call test_default_notimlim()
  call test_timlim_default_window()
  call test_timlim_sign_normalization()
  call test_short_subdir_rejected()
  call test_short_tankid_rejected()
  call test_missing_required_args()

  print *, 'SUCCESS!'

contains

  subroutine test_default_notimlim()
    character(len=16) :: subdir, tankid, appchr, tlflag
    integer :: lsubdr, ltnkid, lapchr, iymdhb, iymdhe, ierr

    call run_w3trnarg((/ character(len=24) :: &
      'gdas pgrb an 2024011512' /), 1, subdir, lsubdr, tankid, ltnkid, appchr, lapchr, tlflag, iymdhb, iymdhe, ierr)

    call expect_eq_int(ierr, 0, 'default notimlim ierr')
    call expect_eq_str(tlflag(1:8), 'NOTIMLIM', 'default notimlim flag')
    call expect_eq_int(lsubdr, 4, 'default lsubdr')
    call expect_eq_int(ltnkid, 4, 'default ltnkid')
    call expect_eq_int(lapchr, 2, 'default lapchr')
    call expect_eq_int(iymdhb, 0, 'default lower bound')
    call expect_eq_int(iymdhe, 2100000000, 'default upper bound')
  end subroutine test_default_notimlim

  subroutine test_timlim_default_window()
    character(len=16) :: subdir, tankid, appchr, tlflag
    integer :: lsubdr, ltnkid, lapchr, iymdhb, iymdhe, ierr

    call run_w3trnarg((/ character(len=24) :: &
      'gdas pgrb z9 2024011512', &
      'TIMLIM' /), 2, subdir, lsubdr, tankid, ltnkid, appchr, lapchr, tlflag, iymdhb, iymdhe, ierr)

    call expect_eq_int(ierr, 0, 'timlim default ierr')
    call expect_eq_str(tlflag(1:6), 'TIMLIM', 'timlim default flag prefix')
    call expect_eq_int(iymdhb, 2024011312, 'timlim default lower bound')
    call expect_eq_int(iymdhe, 2024011600, 'timlim default upper bound')
  end subroutine test_timlim_default_window

  subroutine test_timlim_sign_normalization()
    character(len=16) :: subdir, tankid, appchr, tlflag
    integer :: lsubdr, ltnkid, lapchr, iymdhb, iymdhe, ierr

    ! Positive-before/negative-after should be swapped by w3trnarg.
    call run_w3trnarg((/ character(len=24) :: &
      'gdas pgrb q1 2024011512', &
      'TIMLIM', &
      '6', &
      '-2' /), 4, subdir, lsubdr, tankid, ltnkid, appchr, lapchr, tlflag, iymdhb, iymdhe, ierr)

    call expect_eq_int(ierr, 0, 'timlim normalized ierr')
    call expect_eq_int(iymdhb, 2024011510, 'timlim normalized lower bound')
    call expect_eq_int(iymdhe, 2024011518, 'timlim normalized upper bound')
  end subroutine test_timlim_sign_normalization

  subroutine test_short_subdir_rejected()
    character(len=16) :: subdir, tankid, appchr, tlflag
    integer :: lsubdr, ltnkid, lapchr, iymdhb, iymdhe, ierr

    call run_w3trnarg((/ character(len=24) :: &
      'gda pgrb aa 2024011512' /), 1, subdir, lsubdr, tankid, ltnkid, appchr, lapchr, tlflag, iymdhb, iymdhe, ierr)

    call expect_eq_int(ierr, 2, 'short subdir ierr')
  end subroutine test_short_subdir_rejected

  subroutine test_short_tankid_rejected()
    character(len=16) :: subdir, tankid, appchr, tlflag
    integer :: lsubdr, ltnkid, lapchr, iymdhb, iymdhe, ierr

    call run_w3trnarg((/ character(len=24) :: &
      'gdas pgb aa 2024011512' /), 1, subdir, lsubdr, tankid, ltnkid, appchr, lapchr, tlflag, iymdhb, iymdhe, ierr)

    call expect_eq_int(ierr, 2, 'short tankid ierr')
  end subroutine test_short_tankid_rejected

  subroutine test_missing_required_args()
    character(len=16) :: subdir, tankid, appchr, tlflag
    integer :: lsubdr, ltnkid, lapchr, iymdhb, iymdhe, ierr

    call run_w3trnarg((/ character(len=1) :: ' ' /), 0, subdir, lsubdr, &
      tankid, ltnkid, appchr, lapchr, tlflag, iymdhb, iymdhe, ierr)

    call expect_eq_int(ierr, 1, 'missing required args ierr')
  end subroutine test_missing_required_args

  subroutine run_w3trnarg(lines, nlines, subdir, lsubdr, tankid, ltnkid, appchr, lapchr, tlflag, iymdhb, iymdhe, ierr)
    character(len=*), intent(in) :: lines(:)
    integer, intent(in) :: nlines
    character(len=*), intent(out) :: subdir, tankid, appchr, tlflag
    integer, intent(out) :: lsubdr, ltnkid, lapchr, iymdhb, iymdhe, ierr

    integer :: iu, i
    character(len=256) :: prog_name
    character(len=256) :: temp_file

    call get_command_argument(0, prog_name)
    temp_file = trim(prog_name) // '.stdin'
    do i = 1, len_trim(temp_file)
      if (temp_file(i:i) == '/' .or. temp_file(i:i) == '\') temp_file(i:i) = '_'
    end do

    subdir = ' '
    tankid = ' '
    appchr = ' '
    tlflag = ' '
    lsubdr = -999
    ltnkid = -999
    lapchr = -999
    iymdhb = -999
    iymdhe = -999
    ierr = -999

    open(newunit=iu, file=trim(temp_file), status='replace', action='write', form='formatted')
    do i = 1, nlines
      write(iu, '(A)') trim(lines(i))
    end do
    close(iu)

    open(unit=5, file=trim(temp_file), status='old', action='read', form='formatted')
    call w3trnarg(subdir, lsubdr, tankid, ltnkid, appchr, lapchr, tlflag, iymdhb, iymdhe, ierr)
    close(5)

    open(newunit=iu, file=trim(temp_file), status='old')
    close(iu, status='delete')
  end subroutine run_w3trnarg

  subroutine expect_eq_int(actual, expected, message)
    integer, intent(in) :: actual, expected
    character(len=*), intent(in) :: message

    if (actual /= expected) then
      print *, 'FAIL: ', trim(message), ' expected=', expected, ' actual=', actual
      stop 1
    end if
  end subroutine expect_eq_int

  subroutine expect_eq_str(actual, expected, message)
    character(len=*), intent(in) :: actual, expected
    character(len=*), intent(in) :: message

    if (actual /= expected) then
      print *, 'FAIL: ', trim(message), ' expected="', expected, '" actual="', actual, '"'
      stop 2
    end if
  end subroutine expect_eq_str

end program test_w3trnarg
