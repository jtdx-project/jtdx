program test_init_random_seed
  real :: r(10,4)
  call init_random_seed()
  call random_number(r)
  do i =1,10
     write (*, *) (r(i,j),j=1,4)
  end do
end program test_init_random_seed
