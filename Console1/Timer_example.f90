    !  Timer_testing
    !
    !

    !****************************************************************************
    ! testing timer funciton and learning fortran
    ! by Kyle Gorkowski
    !
    !****************************************************************************

    program Timer_example

    USE timer_module

    IMPLICIT NONE
    REAL(8) :: rand_val(50000,1000), i, loop_max
 ! change the max loops
    loop_max=20

    CALL timer_start('test') ! timer module
    DO i = 1, loop_max
        CALL RANDOM_NUMBER(rand_val) !calc

        CALL timer_update(i/loop_max)! timer module
    END DO

    CALL timer_done ! timer module

    READ(*,*) !waits for user to close

    end program Timer_example

