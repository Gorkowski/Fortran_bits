! Timer Module by Kyle Gorkowski
    !
    !use example
    !
    !loop_max=15
    !CALL timer_start('test') ! timer module, input max char 20
    !DO i = 1, loop_max
    !    CALL RANDOM_NUMBER(rand_val) !calc
    !
    !    CALL timer_update(i/loop_max)! timer module, input real(8), 
    !END DO
    !
    !CALL timer_done ! timer module, summarizes the result

    
    MODULE timer_module

    IMPLICIT NONE
    REAL(8),PRIVATE :: input_progress, current_time, delta_time

    !type declaration
    TYPE,PRIVATE :: timer_stats
        REAL(8) :: start_time, slope, fraction_done, time, learning_rate
        CHARACTER(20),DIMENSION(1) :: timer_title, method
    end type timer_stats
    TYPE(timer_stats) :: Tstats

    ! private/public subroutines
    PRIVATE :: sec2timestr
    PUBLIC  :: timer_start, timer_update, timer_done


    CONTAINS

    !prints out timer start label and starts the clock and resets the Tstats values
    SUBROUTINE timer_start(timer_name)
    CHARACTER(*),INTENT(IN),DIMENSION(1) :: timer_name
    REAL(8) :: temp

    ! start the timer
    CALL CPU_TIME(temp)
    !sets the timer stats to defaults
    Tstats%start_time       =temp;  ! saves start time for total time to complete calc.
    Tstats%fraction_done    =0d0;   ! keeps track of last fraciton done
    Tstats%time             =temp;  ! keeps track of last time called
    Tstats%slope            =0d0;   ! starting slope
    Tstats%learning_rate    =0.90d0;   ! you can change this >0 and <1 addjusts how fast the moving slope is adjusted with longer/shor computation times
    Tstats%timer_title      =timer_name;
    Tstats%method           ='adaptive'; ! adaptive or full_average

    ! Writes out the start of the progress timer
    WRITE(*,*) 'Progress for...', timer_name
    END SUBROUTINE timer_start

    
    
    !updates the time to finish and writes out the update
    SUBROUTINE timer_update(input_progress)
    REAL(8) :: input_progress, current_time, slope_i, current_slope, time_left
    CHARACTER(LEN=50),DIMENSION(1) :: time_string

    CALL CPU_TIME(current_time) ! gets cpu time

    ! caculates slope of progress time
    slope_i =  (current_time-Tstats%time) / (input_progress-Tstats%fraction_done);

    ! gets new weighted slope
    IF (Tstats%fraction_done>0d0) THEN
        current_slope= Tstats%slope*Tstats%learning_rate + (1-Tstats%learning_rate)*slope_i; !learning rate averaging (weighted averaging), may add other estimation methods later
    ELSE
        current_slope =  slope_i; ! saves slope
    END IF

    ! calc time left
    time_left=(1-input_progress)*current_slope;

    !update timer stats
    Tstats%time             =current_time;
    Tstats%fraction_done    =input_progress;
    Tstats%slope            =slope_i;

    CALL sec2timestr(time_left, time_string)
    ! write out updated time
    WRITE(*,'(a, a, f10.4, a, a)') Tstats%timer_title, '; Percent Done ', input_progress*100, '%, Time Left ', time_string
    END SUBROUTINE timer_update

    
    
    ! wirtes the total time
    SUBROUTINE timer_done
     REAL(8) :: current_time
    CHARACTER(LEN=50),DIMENSION(1) :: time_string2
    
    CALL CPU_TIME(current_time) ! gets cpu time
    
    CALL sec2timestr(current_time-Tstats%start_time, time_string2) ! get nice time string
    
    WRITE(*,'(a, a, a)') Tstats%timer_title, '; Completed... Total time ', time_string2 ! write out total time

    END SUBROUTINE timer_done
    
    
    
! Private funcitons
!*************************************************************
    ! subroutine to get time string
    SUBROUTINE sec2timestr(sec, time_str)

    CHARACTER(*),DIMENSION(1) :: time_str
    REAL(8) :: sec, w, d, h,m, s

    !% Convert seconds to other units, legacy matlab code their might be a better way to do this in fortran.
    w = floor(sec/604800); ! Weeks
    sec = sec - w*604800;
    d = floor(sec/86400); ! Days
    sec = sec - d*86400;
    h = floor(sec/3600); ! Hours
    sec = sec - h*3600;
    m = floor(sec/60); ! Minutes
    sec = sec - m*60;
    s = (sec); ! Seconds

    ! Build time string
    IF (w > 0) THEN
        IF (w > 9) THEN
            WRITE(time_str, '(f3.0, a)') w, ' weeks'
        ELSE
            WRITE(time_str, '(f3.0, a, f3.0, a)') w, ' weeks and ', d, ' days'
        END IF
    ELSEIF (d > 0) THEN
        IF (d > 9) THEN
            WRITE(time_str, '(f3.0, a)') d, ' days'
        ELSE
            WRITE(time_str, '(f3.0, a, f3.0, a)') d, ' days and ', h, ' hr'
        END IF
    ELSEIF (h > 0) THEN
        IF (h > 9) THEN
            WRITE(time_str, '(f3.0, a)') h, ' hr'
        ELSE
            WRITE(time_str, '(f3.0, a, f3.0, a)') h, ' hr and ', m, ' min'
        END IF
    ELSEIF (m > 0) THEN
        IF (m > 9) THEN
            WRITE(time_str, '(f3.0, a)') m, ' min'
        ELSE
            WRITE(time_str, '(f3.0, a, f3.0, a)') m, ' min and ', s, ' sec'
        END IF
    ELSE
        WRITE(time_str, '(f6.2, a)') s, ' sec'
    END IF

    END SUBROUTINE sec2timestr

    END MODULE timer_module





