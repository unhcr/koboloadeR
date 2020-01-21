library("devtools")

revdep_check_reset()
revdep_check()

#revdepcheck::revdep_reset()

revdep_check_save_summary()
revdep_check_print_problems()
