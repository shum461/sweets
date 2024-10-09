# only for tests
fd <- utils::read.csv("fake_data.csv")

# output of sweet_disposition
fd_new <- sweets::sweet_disposition(fd,subjid = USUBJID)

# sweet_disposition output from 2024-08-12
fd_old <- utils::read.csv("fd_old.csv")[-1]
