
ipm <- function(psi, ldpsi, theta, ldtheta, npoint, nsub, ijob, x, dx, y, dy, fobj, gap, nvar, keep, ihess, isupres) {

  #Windows
  #load fortran ipm library
  dyn.load("ipm_v4.dll")

  #Linux/MAC
  #dyn.load("ipm.so")
  if (!is.double(psi)) { storage.mode(psi) <- 'double' }
  if (!is.integer(ldpsi)) { storage.mode(ldpsi) <- 'integer' }
  if (!is.double(theta)) { storage.mode(theta) <- 'double' }
  if (!is.integer(ldtheta)) { storage.mode(ldtheta) <- 'integer' }
  if (!is.integer(npoint)) { storage.mode(npoint) <- 'integer' }
  if (!is.integer(nsub)) { storage.mode(nsub) <- 'integer' }
  if (!is.integer(ijob)) { storage.mode(ijob) <- 'integer' }
  if (!is.double(x)) { storage.mode(x) <- 'double' }
  if (!is.double(dx)) { storage.mode(dx) <- 'double' }
  if (!is.double(y)) { storage.mode(y) <- 'double' }
  if (!is.double(dy)) { storage.mode(dy) <- 'double' }
  if (!is.double(fobj)) { storage.mode(fobj) <- 'double' }
  if (!is.double(gap)) { storage.mode(gap) <- 'double' }
  if (!is.integer(nvar)) { storage.mode(nvar) <- 'integer' }
  if (!is.integer(keep)) { storage.mode(keep) <- 'integer' }
  if (!is.integer(ihess)) { storage.mode(ihess) <- 'integer' }
  if (!is.integer(isupres)) { storage.mode(isupres) <- 'integer' }
  #   $lambda
  #  [1] 1.172667e-13 1.172667e-13 2.000000e-01 3.000000e-01 1.173147e-13
  #  [6] 1.000000e-01 2.999733e-01 1.000267e-01 1.172688e-13 1.172667e-13

  # $fobj
  # [1] 165.7383

  #   if (length(x) != length(w)) { stop("Both vectors should have the same size") }
  
  sink("log.txt", append = FALSE, split = TRUE)
  .Call("c_emint", psi, ldpsi, theta, ldtheta, npoint, nsub, ijob, x, dx, y, dy, fobj, gap, nvar, keep, ihess, isupres)
  print(list("fobj" = fobj, "lambda" = x, "ijob" = ijob, "ihess" = ihess))
  sink()
  if(ijob<0 || ihess <0){
    n_err <- n_err + 1
    print("Error detected! Trying to recovery... #err: ")
    print(n_err)
    err_log[[n_err]] <- list("ijob"=ijob, "ihess"=ihess, "count"=count)
    if(n_err>=20){
      sink("error.txt", append = FALSE, split = TRUE)
      print(err_log)
      sink()
      stop("Unrecoverable error")
    }
  }
  return(list("lambda" = x, "fobj" = fobj, "ijob" = ijob, "ihess" = ihess))
}

burke <- function(PSI) {
  # # PSI = pr( observation | support point )
  # nsub <- rlang::duplicate(dim(PSI)[1], shallow = F)
  # npoint <- dim(PSI)[2]

  # # theta has one support point and it's probability on
  # # each row; theta is used only if ijob == 1
  # ldtheta <- 10 # dim(theta)[1]
  # nvar <- 5 # dim(theta)[2] - 1
  # theta <- matrix(c(seq(1, 60, 1)), nrow = 10, ncol = nvar + 1) #corden

  # # working arrays -- must be npoint long
  # ldpsi = nsub #is passed to low level linear algebra routines
  # x <- c(rep(1 / npoint, npoint)) # These are the returned probabilities !!!
  # dx <- c(rep(0, npoint))
  # y <- c(rep(0, npoint))
  # dy <- c(rep(0, npoint))

  # # will be reset inside of ipm; initial values here are meaningless:
  # fobj <- 10 ^ -8
  # gap <- 10 ^ -10 # will be reset inside of ipm
  # keep <- 0 # will be set > 0 inside of ipm

  # # flags that control text output or program control
  # ihess <- 0 # flags Hessian error
  # isupres <- 1 # 0 to not suppress error writes, 1 to supress error writes
  # ijob <- 0 # do not condense
  denstor <- matrix(c(seq(1, ncol(PSI) * 4, 1)), nrow = ncol(PSI), ncol = 4)
  nvar <- nrow(PSI)
  nmaxsub <- nrow(PSI)
  theta <- matrix(c(seq(1, ncol(PSI) * (nvar + 1), 1)), nrow = ncol(PSI), ncol = nvar + 1)
  #corden <- matrix(c(seq(1, ncol(PSI) * (nvar + 1), 1)), nrow = ncol(PSI), ncol = nvar + 1)
  nmaxgrd <- 1024
  nactive <- ncol(PSI)
  nsub <- nrow(PSI)
  ijob <- 0
  #corden[, nvar + 1] <- 0.2
  x <- c(rep(1 / ncol(PSI), ncol(PSI)))
  fobj <- 10 ^ -8
  gap <- 10 ^ -10
  nvar <- ncol(PSI) - 1
  keep <- 0
  ihess <- 0
  isupress <- 1

  # denstor <- matrix(c(seq(1, 20, 1)), nrow = ncol(PSI), ncol = 4)
  # nvar = nrow(PSI)
  # nmaxsub <- nrow(PSI)
  # corden <- matrix(c(seq(1, ncol(PSI) * (nvar + 1), 1)), nrow = ncol(PSI), ncol = nvar + 1)
  # nmaxgrd <- 1024
  # nactive <- ncol(PSI)
  # nsub <- nrow(PSI)
  # ijob <- 0
  # corden[, nvar + 1] <- 0.2
  # fobj <- 10 ^ -8
  # gap <- 10 ^ -10
  # nvar <- nrow(PSI)
  # keep <- ncol(PSI)

  #ipm(PSI, ldpsi, theta, ldtheta, npoint, nsub, ijob, x, dx, y, dy, fobj, gap, nvar, keep, ihess, isupres)
  ipm(as.matrix(PSI), nmaxsub, theta, nmaxgrd, nactive, nsub, ijob,
  x, denstor[, 1], denstor[, 2], denstor[, 3],
  fobj, gap, nvar, keep, ihess, isupress)
}


.test_burke1 <- function() {
  #Real data taken from the matlab example
  library(readxl)
  PSI <- read_excel('../PSI.xlsx',
                  col_names = FALSE)
  ans <- burke(as.matrix(PSI))
  print(ans)
}

.test_burke2 <- function() {
  P1 <- readRDS(file = "p1_new_data.rds")
  ans <- purrr::possibly(burke(as.matrix(P1)))
  print(ans)
}

.test_burke3 <- function() {
  P2 <- readRDS(file = "p2_data.rds")
  ans <- burke(as.matrix(P2))
  print(ans)
}
# write.table(as.matrix(P1), file = "p1.csv", row.names=F, col.names=F, sep = ",")




# true_l <- c(1.172458913793544e-13, 1.172458913793544e-13, 0.199999999999883
#             , 0.299999999999765, 1.172937884144416e-13, 0.100000000000000
#             , 0.299973334863959, 0.100026665135807, 1.172479847547782e-13
#             , 1.172458913793544e-13)
# sum(true_l)

## EXAMPLE DATA

# # to.read = file("/Users/julianotalvaro/Dev/LAPK/R-dev/Pmetrics/src/test/ipmtest01.dat", "rb")
# to.read = file("ipmtest01.dat", "rb")
# readBin(to.read, integer(), n = 1, endian = "little", size = 4)
# nrow = readBin(to.read, integer(), n = 1, endian = "little")
# ncol = readBin(to.read, integer(), n = 1, endian = "little")
# pyjgx = readBin(to.read, double(), endian = "little", n = 82 * 2129, size = 8) %>%
#       matrix(nrow = 82, ncol = 2129)
# nmaxsub = readBin(to.read, integer(), n = 1, endian = "little")
# corden = readBin(to.read, double(), endian = "little", n = 2129 * 3) %>%
#   matrix(nrow = 2129, ncol = 3)
# nmaxgrd = readBin(to.read, integer(), n = 1, endian = "little")
# nactive = readBin(to.read, integer(), n = 1, endian = "little")
# nsub = readBin(to.read, integer(), n = 1, endian = "little")
# ijob = readBin(to.read, integer(), n = 1, endian = "little")
# wtf = readBin(to.read, double(), endian = "little", n = 2129, size = 8)
# denstor = readBin(to.read, double(), endian = "little", n = 2129 * 4) %>%
#   matrix(nrow = 2129, ncol = 4)
# fobj = -2.0
# readBin(to.read, double(), n = 1, endian = "little")
# gap = readBin(to.read, double(), n = 1, endian = "little")
# nvar = readBin(to.read, integer(), n = 1, endian = "little")
# corden[, nvar + 1] <- wtf
# keep = readBin(to.read, integer(), n = 1, endian = "little")
# ihess = readBin(to.read, integer(), n = 1, endian = "little")
# #errfill = readBin(to.read, character(), n=20, endian = "little")
# isupress = 0 #readBin(to.read, integer(), n=1, endian = "little")

# setwd("~/src/pmetrics/src")

