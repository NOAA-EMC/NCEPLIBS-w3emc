! This program tests getgbexm().
program test_getgbexm
  implicit none

  character*256 cg1
  integer, parameter :: mbuf=256*1024
  integer, parameter :: jgmax=1200000
  character cbuf(mbuf)
  integer jpds(200), jgds(200), jens(200)
  integer kpds(200), kgds(200), kens(200)
  integer kprob(2), kclust(16), kmembr(80)
  real xprob(2)
  logical*1 lb(jgmax)
  real f(jgmax)
  integer iret, iretba, j, jf, k, kf, k_first
  integer lugb, lugi, lcg1, mnum, nlen, nnum
  real tolerance
  integer null_idx

  cg1='data/input/ref_gdaswave.t00z.wcoast.0p16.f000.grib1'
  lcg1=len_trim(cg1)

  lugb=11
  call baopenr(lugb,cg1(1:lcg1),iretba)
  if(iretba.ne.0) stop 1

  tolerance = 1e-4

  ! Case 1: initialize index state from the grib file and read first hit.
  jpds=-1
  jgds=-1
  jens=-1
  kpds=0
  kgds=0
  kens=0
  kprob=0
  xprob=0.0
  kclust=0
  kmembr=0
  j=-1
  lugi=0
  jf=jgmax
  call getgbexm(lugb,lugi,jf,j,jpds,jgds,jens,mbuf,cbuf,nlen,nnum,mnum, &
       kf,k,kpds,kgds,kens,kprob,xprob,kclust,kmembr,lb,f,iret)
  if(iret.ne.0) stop 2
  if(kf.le.0 .or. k.lt.0) stop 3

  print *, "kf=", kf, "k=", k
  print *, "kpds(1:10)=", kpds(1:10)
  print *, "kgds(1:5)=", kgds(1:5)
  print *, "maxval(f)=", maxval(f(1:kf)), "minval(f)=", minval(f(1:kf))
  print *, "sum(f)=", sum(f(1:kf))

  k_first=k

  ! Case 2: non-negative j should reuse previously cached state.
  jpds=-1
  jgds=-1
  jens=-1
  kpds=0
  kgds=0
  kens=0
  kprob=0
  xprob=0.0
  kclust=0
  kmembr=0
  j=0
  jf=jgmax
  call getgbexm(lugb,lugi,jf,j,jpds,jgds,jens,mbuf,cbuf,nlen,nnum,mnum, &
       kf,k,kpds,kgds,kens,kprob,xprob,kclust,kmembr,lb,f,iret)
  if(iret.ne.0) stop 4
  if(k.ne.k_first) stop 5

  ! Case 3: impossible request should return request-not-found.
  jpds=-1
  jgds=-1
  jens=-1
  jpds(1)=999
  kpds=0
  kgds=0
  kens=0
  kprob=0
  xprob=0.0
  kclust=0
  kmembr=0
  j=-1
  jf=jgmax
  call getgbexm(lugb,lugi,jf,j,jpds,jgds,jens,mbuf,cbuf,nlen,nnum,mnum, &
       kf,k,kpds,kgds,kens,kprob,xprob,kclust,kmembr,lb,f,iret)
  if(iret.ne.99) stop 6

  ! Case 4: too-small output buffer should return the length error code.
  jpds=-1
  jgds=-1
  jens=-1
  kpds=0
  kgds=0
  kens=0
  kprob=0
  xprob=0.0
  kclust=0
  kmembr=0
  j=-1
  jf=1
  call getgbexm(lugb,lugi,jf,j,jpds,jgds,jens,mbuf,cbuf,nlen,nnum,mnum, &
       kf,k,kpds,kgds,kens,kprob,xprob,kclust,kmembr,lb,f,iret)
  if(iret.ne.98) stop 7

  ! Case 5: invalid index unit should hit index-read error handling.
  jpds=-1
  jgds=-1
  jens=-1
  kpds=0
  kgds=0
  kens=0
  kprob=0
  xprob=0.0
  kclust=0
  kmembr=0
  j=-1
  lugi=99
  jf=jgmax
  call getgbexm(lugb,lugi,jf,j,jpds,jgds,jens,mbuf,cbuf,nlen,nnum,mnum, &
       kf,k,kpds,kgds,kens,kprob,xprob,kclust,kmembr,lb,f,iret)
  if(iret.ne.96) stop 8

end program test_getgbexm
