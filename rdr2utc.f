
c RDR2UTC is template for RDR Switchout time in ns from minor frame start after 9.5 ms delay, energy, tx_PW, optionally
c spacecraft position, height, geoid etc.
c **********************************************************************/
c ************************************************************************
c double unsigned_ (unsigned int *s) return (double) *s;
c gfortran -ffixed-line-length-none -O rdr2utc.f unsigned.o kzext2.o spicelib.a -static-libgfortran -o rdr2utc
c v2.1 usage message
c v2.2 adds offndr angle
c
	IMPLICIT NONE
	real*8 unsigned, tick, tickold, d28, pi,r2d,d2r,r2de,offlo,offhi,zmin/0./,zmax /180.d0/
	data offlo/0./,offhi/25.d0/
	external unsigned
	parameter(d28=28.d0 + 1./8192.d0)!
	parameter(pi=3.141592653589794d0)
	parameter (r2d=180.d0/pi,d2r=pi/180.d0, r2de =9.d-03/pi)
c ******* input record more than a day
	integer*4 rdr(64*86400*28), iwrd, mypos !stream
	integer*2 wdr(128*86400*28)
	equivalence(rdr,wdr)
c *******
	integer*4 tdt, tdtold !Terrestrial dynamic time whole SI seconds
	integer*4 numarg, ngrd
c must be swapped
	integer*4 invalid /-2147483648/
	integer*4 kzext2,zext
	external zext,kzext2
	integer*4 MET,metold,metstart,i,j,irec,orec,itdc,lseq,ifrm,iargc,subs,hz2fire,lo,hi,lim
	integer*4 isum /0/,noissm(5)
	real*8 sumo /0./,sumpw/0./,sumtx/0./,rngmx/0./
	real*8 dlonmin /-180.0/,dlonmax/360.d0/,dlatmin/-90.d0/,dlatmax/90.d0/,zenith
	real*8 timelo/0./,timehi/7500./,alt
	character*16 name(9)/'long_min','long_max','lat_min','lat_max','time_lo','time_hi','offnadlo','offnadhi','zenithmx'/,chr
	real*8 fred(9)
	equivalence(dlonmin,fred(1)),(dlonmax,fred(2)),(dlatmin,fred(3)),(dlatmax,fred(4)),(timelo,fred(5)),(timehi,fred(6)),(offlo,fred(7)),(offhi,fred(8)),(zmax,fred(9))
	real*8 offset,fract, fractold,time,geoid,xplse,xenrg,earle,earce,earpw
	real*8 solinc,solphs,soltim,offndr,emission
	real*8 c32 /4294967296.d0/
	real*8 tmp,et,tt,sclkdp, orig /0./
	real*8 sclon,sclat,scrad,ref
	real*8 radius/1737.4d0/
	real*8 gain(5),thrs(5),bkgd(5),plse(5),rnge(5),enrg(5)
	real*8 dlon(5),dlat(5),drad(5)
	integer nois(5),gflg(5)
	real*8 rflct(5), trans(5),dark(5),fiber(5)!area-transm. product
	data dark /0.135d0,0.1159d0,0.1303d0,0.1775d0,0.2164d0/
	integer*4 buf,rgstart,rgstop
c minor frame counts as fractions of 5000000.d0
	integer*4 minor(0:27)
	real*8 dscale/1.d-7/,gscale/1.d-6/, escale/1.d-6/
	real*8 pscale/1.d-3/,rscale/1.d-6/, tscale/1.d-6/
	character*16 arg
	character*28 utc
        character*128 filename
	character*128 limitfile /'r2x.prm'/
c	logical swap /.false./,head/.false./,all/.false./,geop/.false./ !change for rdr2taba
	logical swap /.false./,head/.true./, all/.true./,geop/.false./
	logical flag/.false./,spot(0:5)/6*.true./,oflag/.false./,limit/.false./,flagt/.false./
	logical flagq/.false./,geographic/.true./,flagu/.true./,flagv/.false./,flagw/.false./
c	integer flagd /0/ !everything goes
	integer flagd /255/ !any condition matters

c fiber, and aft optics efficiency in percent
c	data fiber /84.045654,87.437592,85.325625,90.273744,80.809245/
c diffractive optics splits energy into 5 spots, more in the center
c these should be symmetrical but owing to measurement imprecision,
c separate values are given in the cal rpt. This really needs to be flatfielded!
c	data trans /0.22d0,0.14d0,0.14d0,0.14d0,0.14d0/!nominal
c initialize optical link, transmission times rx telescope radius 0.07 squared
cc	do i=1,5
c	 trans(i)=trans(i)*0.01d0*fiber(i)*0.0049d0
c	enddo
c initialize minor frame offsets - use to check range window against time
	minor(0)=0
	do i=1,16
	 minor(i)=minor(i-1)+178571
	enddo
	do i=17,27
	 minor(i)=minor(i-1)+178572
	enddo
	numarg=iargc() !flag for headers, swapping
	if(numarg.eq.0) then
	 write(*,*)'rdr2utc 2.2 - LOLA reduced data record to 1-Hz table'
	 write(*,*)'Averages of switchout time from pump to fire, Transmit energy, PW, range to surface, solar incidence'
	 write(*,*)'5-detector noise counts, detector thresholds, channel gain setting'
	 write(*,*)'usage: rdr2csv <rdrfile> [<h|H>eader]'
	 goto 99999
	endif
c you don't need most of these arguments for housekeeping, but the limits file can be useful if looking at a specific area or solar time.
	do i=2,numarg
	 call getarg(i,arg)
c	 if(arg(1:1).eq.'s' .or. arg(1:1).eq.'S') swap=.true.
c	 if(arg(1:1).eq.'d' .or. arg(1:1).eq.'D') geographic=.false. !dateline
	 if(arg(1:1).eq.'h' .or. arg(1:1).eq.'H') head=.not.head
ccc	 if(arg(1:1).eq.'f' .or. arg(1:1).eq.'F') flagd=255 ! any flag
	 if(arg(1:1).eq.'l' .or. arg(1:1).eq.'L') then
	  limit =.true.
	  if(len_trim(arg).gt.1) limitfile = arg(2:)
	 endif
c	 if(arg(1:1).eq.'m' .or. arg(1:1).eq.'M') flagd=255-64 !manual flag only
c	 if(arg(1:1).eq.'n' .or. arg(1:1).eq.'N') flagd=254 !neumann's auto flag only
c output bit 6 disagrees with bit 0 -failure of neumann's own?
c	 if(arg(1:1).eq.'q' .or. arg(1:1).eq.'Q') flagq=.true.!quarrel-matlab flag, auto flag
	 if(arg(1:1).eq.'u' .or. arg(1:1).eq.'U') flagu=.not.flagu!utc output
c	 if(arg(1:1).eq.'v' .or. arg(1:1).eq.'V') flagv=.true. !verbose
c	 if(arg(1:1).eq.'t' .or. arg(1:1).eq.'T') flagt=.true.!transmit
c	 if(arg(1:1).eq.'w' .or. arg(1:1).eq.'W') flagw=.true.
c	 if(arg(1:1).eq.'w' .or. arg(1:1).eq.'W') flagd=flagd+2**15 !weird
c	 if(arg(1:1).eq.'c' .or. arg(1:1).eq.'C') flagd=flagd+2**10 !check for C-kernels
c	 if(arg(1:1).eq.'g' .or. arg(1:1).eq.'G') geop=.true.
c	 if(arg(1:1).eq.'o' .or. arg(1:1).eq.'O') oflag=.true. !MET origin

c	 if(arg(1:1).eq.'a' .or. arg(1:1).eq.'A') all =.true.
c	 if(arg(1:1).eq.'0'       ) spot(0) =.true.
c	 if(arg(1:1).eq.'1'.or.all) spot(1) =.true.
c	 if(arg(1:1).eq.'2'.or.all) spot(2) =.true.
c	 if(arg(1:1).eq.'3'.or.all) spot(3) =.true.
c	 if(arg(1:1).eq.'4'.or.all) spot(4) =.true.
c	 if(arg(1:1).eq.'5'.or.all) spot(5) =.true.
	enddo
c SPICE needs these if you uncomment et2utc
	if (flagu) call ldpool('/Users/ganeuman/naif0012.tls')
c ======= limits
	if(limit) then
	open(unit=1,file=limitfile,status='old',err=3)
	 read(1,*,end=2) dlonmin,dlonmax,dlatmin,dlatmax,timelo,timehi,offlo,offhi,zmax,zmin
2	close(1)
	goto 4
3	continue
	write (0,*)'limit file [e.g., r2x.prm] not found'
	stop
4	continue
	if(dlonmin.ge.dlonmax) then
	 tmp=dlonmax
	 dlonmax=dlonmin
	 dlonmin=tmp
	endif
	if(dlatmin.ge.dlatmax) then
	 tmp=dlatmax
	 dlatmax=dlatmin
	 dlatmin=tmp
	endif
	lo=timelo
	hi=timehi
	endif
	if(flagv) then
	do i=1,9
	 write(*,6) name(i),' = ', fred(i)
	 write(*,*)
	enddo
	endif
6	 format(a,a,f10.3," ", $)
check if output specified
c	if(.not.(spot(0).or.spot(1).or.spot(2).or.spot(3).or.spot(4).or.spot(5)))then
c	 write(0,*)'Must use at least one option (0, 1, 2, 3, 4, 5, or a)'
c	 goto 99999
c	endif
c	if (flagq.and.flagu) then
c	 write(*,*) 'inconsistent usage of q(uarrel) and u(nflagged)'
c	 stop
c	endif 
        if(numarg.gt.0) call getarg(1,filename)
c rdr?
c	open(unit=1,file=filename,recl=256,access='direct'
c     & ,status='old',err=9999)
	open(unit=1,file=filename,access="stream"
     & ,status='old',err=9999)
	read(1,end=30) rdr  !get up to 7500 seconds
30	inquire(unit=1, POS=mypos)!points to next byte.
	close(1)
        irec=0
c  sum the noise counts for 28 records
	if(head)
     & write(*,'(a,a,a)')
     &' SCLK_LOLA, TDT_Seconds,   Switchout_ns,  N,  Txenrg,    TxPw,   rngmx'
     & ,',  solinc, offndr, nois1,  nois2,  nois3,  nois4,  nois5, thrs1, thrs2, thrs3, thrs4, thrs5'
     & ,', gain1, gain2, gain3, gain4, gain5,    sclon,    sclat, UTCdate_time'
c first record
	MET=rdr(1)
	metold=met ! for averaging, save the first MET and fraction
	fract=unsigned(rdr(2))/c32 !cast fraction of met seconds to real*8
	fractold=fract
	tdtold=rdr(3)
	tickold=unsigned(rdr(4))/c32
c	write(0,*) tdtold,tickold,tt
c	lim = MET
c	sclkdp = MET+fract
c offset from 2000T12:00:00 to 2001T00:00:00 adopted by LRO is added
        if (flagu) then
	 tt=tdtold+tickold
	 call et2utc (tt,'isod',5,utc)
c	 write(0,*) utc
	endif
c set origin time to MET, otherwise to start of orbit
c	if(oflag) orig=sclkdp
	ref=radius
c	write(0,'(a,i12,f18.6,i12,1x,a28)')'MET,orig,lim',MET,orig,lim
c ================================================================== big loop
1       continue
	iwrd=64*irec !offset to current record, initially zero
        irec=irec+1 ! record number
	if(irec.gt.mypos/256) goto 999
c the 'old' values will be output
	MET=rdr(iwrd+1)
c	write(0,*) MET - lim, lo, hi
c check time from start of orbit
	if (.not. limit .or.
     &   (MET - lim .ge.lo .and. MET -lim .lt. hi)
     &  ) then

	fract=unsigned(rdr(iwrd+2))/c32 !cast fraction of met seconds to real*8
c	geoid = rscale*rdr(iwrd+10)
c	if(geop) ref=geoid
c	sclkdp = MET+fract (is this right?)
c offset from 2000T12:00:00 to 2001T00:00:00 adopted by LRO is added
c	ET = tdt/c32 + 64.184d0 !plus a small periodic term
c        call et2utc (et,'isod',5,utc)
c	write(0,'(a,2f18.6,1x,a28)')'LMET,TDT',lmet/c32,tdt/c32,utc
	xenrg = escale*rdr(iwrd+5) 
	xplse = pscale*rdr(iwrd+6) 
	sclon = dscale*rdr(iwrd+7)
	if(geographic .and. sclon.lt.0.) sclon=sclon+360.d0
	sclat = dscale*rdr(iwrd+8)
	scrad = rscale*unsigned(rdr(iwrd+9))
	offndr=r2de*kzext2(wdr(2*iwrd+121))!always found
	zenith = r2de*kzext2(wdr(2*iwrd+123))
	if(wdr(2*iwrd+122).ne. -1) then
	 emission=r2de*kzext2(wdr(2*iwrd+122))
	else
	 emission=-1.d0! or zero?
	endif
	if(wdr(2*iwrd+123).ne. -1) then
	 solinc=r2de*kzext2(wdr(2*iwrd+123))
	else
	 solinc=-1.d0
	endif
	if(wdr(2*iwrd+124).ne. -1) then
	 solphs=r2de*kzext2(wdr(2*iwrd+124))
	else
	 solphs=-1.d0
	endif
c truncate 28*fract and get fractional time of fire 9.5 ms command
 	ifrm = int(d28*fract)
	offset=9.5d-3 + minor(ifrm)/5000000.d0 ! per Igor 7/12/2020
c convert from radians to hours
c multiply by 360/2pi times 10^-4 to rescale integers 0-62831.853
c divide by 15 to get hours
	 soltim= (2.*r2de*kzext2(wdr(2*iwrd+127)))/15.d0

	if(wdr(2*iwrd+128).ne. -1) then
	 earpw=pscale*kzext2(wdr(2*iwrd+128))
	else
	 earpw=99.d0
	endif
	ngrd=0 !if flagd mask is nonzero, this counts ground returns, otherwise any triggers
	do i=1,5
	 if(iand(rdr(iwrd+i*10+10),flagd).eq.0.and.rdr(iwrd+i*10+4).ne.-1) ngrd=ngrd+1
	enddo
c **********************************************************************/
check tx pw for laser fire, otherwise skip.
c or do we output all records  average regardless of laser status?
c average for single MET is posted to the beginning of the frame after reading 28 records
	if (met.ne.metold) then !new frame
	  tdt =rdr(iwrd+3)
	  tick=unsigned(rdr(iwrd+4))/c32
	  write(*,1201) metold,tdtold,tickold,sumo/isum, isum, sumtx/isum, sumpw/isum, rngmx, solinc,offndr,noissm, thrs, gain,sclon,sclat
c tricky leading zero suppress to concatenate tdt and tick
1201	format(i10,",",i10,f0.6,",",9p,f10.1,",",i3,",",0p,2(f8.3,","),f8.2,",",f8.2,",",f8.2,",",5(i7,","),10(f6.2,","),f9.4,",",f9.4,$)
	if (flagu) then
	 tt=tdtold+tickold
	 call et2utc (tt,'isod',5,utc)
	 write(*,1202) utc
	else
	 write(*,'(a)')"," !end string
	endif
c setup
	 metold=met !first shot of next frame
	 tdtold=tdt
	 tickold=tick
c reset for average of switchout times, transmit energies, pulse widths
	 isum=0
	 sumo=0.
	 sumtx=0.
	 sumpw=0.
	 noissm(1)=0
	 noissm(2)=0
	 noissm(3)=0
	 noissm(4)=0
	 noissm(5)=0
	endif
c ***** now spots to calculate rngmx
	if( rdr(iwrd+6) .gt. 0.) then
	isum = isum +1
	sumo = sumo + fract-offset
	sumtx= sumtx+xenrg
	sumpw= sumpw+xplse
	if(.not.limit.or.
     &      (     sclon.ge.dlonmin .and. sclon.le. dlonmax
     &      .and. sclat.ge.dlatmin .and. sclat.le. dlatmax
     &      .and.  offndr.ge.offlo .and. offndr.le. offhi 
     &      .and. zenith.le.zmax .and. zenith .ge.zmin  )
     &  ) then
c
	endif !limit
	endif !spot 0
c====main spot loop=====
	do i=1,5
	 nois(i) =     rdr(iwrd+i*10+7)
	 thrs(i) = tscale*rdr(iwrd+i*10+8)
	 gain(i) = gscale*rdr(iwrd+i*10+9)

	 noissm(i)=noissm(i)+nois(i)

	 flag =(iand(rdr(iwrd+i*10+10),flagd).eq.0)
c	 if(flagq) flag=flag.and.(iand(rdr(iwrd+i*10+10),64).ne.iand(rdr(iwrd+i*10+10),1)*64)!bit 6 differs from bit 0
c	 if(flagw) flag=flag.or.(iand(rdr(iwrd+i*10+10),32768).ne.0)!bit 15
c	 if(flagt) flag=flag.or.(iand(rdr(iwrd+i*10+10),24).eq.0)!transmit bits
check output desired switches, valid geolocation, valid range
c**********dumb programming *********** these range/
	 if((flag.or.flagv).and.spot(i)
     & .and.(flagv.or.rdr(iwrd+i*10+1).ne.invalid)
     & .and.(flagv.or.rdr(iwrd+i*10+3).ne.-1)
     & .and.(flagv.or.rdr(iwrd+6).gt.0) !xplse
     & ) then
c**********dumb programming ***********
	 dlon(i) = dscale*rdr(iwrd+i*10+1)
	 if(geographic .and.dlon(i).lt.0.) dlon(i)=dlon(i)+360.d0
	 dlat(i) = max(-99., dscale*rdr(iwrd+i*10+2))!bad value format protector
c
c  values other than 0 should be regarded as an invalid measurement.
c   bit 0 = ground/not ground
c   bit 1 = xmt LE
c   bit 2 = xmt TE
c   bit 3 = rcv LE
c   bit 4 = rcv TE
c   bit 5 = xmt energy invalid
c   bit 6 = autoedit flag
c   bit 7 = Pointing not found from spacecraft C-Kernel
c   bit 10= TDC status invalid, rare but serious
c   bit 11= set to 1 if signal NOT acquired by FSW on last packet
c   bits 12-15 are reserved for manual editing and give reasons bit 0 is set
c These are not always recorded by the manual editing software.
c   bit 12= failed n-sigma edit criteria on altitude
c   bit 13= failed slope edit criteria
c   bit 14= just bad
c   bit 15= weird

	if(.not.limit.or.flagv
     &      .or.( dlon(i).ge.dlonmin .and. dlon(i).le. dlonmax
     &      .and. dlat(i).ge.dlatmin .and. dlat(i).le. dlatmax
     &      .and.  offndr.ge.offlo   .and. offndr.le. offhi .and. zenith.le.zmax )
     &  ) then

	 if(rdr(iwrd+i*10+3).ne.-1) then
	  drad(i) = rscale*unsigned(rdr(iwrd+i*10+3))
	  alt = max(-999.999d0,drad(i)-ref)
	 else
	  alt = -999.d0
	 endif
	 rnge(i) = rscale*unsigned(rdr(iwrd+i*10+4))
	 rngmx=max(rngmx,rnge(i))
	 plse(i) = pscale*rdr(iwrd+i*10+5)
	 enrg(i) = escale*rdr(iwrd+i*10+6)
	 thrs(i) = tscale*rdr(iwrd+i*10+8)
	 gain(i) = gscale*rdr(iwrd+i*10+9)
	 gflg(i) = iand(255+1024+8192+32768,rdr(iwrd+i*10+10))!bits 0-7,10,13,15
c	 gflg(i) = iand(  1,rdr(iwrd+i*10+10))
c reflectivity
c	 if(flag .and. xenrg.gt.0. .and. rnge(i).lt.320.d0) then !patch for radiometry
c trans(i) contains aperture, transmission efficiency, etc
c scale factor accounts for km^2, mJ to fJ
c	  rflct(i)=max(0.,rnge(i)**2 *escale*(enrg(i)-dark(i))/(xenrg*trans(i)))
c	 else
c	  rflct(i)=0.d0
c	 endif
c	write(*,1005)
c     &  sclkdp-orig,alt,i,dlon(i),dlat(i),rnge(i)
c     &  ,enrg(i),nois(i),thrs(i),gain(i),gflg(i),rflct(i)
c     &  ,plse(i),soltim
c1004	format
c     & (f14.3,f10.6,i2,2f11.6,f10.3,f9.4,i6,f8.3,f8.3,i6,f10.5,f9.2)
c1005	format
c     & (f14.3,f10.4,i2,2f11.6,f10.3,f9.4,i6,f8.3,f8.3,i6,f10.5,f9.2,f9.4,f9.1)
c
	 endif !lon/lat limit
	 endif !flag
	enddo
	endif !met
        goto 1
999     continue
c cleanup last record
c	write(*,1201) met,tdtold,tickold,sumo/isum, isum, sumtx/isum, sumpw/isum, rngmx, solinc,noissm, thrs, gain,sclon,sclat
        write(*,1201) met,tdtold,tickold,sumo/isum, isum, sumtx/isum, sumpw/isum, rngmx, solinc,offndr,noissm, thrs, gain,sclon,sclat

	if (flagu) then
	 tt=tdtold+tickold
	 call et2utc (tt,'isod',5,utc)
	 write(*,1202) utc
	else
	 write(*,'(a)')"," !end string
	endif
1202	format(", ",a25)
       close(1)
	goto 99999
9999	write(0,*)'file not opened: ',filename
99999   end
