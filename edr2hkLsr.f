c edr2hkLsr 3424-byte format -> housekeeping at 1-s intervals
c V2.0 
c gfortran -ffixed-line-length-none -O edr2hkLsr.f zext.o kzext2.o kzext3.o ksext3.o jzext.o -o edr2hkLsr
	implicit none
	integer hksize,mypos,iwrd,lsr,numarg
	parameter (hksize=7500*1712)
	integer*2 hkdata(hksize)
        integer*1 block(3424)! 4*856 is the whole edr structure, irec is 1-s, hkdata is 1712 half-words
c 12 byte tlm, apid len seq met! 20 bytes
        integer*2 word(1712), mt(2),lmt(2)
        integer*4 met,lmet,intbuf(856)!record size
        integer*4 kzext3,ksext3,subs,hz2fire,fire_width,kzext2
c        external jzext,zext,kzext3,ksext3
	integer*4 zext,jzext
	equivalence (word,intbuf)
        equivalence (block,intbuf)
	logical oflag/.false./,head/.false./
	integer orig /0/
	integer*4 TX_PHS
	integer TX_FINE1,TX_FINE2,TX_FINE3,TX_COARSE
	integer RX_COARS,RX_FINE1,RX_FINE2,RX_FINE3
	integer EAR_COARS,EAR_FINE1,EAR_FINE2,EAR_FINE3
c
	REAL*8 TX_LE,TX_PW,TX_CE,RX_LE,RX_PW,RX_CE,EAR_LE,EAR_PW,EAR_CE,E
	REAL*8 duty ! percentage of duty_cycle
c bitmasks for channels
	integer*1 bTX,bERX
	integer*1 bRX(5)
	data bTX/1/,bERX/4/
	data bRX/2, 8, 16, 32, 64/
c bad le/te/pw counters
	integer irx(5)/5*0/,ick/0/,itx/0/,iear/0/,itdc/0/,tdcflag/0/
C
c local structures to align or unscramble b1,b0,b3,b2
        equivalence (mt, met)  !CCSDS MET
        equivalence (lmt,lmet) !LOLA MET
	integer*4 i /0/! 28 shot loop variable
	integer*4 buf,rgstart,rgstop,dutycycle,alg_mode,drive_width,irec,j,l,mtick,lun_sig,ert_sig
        character*128 filename /'LOLAEDR091940158.DAT'/
	character*64 filetsc/'/Users/neumann/LOLA/lro_sclk.tsc'/
	character*16 arg

	integer*2 ROM_CRC
	equivalence(ROM_CRC,word(58))
c
	character*6 lsrcnfst /'ENABLD'/
	character*6 lsrfires /'NOFIRE'/
	character*3 algmod /'STA'/
	character*6 lunsig /'NO_SIG'/
	character*6 ertsig /'NO_LRS'/
c this section is a mess! Not much of this is needed for edr dump.
c integral 64-bit arithmetic preserving precision at the 233 picosecond level
	integer*8 lmtime,mtime,tai2tdt,stcf,leap!met and fractions; offset
	integer*4 ltime(2),xmtime(2), tai(2), swtstcf(2)
	integer*4 leapsecond(2)
	equivalence (leap,leapsecond)
	equivalence(ltime,lmtime)
	equivalence(stcf,swtstcf) !offset to tdt as 8 byte integer
	equivalence(mtime,xmtime)
	equivalence(tai2tdt,tai)
	integer*2 jtime(4)
	equivalence(jtime,xmtime) ! to get fraction as 16-bit
c@@
c	data tai /790273982, 32/!32.1839999999 seconds for 2009
c	data leapsecond /0,31579234/ !for 2009,smallendian equals 365.5 days plus 34 seconds
c	data swtstcf /0, 0/!from apid_05
c	character*18 sclkch
c	character*32 utc  !big enuf?
c	data sclkch /'1/0220881601.00000'/
c@@ time stuff
	real*8 TBD /0.02815d0/ ! tof bit delta, ns
	real*8 et, C,di_curr,tx_energy
	integer LRO /-85/
	integer noise(5),clkcfg
	integer*1 LEA_BYTE
	equivalence (block(14),LEA_BYTE)
	real*8 etx, toff_tx_ns
cthese are subtracted from nominal times
        real*8 txphsle(0:1)
     &      /0.,0.2/ !correction for Leading edge offset TIU

        real*8 txphste(0:1)
     &      /2.22,1.83/ !correction for Trailing edge offset TIU

c temperature function
	real*4 Y,BEAM_EXPANDER_MIDDLE,BEAM_EXPANDER_TOP,BEAM_EXPANDER_BOTTOM
	real*4 RX_TUBE_BOTTOM,RX_TUBE_MIDDLE,RX_TUBE_TOP,HOUSING_TEMP,CALIBRATION_HIGH_TEMP,CALIBRATION_LOW_TEMP
	real*4 DUA_TEMP,DUA_HOT1_TEMP,DUA_FPGA_TEMP,DUA_HOT2_TEMP
	real*4 PCA_T, ANBD_T, OSC_T, DUBD_T
	real*4 DIODE_1T,DIODE_2T,y1,y2,LASER_2_BENCH,LASER_1_BENCH
	real*4 HYB_TEMP_1,HYB_TEMP_2,HYB_TEMP_3,HYB_TEMP_4,HYB_TEMP_5
	real*4 DTB_TEMP_1,DTB_TEMP_2,DTB_TEMP_3,DTB_TEMP_4,DTB_TEMP_5
	real*4 V550,V5,V12,V3PT3D,VZERO,V5NEG,V3CUR,V3CRD,V1PT5,V12CU,V1P5D,V1P5V
	real*4 V3DOT3A_MON,V3DOT3D_MON,ZERO_CHECK,V550_MON,V5NEG_MON,V5_MON,V12_MON,V3PT3A,V3PT3
	real*4 YT, YC, TX_THRESHOLD_READ_BACK, DIODE_CURRENT_SET
	real*4 V3DOT3A_DU_CURRENT_IMON,V3DOT3D_DU_CURRENT_MON,V1DOT5_DUA_CURRENT_IMON,V12_DU_CURRENT_IMON
	real*4 V1DOT5_DUA_VMON,V1DOT5_DUD_CURRENT_IMON,V1DOT5_DUD_VMON
	real*4 LEA_BOARD_TEMP,LASER_2_DIODE,LASER_1_DIODE
c ************** Statement functions
	C(L) = 0.4281*L -5.117 ! LSR_DIODE_PUMP_CURRENT for x in [12,255] from dbx LOLASERDIODPUMPI_ALG.
c tx_energy
	E(L)= 0.01435*L -0.17  ! LOTXPULSEENERGY_ALG
c
	Y(L) = ((-1.030E-05*L + 4.011E-03)*L - 8.309E-01)*L + 8.034E+01!LOTEMP
c 
	Y1(L) = (7.949E-06*L  - 1.036E-02)*L + 1.649E+01 ! degrees C."
	Y2(L) = (-2.142E-06*L - 9.013E-03)*L + 2.303E+01 ! degrees C."
	YT(L) = 2.079*L -25.02 ! tx threshold
	YC(L) = 1.319E-01*L + 5.820E+01
c***** voltage monitors
	V550(L) = 3.0926*L-37.363
	V5(L)	= 2.1646E-02*L-2.5956E-01
	V12(L)	= 5.120E-02*L - 6.055E-01
	V3PT3(L)= 1.452E-02*L-1.747E-01
	VZERO(L)= 0.01083*L-0.1303
	V5NEG(L)= -2.167E-02*L +2.606E-01
	V3CUR(L)= 1.0701E-02*L - 1.3913E-01!Analog
	V3CRD(L)= 1.0665E-02*L - 1.3963E-01!Digital
	V1PT5(L)= 4.154E-03*L - 1.626E-01  !Analog voltage
	V12CU(L)= 1.0614E-02*L - 1.1528E-01
	V1P5D(L)= 1.989E-03*L - 5.376E-02 
	V1P5V(L)= 1.084E-02*L - 1.297E-01
c*****
	numarg=iargc() !flag for headers, swapping
        if(numarg.gt.0) then
	 call getarg(1,filename)
	 open(unit=1,file=filename,access="stream"
     & ,status='old',err=9999)
	else
	 write(0,*) 'edr2hk <filename> [<toggle_header>]'
	 stop
	endif
	do i=2,numarg
	 call getarg(i,arg)
	 if(arg(1:1).eq.'o' .or. arg(1:1).eq.'O')oflag=.not.oflag
	 if(arg(1:1).eq.'h' .or. arg(1:1).eq.'H') head=.not.head
 	enddo

	read(1,end=1) hkdata  !get up to 7500 seconds
1	inquire(unit=1, POS=mypos)!points to next byte.
	close(1)
        irec=0
	if(head) then
	write(*,'(16A)')'MET_TIME,',' Lsr, Enab, Fire, FireWid'
     & , ', RX1, RX2, RX3, RX4, RX5, V550, V5, V12, V3.3D, V3.3A, V5NEG' !out-of-order
     & , ', D_CURR, TX_TH, DIODE1T, DIODE2T'
     & , ', DU3D3IMONA, DU3D3IMOND, DU1D5IMONA, DU12IMON, DU1D5VMONA, DU1D5IMOND' !out-of-order
     & , ', LEA_T, L1DI_T, L2DI_T, L1B_T, L2B_T, PCA_T, ANBD_T'
     & , ', BX_BOT, BX_MID, BX_TOP, RX_BOT, RX_MID, RX_TOP'
     & , ', HSNG_T'
     & , ', Av_Transit, LSignal, LEst_Rng, LRet_Ct'
     & , ', TX_0Dup, TX_14Dup, Ldrv_MX, Ldrv_MN, Ldrv_AV'
	endif
c skips block(109) which is always "LOLA K"
10       continue
	j=0
	iwrd=1712*irec !offset to current record for RealTime
	do i=1,1712
	 word(i)=hkdata(iwrd+i)
	enddo
        irec=irec+1 !!! increment pointer
C start of LOLA EDR
	lmt(1)=jzext(word(1))!B1,B0 are swapped
	lmt(2)=jzext(word(2))!B3,B2 are swapped
c	write(*,'(2i10,$)')lmt(1),lmt(2)
	if(oflag.and.irec.eq.1) orig=lmet
c===============
c in RDR, mtime=lmtime+stcf which is read from apid05 for the current jammed time.
c in EDR we do not really have the kernels to do sclkch calculation, so skip this
c	mtime = lmtime + tai2tdt + leap !convert to tdt
c	mtick = kzext2(jtime(2)) ! major ticks, not swapped with jzext
c	write(unit=sclkch(3:18),fmt='(i10.10,".",i5.5)') xmtime(2),mtick
c	call scs2e(LRO, sclkch, et)
c	call et2utc (et,'isoc',3,utc)
c utc can be calculated if a bunch of kernels are loaded but that is derived data
c =================
c LRO MET (or start of pass), packet sequence, phase A/B locks in ASCII_Numeric_ Base16
        write(*,'(i9,",",$)')LMET-orig
c	write(*,'(i9,",",$)')LMET-orig+326916397
c UART error (count and Conf register, see LOLAEDR.FMT)
c	write(*,'(1x,z2.2,",",$)') block(9)! ASCII_Numeric_Base16
c LEA discretes:
c    The bitwise functional breakdown is
c     0  - laser state (0=disable, 1=enable)
c     1  - laser select (0=Laser 1, 1=Laser 2)
c     2  - laser fire state (1=enable) (momentary)
c     3  - TEC 1 state (0=disable, 1=enable)
c     4  - TEC 2 state (0=disable, 1=enable)
c     5  - cpu reset enabled (0=disable, 1=enable)
c     6  - cpu reset state (0=idle, 1=active)
	if     (btest(LEA_BYTE,0)) then  !laser state corrected /gan/
	 lsrcnfst="ENABLD"
	else
	 lsrcnfst="DISABL"
	endif
	 if (btest(LEA_BYTE,1)) then
	  lsr = 2
	 else
	  lsr = 1
	 endif
	if     (btest(LEA_BYTE,2)) then
	 lsrfires="FIRING"
	else
	 lsrfires="NOFIRE"
	endif

	if(block(10).lt.0) then
	 dutycycle=ksext3(block(10),block(11), block(12))
	else
	 dutycycle=kzext3(block(10),block(11), block(12))
	endif
c convert to percentage: add 199993, divide by 199993, float in percent
	duty=(50.d0/199993.d0)*(dutycycle+199993)
	if(duty.gt.100.d0) duty=-1.d0
	drive_width=jzext(word(8))!bytes 15,16
	rgstart=kzext3( block(17),block(18), block(19))
	rgstop =kzext3( block(20),block(21), block(22))
c the LEA high order bits don't change
	write(*,'(i2,",",a7,",",a7,",",$)')lsr,lsrcnfst,lsrfires
c commanded thresholds and gains detectors 1-5
c	write(*,'(5(i4,",",i3,","),$)')
c    &   zext(block(23)),zext(block(24))
c    &  ,zext(block(25)),zext(block(26))
c    &  ,zext(block(27)),zext(block(28))
c    &  ,zext(block(29)),zext(block(30))
c    &  ,zext(block(31)),zext(block(32))
	hz2fire = kzext3(block(35),block(34),block(33))!LSB integer
c	write(*,'(i9,", ''",b5.5,"'',",$)') hz2fire,block(36)
	fire_width=kzext3(block(37),block(38),block(39))
c	clkcfg=iand(zext(block(40)),15)
	write(*,'(i6, ",",$)')fire_width	
c mask clock config to bits 0-3;minor_frame_number;tx_clamp
c rxenrgies at minor frame are block(44),block(43),block(46),block(45),block(48)
	write(*,'(5(i3,","),$)')zext(block(44)),zext(block(43)),zext(block(46)),zext(block(45)),zext(block(48))
c v monitors block(47), 49 to 54
	V550_MON   = V550(zext(block(47)))
	V5_MON	   = V5(zext(block(49)))
	V12_MON	   = V12(zext(block(50)))
	V3DOT3D_MON= V3PT3(zext(block(51)))
	V3DOT3A_MON= V3PT3(zext(block(52)))
	ZERO_CHECK = VZERO(zext(block(53)))
	V5NEG_MON=V5NEG(zext(block(54)))
	write(*,'(f7.1,",",5(f6.2,","),$)')
     & V550_MON,V5_MON,V12_MON,V3DOT3D_MON,V3DOT3A_MON,V5NEG_MON
c gain readbacks 56,55,58,57,60 are calibrated in RDR. We provide raw values here.
c	write(*,'(5(i3,","),$)')zext(block(56)),zext(block(55)),zext(block(58)),zext(block(57)),zext(block(60))
c threshold readbacks 59,62,61,64,63 are calibrated in RDR. tx is 66
c	write(*,'(5(i4,","),$)')zext(block(59)),zext(block(62)),zext(block(61)),zext(block(64)),zext(block(63))

	DIODE_CURRENT_SET=YC(zext(block(65)))
	TX_THRESHOLD_READ_BACK=YT(zext(block(66)))
	write(*,'(F6.2,",",F7.2,",",$)') DIODE_CURRENT_SET,TX_THRESHOLD_READ_BACK
c temperatures 68,67;75,77
	DIODE_1T = Y1(zext(block(68)))
	DIODE_2T = Y2(zext(block(67)))
	write(*,'(2(f8.2,","),$)')DIODE_1T,DIODE_2T
	V3DOT3A_DU_CURRENT_IMON=1.0701E-02*zext(block(69)) - 1.3913E-01
	V3DOT3D_DU_CURRENT_MON =1.0665E-02*zext(block(70)) - 1.3963E-01!
	V1DOT5_DUA_CURRENT_IMON= 3.709E-03*zext(block(71)) - 1.398E-01
	V12_DU_CURRENT_IMON    =1.0614E-02*zext(block(72)) - 1.1528E-01
	V1DOT5_DUA_VMON        = 1.084E-02*zext(block(76)) - 1.297E-01 !LODUVOLT
	V1DOT5_DUD_CURRENT_IMON= 2.202E-03*zext(block(74)) - 4.141E-02 !
c	V1DOT5_DUD_VMON        = 1.084E-02*zext(block(76)) - 1.297E-01 !LODUVOLT
	write(*,'(6(f6.3,","),$)')
     & V3DOT3A_DU_CURRENT_IMON,V3DOT3D_DU_CURRENT_MON,V1DOT5_DUA_CURRENT_IMON,V12_DU_CURRENT_IMON
     &,V1DOT5_DUA_VMON,V1DOT5_DUD_CURRENT_IMON
c copy and paste error for dtb
c hybrids 78,80,82,84,86
	HYB_TEMP_1 = Y(zext(block(78)))
	HYB_TEMP_2 = Y(zext(block(80)))
	HYB_TEMP_3 = Y(zext(block(82)))
	HYB_TEMP_4 = Y(zext(block(84)))
	HYB_TEMP_5 = Y(zext(block(86)))
c	write(*,'(5(f8.2,","),$)')
c    & HYB_TEMP_1,HYB_TEMP_2,HYB_TEMP_3,HYB_TEMP_4,HYB_TEMP_5
c boards 75,77,79,81,83
	DTB_TEMP_1 = Y(zext(block(75)))
	DTB_TEMP_2 = Y(zext(block(77)))
	DTB_TEMP_3 = Y(zext(block(79)))
	DTB_TEMP_4 = Y(zext(block(81)))
	DTB_TEMP_5 = Y(zext(block(83)))
c	write(*,'(5(f8.2,","),$)')
c     & DTB_TEMP_1,DTB_TEMP_2,DTB_TEMP_3,DTB_TEMP_4,DTB_TEMP_5
	LEA_BOARD_TEMP=Y(zext(block(85)))
	LASER_2_DIODE=Y(zext(block(87)))
	LASER_1_DIODE=Y(zext(block(88)))
	LASER_2_BENCH=Y(zext(block(89)))
	LASER_1_BENCH=Y(zext(block(90)))

	PCA_T        =Y(zext(block(91)))
	ANBD_T       =Y(zext(block(92)))
	OSC_T        =Y(zext(block(93)))
	DUBD_T       =Y(zext(block(94)))

	write(*,'(7(f8.3,","),$)')LEA_BOARD_TEMP,LASER_1_BENCH,LASER_2_BENCH,LASER_1_DIODE,LASER_2_DIODE,PCA_T,ANBD_T
c lola tubes - could have switched in cal high/low in here?
	BEAM_EXPANDER_MIDDLE=Y(zext(block(95)))
	BEAM_EXPANDER_TOP   =Y(zext(block(96)))
	RX_TUBE_TOP         =Y(zext(block(97)))
	BEAM_EXPANDER_BOTTOM=Y(zext(block(98)))
	RX_TUBE_BOTTOM      =Y(zext(block(99)))
	RX_TUBE_MIDDLE      =Y(zext(block(100)))
	write(*,'(6(f7.2,","),$)')
     & BEAM_EXPANDER_BOTTOM,BEAM_EXPANDER_MIDDLE,BEAM_EXPANDER_TOP,
     & RX_TUBE_BOTTOM,RX_TUBE_MIDDLE,RX_TUBE_TOP

	CALIBRATION_HIGH_TEMP=Y(zext(block(101)))! apparently these are the lowest values!
	HOUSING_TEMP      =Y(zext(block(102)))
	DUA_TEMP          =Y(zext(block(103)))
	CALIBRATION_LOW_TEMP =Y(zext(block(104)))! apparently these are the highest values!
	DUA_HOT1_TEMP     =Y(zext(block(105)))
	DUA_FPGA_TEMP     =Y(zext(block(106)))
	DUA_HOT2_TEMP     =Y(zext(block(108)))
	write(*,'((f7.2,","),$)')
     & HOUSING_TEMP

c	write(*,'(" ''",B6.6,"'',",$)') block(107) !RX channel enable readback
c analog board error flags
c	write(*,'(" ''",B8.8,"'',",$)')block(110)
c Vertical bitwise parity of analog data
c	write(*,'(" ''",B8.8,"'',",$)')block(111)
c cmd "C" counter
c	write(*,'(i4,",",$)') zext(block(112))
c FSW counter
c	write(*,'(i5,",",$)') jzext(word(57))
c ROM CRC
c	write(*,'(i5,",",$)') jzext(word(58))
c Override Status
c	write(*,'(" ''",B12.12,"'',",$)')word(59)
c Software detector disables
c	write(*,'(" ''",B5.5,"'',",$)')block(119)

c Signal processing
	alg_mode=zext(block(120))
	if(alg_mode.eq.2) then
	 algmod='TRK'
	elseif (alg_mode.eq.1) then
	 algmod='ACQ'
	elseif (alg_mode.eq.0) then
	 algmod='STA'
	else
	 algmod='UNK'
	endif
c	write(*,'(1x,a3,",",$)')algmod
cAVERAGE_TRANSMIT_TIME
	write(*,'(1x,i5,",",$)')jzext(word(61))
	lun_sig=zext(block(123))
	if(lun_sig.eq.1) then
	 lunsig='SIGNAL'
	elseif (lun_sig.eq.0) then
	 lunsig='NO_SIG'
	else
	 lunsig='UNK'
	endif
	write(*,'(1x,a6,",",$)')lunsig
c calculated or estimated range (200 ns counts) not word aligned
	write(*,'(1x,i5,",",$)') 256*zext(block(124))+zext(block(125))
c lunar return count
	write(*,'(i4,",",$)') zext(block(126))
c LUNARSUBWINDOW_BIN
c	write(*,'(1x,i5,",",$)')jzext(word(64))
c LUNAR_SUBWINDOW_COUNT
c	write(*,'(i4,",",$)') zext(block(129))
c LUNAR_SUBWINDOW_MAX_BIN
c	write(*,'(1x,i5,",",$)') 256*zext(block(130))+zext(block(131))
c LUNAR_SUBWINDOW_MAX_COUNT
c	write(*,'(i4,",",$)') zext(block(132))
c LUNAR_OUTSIDE_MAX_BIN
c	write(*,'(1x,i5,",",$)')jzext(word(67))
c LUNAR_OUTSIDE_MAX_COUNT
c	write(*,'(i4,",",$)') zext(block(135))
c Earth signal processing for laser ranging
c EARTH_SIGNAL_AQUIRED
	ert_sig = zext(block(136))
	if(ert_sig.eq.1) then
	 ertsig='LR_SIG'
	elseif (ert_sig.eq.0) then
	 ertsig='NO_LRS'
	else
	 ertsig='UNK'
	endif
c	write(*,'(1x,a6,",",$)')ertsig
c EARTH_ESTIMATED_RANGE
c	write(*,'(1x,i5,",",$)')jzext(word(69))
c EARTH_RETURN_COUNT
c	write(*,'(i4,",",$)') zext(block(135))
c EARTH_SUBWINDOW_BIN
c	write(*,'(1x,i5,",",$)') 256*zext(block(140))+zext(block(141))
c EARTH_SUBWINDOW_COUNT 
c	write(*,'(i4,",",$)') zext(block(142))
c EARTH_SUBWINDOW_MAX_BIN
c	write(*,'(1x,i5,",",$)')jzext(word(72))
c EARTH_SUBWINDOW_MAX_COUNT
c	write(*,'(i4,",",$)') zext(block(145))
c EARTH_OUTSIDE_MAX_BIN
c	write(*,'(1x,i5,",",$)') 256*zext(block(146))+zext(block(147))
c EARTH_OUTSIDE_MAX_COUNT
c	write(*,'(i4,",",$)') zext(block(148))
c TX_SHOT_0_DUP
	write(*,'(1x,i5,",",$)')jzext(word(75))
c TX_SHOT_14_DUP
	write(*,'(1x,i5,",",$)')jzext(word(76))
c LUNAR_RX_DET_0_SHOT_0_DUP
c	write(*,'(1x,i5,",",$)')jzext(word(77))
c LUNAR_RX_DET_0_SHOT_14_DUP
c	write(*,'(1x,i5,",",$)')jzext(word(78))
c EARTH_RX_SHOT_0_DUP
c	write(*,'(1x,i5,",",$)')jzext(word(79))
c EARTH_RX_SHOT_14_DUP  
c	write(*,'(1x,i5,",",$)')jzext(word(80))
c LASER_DRIVE_PULSE_MIN, MAX, AVG
	write(*,'(i4,",",$)') zext(block(161))
	write(*,'(i4,",",$)') zext(block(162))
	write(*,'(1x,i4)')zext(block(163))

c COMMANDED_THRESHOLDS_MIDFRAME
c	write(*,'(5(i4,","),$)')
c     & zext(block(164)),zext(block(165)),zext(block(166)),zext(block(167)),zext(block(168))
c GLITCH_STATUS
c	write(*,'(" ''",B5.5,"'',",$)')block(175)

c LOLA HEALTH_AND_SAFETY_FLAGS - LOHSFLAGS
c	write(*,'(1x,i3)')zext(block(176))
c Some tx info is repeated 28 times along with range data. Laser pulse energy is calibrated and reported in the RDR.
cxxxx	do i=0, 27
c LOTXPLSENRGYSH 177
c LOLASERDIODISH 178
c	 tx_energy=E(zext(block(177+20*i)))
c	 di_curr  =C(zext(block(178+20*i)))
c	 write(*,'(2(f12.3,","))') tx_energy,di_curr
cxxxx	enddo
	if(irec.ge.mypos/3424) goto 999 !normal exit
        goto 10
9999	continue !error
	write(0,'(a,a,a)')'file ',filename,' not opened.'
999     continue
        close(1) ! is always good even if not open
        end
