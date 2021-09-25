
% % 1	MET_TIME
% % 2	J2000
% % 3	 Lsr
% % 4	 Enab
% % 5	 Fire
% % 6	 Rgstrt
% % 7	 Rgstop
% % 8	 Clk
% % 9	 RX1
% % 10	 RX2
% % 11	 RX3
% % 12	 RX4
% % 13	 RX5
% % 14	 V550
% % 15	 V5
% % 16	 V12
% % 17	 V3.3D
% % 18	 V3.3A
% % 19	 V5NEG
% % 20	 DIODE1T
% % 21	 DIODE2T
% % 22	 DU3D3IMONA
% % 23	 DU3D3IMOND
% % 24	 DU1D5IMONA
% % 25	 DU12IMON
% % 26	 DU1D5VMONA
% % 27	 DU1D5IMOND
% % 28	 HY_T1
% % 29	 HY_T2
% % 30	 HY_T3 
% % 31	 HY_T4
% % 32	 HY_T5
% % 33	 DB_T1
% % 34	 DB_T2
% % 35	 DB_T3
% % 36	 DB_T4
% % 37	 DB_T5
% % 38	 LEA_T
% % 39	 L1DI_T
% % 40	 L2DI_T
% % 41	 L1B_T
% % 42	 L2B_T
% % 43	 PCA_T
% % 44	 ANBD_T
% % 45	 OSC_T
% % 46	 DUBD_T
% % 47	 BX_BOT
% % 48	 BX_MID
% % 49	 BX_TOP
% % 50	 RX_BOT
% % 51	 RX_MID
% % 52	 RX_TOP
% % 53	 C_HI_T
% % 54	 HSNG_T
% % 55	 DUA_T
% % 56	 DUA_HOT1_T
% % 57	 DUA_HOT2_T
% % 58	 DUA_FPGA_T
% % 59	 OVRStatus
% % 60	 Alg_mode
% % 61	 Av_Transit
% % 62	 LSignal
% % 63	 LEst_Rng
% % 64	 LRet_Ct
% % 65	 TX_0Dup
% % 66	 TX_14Dup
% % 67	 Ldrv_MX
% % 68	 Ldrv_MN
% % 69	 Ldrv_AV
% % 70	 SCLK_LOLA
% % 71	 TDT_Seconds
% % 72	   Switchout_ns
% % 73	  N
% % 74	  Txenrg
% % 75	    TxPw
% % 76	   rngmx
% % 77	  solinc
% % 78	  nois1
% % 79	  nois2
% % 80	  nois3 
% % 81	  nois4
% % 82	  nois5
% % 83	 thrs1
% % 84	 thrs2
% % 85	 thrs3
% % 86	 thrs4 all + 1
% % 87	 thrs5
% % 88	 gain1
% % 89	 gain2
% % 90	 gain3
% % 91	 gain4
% % 92	 gain5
% % 93	    sclon
% % 94	    sclat
% % 95	 UTC
%  

LOLA_DATA=readmatrix("LRO_ES_02_122870032.csv");
met = (LOLA_DATA(:,1));
ht2 = (LOLA_DATA(:,30)); 
solinc = (LOLA_DATA(:,77));
nois2 = (LOLA_DATA(:,80)); 
thrs4 = (LOLA_DATA(:,87));
tdt = (LOLA_DATA(:,71));

gain4 = (LOLA_DATA(:,92));

hzdata=readmatrix("LRO_ES_02_122870032_28hz.csv");

met_=hzdata(:,1);
switchout=hzdata(:,2);
energy=hzdata(:,3);
pulsewidth=hzdata(:,4);


% yyaxis right;
% set(gca, 'yTick', [14:1:19, 80:5:120]);
% plot(met, ht2, met, solinc);

plot(met, solinc, met, ht2);
yyaxis right;
plot(met, nois2);