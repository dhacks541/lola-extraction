LOLA Extraction & Analysis Tools -- README

Two components to extraction:
	- Fortran parsing scripts
	- Matlab extraction and analysis tools

Instructions can be used for handling both "Engineering" and "Laser" fields (view sample data files)
	- Eng files contain ~100 fields, including spacecraft data
	- Lsr files contain ~60 fields, mostly laser-specific data

All programs and scripts have been tested to work on ADAPT's Linux VMs (as of July 2021)

Data flow:

LOLA Data Products  mergeEng or mergeLsr   Mission Phase Files 	 LOLAExtract.m	 graphical products,
		      ---------------> 				-------------->  long-term analysis,
 (EDRs and RDRs)	  Fortran	      (EDRs + RDRs)	   Matlab	        etc.


EDRs and RDRs are hosted on ADAPT @ /att/gpfsfs/briskfs01/project/PGDA/LOLA

Current Mission Phase Files, NO01-ES101, hosted on ADAPT @ /att/nobackup/dhackett

Required scripts/programs:

-mergeEng/mergeLsr: bash script, calls fortran csv extraction scripts and merges orbits (vertically) and EDRs/RDRs (horizontally) into large csv files (~1-3 GB), one per mission phase (~1 month)
	- usage: type "mergeEng" or "mergeLsr", source files declared in line 5 
	- dependencies:
		-rdr2utcEng/rdr2utcLsr: constructs single orbit csv's from RDRs
			- usage (to run on only one orbit, one data type): rdr2utc[Eng/Lsr] RDRNAME.DAT h > /path/to/destination/RDRNAME.csv
			- sub-dependencies: unsigned.o kzext2.o spicelib.a
			- to compile: gfortran -ffixed-line-length-none -O rdr2utc[Eng/Lsr].f unsigned.o kzext2.o spicelib.a -static-libgfortran -o rdr2utc[Eng/Lsr]
		-edr2hkEng/edr2hkLsr: constructs single orbit csv's from EDRs
			- usage (to run on only one orbit, one data type): edr2hk[Eng/Lsr] EDRNAME.DAT h > /path/to/destination/EDRNAME.csv
			- sub-dependencies: zext.o kzext2.o kzext3.o ksext3.o jzext.o
			- to compile: gfortran -ffixed-line-length-none -O edr2hk[Eng/Lsr].f zext.o kzext2.o kzext3.o ksext3.o jzext.o -o edr2hk[Eng/Lsr]
-LOLAExtract.m: written for trending and temperature-correcting of detector dark noise counts, however can be modified or discarded as needed. At present primary functions are to 1) cycle through csv's for given range of mission phases, 2) filter data against certain parameters (namely solar inclination), 3) create collated matrices for desired data fields, 4) perform segmented, emperical temperature correction transformation, 5) apply various smoothing, filtering, and graphing effects
	- usage: runs as Matlab script
	- dependencies: 
		- TempNoiseExtraction.m: accepts dark noise and hybrid detector temperature, returns "equivalent," temperature-corrected noise counts


Bugs:
-J2000 field (column 2) incorrect, disregard & suse TDT seconds for J2000
-Matlab reads only numerical fields, modification needed to capture string fields in csv's

Other notes:
-mergeEng/mergeLsr typically take about 20 minutes to create csv for one mission phase