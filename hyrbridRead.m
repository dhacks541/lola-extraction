function [] = hyrbridRead()

EDRin='LOLAEDR100461925.DAT';
RDRin='LOLARDR_100461925.DAT';
ispots = [1]; %ispots = [1:5] for all channels

    %% EDR extraction
    function [Emet, dbt1] = readEDR(EDRin)
        full_sz=0;
        fstat=0;
        pkt_sz=3424;

        fid=fopen(EDRin, 'rb');
        fseek(fid,0,'eof');
        filesize = ftell(fid);
        fsz = size(EDRin, 2);

        fseek(fid,0,'bof');
        pkts2rd=(filesize-64)/(pkt_sz);
        full_sz=full_sz+pkts2rd;
        m=int32(full_sz);
        Emet=[];  %MET from EDR
        dbt1=[];   %detector board 1 temp
        Ethr=[];  %detector 1 commanded threshold 
        tempEmet(1:m) = 0; %kludge-should not require temp variables
        tempdbt1(1:m) = 0;
        tempEthr(1:m)=0;

            for x=1:fsz
                count=pkt_sz;
                while((fstat == 0) && (count == pkt_sz))
                    [pkt, count] = fread(fid,pkt_sz); 
                        if (count == pkt_sz)
                        tempEmet(x) = pkt(2)+2^8*pkt(1)+2^16*pkt(4)+2^24*pkt(3);
                        tempdbt1(x)=-1.030e-5*(pkt(75)^3)+4.011e-3*(pkt(75)^2)-0.8309*pkt(75)+80.34; 
                        tempEthr(x)=pkt(23);
                        end
                        
                        if (size(Emet)<m)   
                        Emet=[Emet;tempEmet(x)];    
                        dbt1=[dbt1;tempdbt1(x)];
                        Ethr=[Ethr;tempEthr(x)];
                        end
                        fstat=feof(fid);
                    end
                end
           fclose(fid);
    end

    %% RDR extraction
    function [lon,lat,rad,et,met,flg,scrad,time,offndr,pw,en,rnge,rflct,...
            xenrg,sclon,sclat,noi,thr,gn,emi,inc,phs,...
            earth_range,earth_pulse,local_soltim] = readRDR(RDRin,ispots,skip)
        if(~exist('skip','var')),skip=1;end

        dark=[0.135d0,0.1159d0,0.1303d0,0.1775d0,0.2164d0];
        % fiber, and aft optics efficiency in percent
        fiber=[84.045654,87.437592,85.325625,90.273744,80.809245];
        % diffractive optics splits energy into 5 spots, more in the center
        % these should be symmetrical but owing to measurement imprecision,
        % separate values are given in the cal rpt. This really needs to be flatfielded!
        trans=[0.22d0,0.14d0,0.14d0,0.14d0,0.14d0]; %nominal
        % initialize optical link, transmission times rx telescope radius 0.07 squared
        trans=trans.*0.01.*fiber.*0.0049;


        % parameters for RDR conversion
        c32=4294967296.0e0;
        dscale=1e-7;
        rscale=1e-6;
        escale=1e-6;
        tscale=1e-6;
        gscale=1e-6;
        pscale=1e-3;

         %rdr_int32 = ones(64,200000);
         %rdr_uint32 = ones(64,200000);
        % lon, lat, radius, range, pw, energy

        fid=fopen(RDRin, 'rb');
        rdr_int32=fread(fid,[64 2e6],'int32', 'ieee-le');
        frewind(fid);
        rdr_uint32=fread(fid,[64 2e6],'uint32', 'ieee-le');
        fclose(fid);

        met=double(rdr_int32(1,:));  
        subseconds=double(rdr_uint32(2,:)/c32);
        et=double(rdr_int32(3,:))+double(rdr_uint32(4,:)/c32);
        sclon=double(rdr_int32(7,:))*dscale;
        sclat=double(rdr_int32(8,:))*dscale;
        scrad=double(rdr_uint32(9,:))*rscale;

        xenrg=escale*double(rdr_int32(5,:))';

        lon=ones(size(rdr_int32,2),length(ispots));
        lat=ones(size(rdr_int32,2),length(ispots));
        rad=ones(size(rdr_int32,2),length(ispots));
        rnge=ones(size(rdr_int32,2),length(ispots));
        pw=ones(size(rdr_int32,2),length(ispots));
        en=ones(size(rdr_int32,2),length(ispots));
        noi=ones(size(rdr_int32,2),length(ispots));
        thr=ones(size(rdr_int32,2),length(ispots));
        gn=ones(size(rdr_int32,2),length(ispots));
        flg=ones(size(rdr_int32,2),length(ispots));
        rflct=ones(size(rdr_int32,2),length(ispots));

        for j=1:skip:length(ispots)
            i=ispots(j);
            lon(:,j)=mod(dscale*double(rdr_int32(i*10+1,:)),360);
            lat(:,j)=dscale*double(rdr_int32(i*10+2,:));
            rad(:,j)=rscale*double(rdr_uint32(10*i+3,:))-1737.4;
            rnge(:,j)=rscale*double(rdr_uint32(10*i+4,:));
            pw(:,j)=pscale*double(rdr_uint32(10*i+5,:));
            en(:,j)=rscale*double(rdr_uint32(10*i+6,:));
            noi(:,j)=double(rdr_uint32(10*i+7,:));
            thr(:,j)=tscale*double(rdr_uint32(10*i+8,:));
            gn(:,j)=gscale*double(rdr_uint32(10*i+9,:));
        %    flg(:,j)=(double(rdr_int32(i*10+10,:)));
            flg(:,j)=mod(double(rdr_int32(i*10+10,:))+0.0,64);
            flg(mod(double(rdr_int32(i*10+10,:)),65536)>=32768,i)=15;

        % reflectivity
        % trans(i) contains aperture, transmission efficiency, etc
        % scale factor accounts for km^2, mJ to fJ
             rflct(:,j)=rnge(:,j).^2.*escale.*(en(:,j)-dark(j))./(xenrg.*trans(j));


        end
        offndr=(rdr_uint32(5*10+10+1,:)/65536 )/2e4*180/pi;
        emi=(mod(rdr_uint32(5*10+10+1,:),65536)/2e4*180/pi);
        inc=(rdr_uint32(5*10+10+2,:)/65536 )/2e4*180/pi;
        phs=(mod(rdr_uint32(5*10+10+2,:),65536)/2e4*180/pi);

        earth_range=double(rdr_uint32(63,:)) ./ 2^32 ; % seconds from T0
        earth_pulse=( rdr_uint32(64,:)/65536 ) ./ 1e3 ; % pulsewidth in ns
        local_soltim=( mod(rdr_uint32(64,:),65536) ) ./ 1e3 ; % local solar time

        time=met+subseconds;

        clear rdr_int32 rdr_uint32

    end

        [e_met, dbt1] = readEDR(EDRin);
        [lon,lat,rad,et,met,flg,scrad,time,offndr,pw,en,rnge,rflct,...
          xenrg,sclon,sclat,noi,thr,gn,emi,inc,phs,...
          earth_range,earth_pulse,local_soltim] = readRDR(RDRin, ispots);

       
        %%
        
        plot(e_met, dbt1) 
end