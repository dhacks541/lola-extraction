function [Emet, dbt1] = readEDRfunc(EDRin)
        full_sz=0;
        fstat=0;
        pkt_sz=3424;
        tempEmet=0;

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
        tempEmet(1:m) = 0; 
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
           plot(Emet);
    end