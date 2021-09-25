function [tempNoiseArr] = TempNoiseExtraction(tempArr, noiseArr)

%% initializations
nois2 = noiseArr{1};
nois3 = noiseArr{2};
nois4 = noiseArr{3};
nois5 = noiseArr{4};

ht2 = tempArr{1};
ht3 = tempArr{2};
ht4 = tempArr{3};
ht5 = tempArr{4};

tempnoise2=nois2; tempnoise3=nois3;
tempnoise4=nois4; tempnoise5=nois5;

at2 = 19;
at3 = 16;
at4 = 17;
at5 = 19;

artifact0=0;
before_art=0;
artifactend=0;

%% segmentation & temperature correction
blocksz = 300000;   % sets size (seconds) of temperature correction segments
segmentCount = fix(length(nois2)/blocksz);

for i=1:segmentCount
    strcat(num2str(i), " out of " , num2str(segmentCount), " temperature correction segments")
    
    % storing previous polynomial co-effs, in case current ones are bad
    if i>1
        prev2=p2;
        prev5=p5;
    end
    
    % generating cubic fit
    p2 = polyfit(ht2((i*blocksz)-(blocksz-1):i*blocksz), nois2((i*blocksz)-(blocksz-1):i*blocksz), 3);
    p3 = polyfit(ht3((i*blocksz)-(blocksz-1):i*blocksz), nois3((i*blocksz)-(blocksz-1):i*blocksz), 3);
    p4 = polyfit(ht4((i*blocksz)-(blocksz-1):i*blocksz), nois4((i*blocksz)-(blocksz-1):i*blocksz), 3);
    p5 = polyfit(ht5((i*blocksz)-(blocksz-1):i*blocksz), nois5((i*blocksz)-(blocksz-1):i*blocksz), 3);

    for j = (i*blocksz)-(blocksz-1):i*blocksz
        
        if (j==(i*blocksz-(blocksz-1))) && i~=1
            % temperature corrects first noise value of new segment, and
            % checks if there is a large jump from the previous one (>1000
            % counts). If so, disregards current segment co-effs and uses
            % previous one for channels 2 & 5 (bug only occurred for 2&5)
            tempnoise2(j) = (nois2(j) * ((p2(1)*(at2^3) + p2(2)*(at2^2) + p2(3)*at2 + p2(4)) /( p2(1)*(ht2(j)^3) + p2(2)*(ht2(j)^2) + p2(3)*ht2(j) + p2(4))));
            tempnoise5(j) = (nois5(j) * ((p5(1)*(at5^3) + p5(2)*(at5^2) + p5(3)*at5 + p5(4)) /( p5(1)*(ht5(j)^3) + p5(2)*(ht5(j)^2) + p5(3)*ht5(j) + p5(4))));
            if abs(tempnoise2(j)-tempnoise2(j-1)) > 1000 || abs(tempnoise5(j)-tempnoise5(j-1)) > 1000
                p2=prev2;
                p5=prev5;
            end
        end
        % normal temperature correction
        tempnoise2(j) = (nois2(j) * ((p2(1)*(at2^3) + p2(2)*(at2^2) + p2(3)*at2 + p2(4)) /( p2(1)*(ht2(j)^3) + p2(2)*(ht2(j)^2) + p2(3)*ht2(j) + p2(4))));  
        tempnoise3(j) = (nois3(j) * ((p3(1)*(at3^3) + p3(2)*(at3^2) + p3(3)*at3 + p3(4)) /( p3(1)*(ht3(j)^3) + p3(2)*(ht3(j)^2) + p3(3)*ht3(j) + p3(4))));
        tempnoise4(j) = (nois4(j) * ((p4(1)*(at4^3) + p4(2)*(at4^2) + p4(3)*at4 + p4(4)) /( p4(1)*(ht4(j)^3) + p4(2)*(ht4(j)^2) + p4(3)*ht4(j) + p4(4))));
        tempnoise5(j) = (nois5(j) * ((p5(1)*(at5^3) + p5(2)*(at5^2) + p5(3)*at5 + p5(4)) /( p5(1)*(ht5(j)^3) + p5(2)*(ht5(j)^2) + p5(3)*ht5(j) + p5(4))));
        
    end    
end

% trimming non-corrected remainder at end of data record
tempnoise2(segmentCount*blocksz+1:length(nois2))=NaN;   
tempnoise3(segmentCount*blocksz+1:length(nois3))=NaN;
tempnoise4(segmentCount*blocksz+1:length(nois4))=NaN;
tempnoise5(segmentCount*blocksz+1:length(nois5))=NaN;

% return corrected noise counts
tempNoiseArr={tempnoise2, tempnoise3, tempnoise4, tempnoise5};

end