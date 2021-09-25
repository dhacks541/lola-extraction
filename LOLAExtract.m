%% LOLAExtract - long term trending of LOLA  data
%% Last modified: Aug 2 2021 by Damian Hackett
%% see fieldIdent.txt for variable index numbers
%% see LOLA_EAT_readme.txt for further instructions

%% "combined" / concatenated fields for up to entire mission -- add as needed
combimet=[]; % combined MET

combinoise2=[]; % combined noise, detectors 2-5
combinoise3=[];
combinoise4=[];
combinoise5=[];

combithrs2=[];  % combined threshold
combithrs3=[];
combithrs4=[];
combithrs5=[];

combit2=[];    % combined hybrid detector temperature
combit3=[];
combit4=[];
combit5=[];

combigain2=[];  % combined gain
combigain3=[];
combigain4=[];
combigain5=[];

combitempnoise2=[]; % combined temperature corrected noise
combitempnoise3=[];
combitempnoise4=[];
combitempnoise5=[];


%% initializations
metReset = datetime(2019,3, 26, 20, 07, 37)-seconds(287928554); %magic numbers: calibrate MET with UTC
startR = 14;    % start record
endR = 103;     % end record
fileIndex = [startR:endR];
prefix = "LRO_ES_";
suffix = ".csv";
fileList=strings(size(fileIndex));
for record = 1:length(fileIndex)
    fileList(record) = append(prefix, num2str(fileIndex(record)), suffix);
end

% baseline gain settings
avggain2 = 49.06;
avggain3 = 49.97;
avggain4 = 50.92;
avggain5 = 50.71;

% baseline threshold settings
avgthrs2 = 12.92;
avgthrs3 = 13.34;
avgthrs4 = 11.79;
avgthrs5 = 14.21;

%% creating complete data records

for i=1:length(fileList)    % step through each mission phase file
    
file=fileList(i)
SOURCE_DATA = readmatrix(file);
recordSize=size(SOURCE_DATA);
LOLA_DATA=zeros(recordSize(1), recordSize(2));
z=1;

% filtering   
for j=2:length(SOURCE_DATA)-1                              
   if SOURCE_DATA(j,77)>93 && SOURCE_DATA(j,73)==0 && SOURCE_DATA(j,90)*100 == avggain2*100 && SOURCE_DATA(j,91)*100 == avggain3*100 && ...     % include only data  w/ solinc>93, 
      SOURCE_DATA(j, 92)*100 == avggain4*100 && SOURCE_DATA(j, 93)*100 == avggain5*100 && SOURCE_DATA(j, 85)*100 == avgthrs2*100 && ...         % laser off, and at baseline threshold and gain settings
      SOURCE_DATA(j, 86)*100 == avgthrs3*100 && SOURCE_DATA(j, 87)*100 == avgthrs4*100 && SOURCE_DATA(j, 88)*100 == avgthrs5*100 && ...
      ...
      SOURCE_DATA(j+1,90)*100 == avggain2*100 && SOURCE_DATA(j+1, 92)*100 == avggain4*100 && SOURCE_DATA(j+1, 93)*100 == avggain5*100 && ...    % expanding threshold/gain filtering window by one
      SOURCE_DATA(j+1, 85)*100 == avgthrs2*100 && SOURCE_DATA(j+1, 87)*100 == avgthrs4*100 && SOURCE_DATA(j+1, 88)*100 == avgthrs5*100 && ...   % second on either side of data record
      ...
      SOURCE_DATA(j-1,90)*100 == avggain2*100 && SOURCE_DATA(j-1, 92)*100 == avggain4*100 && SOURCE_DATA(j-1, 93)*100 == avggain5*100 && ...         
      SOURCE_DATA(j-1, 85)*100 == avgthrs2*100 && SOURCE_DATA(j-1, 87)*100 == avgthrs4*100 && SOURCE_DATA(j-1, 88)*100 == avgthrs5*100
        LOLA_DATA(z, :) = SOURCE_DATA(j, :);
        z=z+1;
   end
end
    
LOLA_DATA(z:length(LOLA_DATA), :) = []; % trim orginal sized array after filtering
    
met = (LOLA_DATA(:,1));
solinc = (LOLA_DATA(:,77));
nois2 = (LOLA_DATA(:,80));
nois3 = (LOLA_DATA(:,81));
nois4 = (LOLA_DATA(:,82));
nois5 = (LOLA_DATA(:,83));
tempnoi2 = zeros(size(nois2));
tempnoi3 = zeros(size(nois3));
tempnoi4 = zeros(size(nois4));
tempnoi5 = zeros(size(nois5));
ht2 = (LOLA_DATA(:,29)); 
ht3 = (LOLA_DATA(:,30));
ht4 = (LOLA_DATA(:,31)); 
ht5 = (LOLA_DATA(:,32));
thrs2 = (LOLA_DATA(:,85));
thrs3 = (LOLA_DATA(:,86));
thrs4 = (LOLA_DATA(:,87));
thrs5 = (LOLA_DATA(:,88));
gain2 = (LOLA_DATA(:,90));
gain3 = (LOLA_DATA(:,91));
gain4 = (LOLA_DATA(:,92));
gain5 = (LOLA_DATA(:,93));

avgt2 = 19;
avgt3 = 16;
avgt4 = 17;
avgt5 = 19;

% concatentation of records
combimet=vertcat(combimet, met);        
combinoise2=vertcat(combinoise2, nois2);
combinoise3=vertcat(combinoise3, nois3);
combinoise4=vertcat(combinoise4, nois4);
combinoise5=vertcat(combinoise5, nois5);
combithrs2=vertcat(combithrs2, thrs2);
combithrs3=vertcat(combithrs3, thrs3);
combithrs4=vertcat(combithrs4, thrs4);
combithrs5=vertcat(combithrs5, thrs5);
combit2=vertcat(combit2, ht2);
combit3=vertcat(combit3, ht3);
combit4=vertcat(combit4, ht4);
combit5=vertcat(combit5, ht5);
combigain2=vertcat(combigain2, gain2);
combigain3=vertcat(combigain3, gain3);
combigain4=vertcat(combigain4, gain4);
combigain5=vertcat(combigain5, gain5);
combitempnoise2=vertcat(combitempnoise2, tempnoi2);
combitempnoise3=vertcat(combitempnoise3, tempnoi3);
combitempnoise4=vertcat(combitempnoise4, tempnoi4);
combitempnoise5=vertcat(combitempnoise5, tempnoi5);
end

%% temperature correction

tempArr = {combit2, combit3, combit4, combit5};
noiseArr = {combinoise2, combinoise3, combinoise4, combinoise5};

[tempNoiseArr] = TempNoiseExtraction(tempArr, noiseArr); %returns temperature corrected noise

combitempnoise2 = tempNoiseArr{1};
combitempnoise3 = tempNoiseArr{2};
combitempnoise4 = tempNoiseArr{3};
combitempnoise5 = tempNoiseArr{4};

%% averaging/smoothing

sz=(length(combimet));
block=10000;   % size of averaging block in seconds
modulus = mod(sz, block);

smet = zeros((sz-modulus)/block, 1);
sn2 = smet; sn3 = smet; sn4 = smet; sn5 = smet;
stn2 = smet; stn3 = smet; stn4 = smet; stn5 = smet;
st2 = smet; st3 = smet; st4 = smet; st5 = smet;
sthrs2 = smet; sthrs3 = smet; sthrs4 = smet;
sthrs5 = smet; sgain2 = smet; sgain3 = smet; 
sgain4 = smet; sgain5 = smet; 


for x=1:length(smet)
    smet(x) = mean(combimet((block*x)-(block-1):(block*x))); % smoothed MET
    
    sn2(x) = mean(combinoise2((block*x)-(block-1):(block*x)));  % smoothed noise
    sn3(x) = mean(combinoise3((block*x)-(block-1):(block*x)));
    sn4(x) = mean(combinoise4((block*x)-(block-1):(block*x)));
    sn5(x) = mean(combinoise5((block*x)-(block-1):(block*x)));
    
    stn2(x) = mean(combitempnoise2((block*x)-(block-1):(block*x)),'omitnan'); % smoothed temp corrected noise
    stn3(x) = mean(combitempnoise3((block*x)-(block-1):(block*x)),'omitnan');
    stn4(x) = mean(combitempnoise4((block*x)-(block-1):(block*x)),'omitnan');
    stn5(x) = mean(combitempnoise5((block*x)-(block-1):(block*x)),'omitnan');
    
    st2(x) = mean(combit2((block*x)-(block-1):(block*x)));  % smoothed temp
    st3(x) = mean(combit3((block*x)-(block-1):(block*x)));
    st4(x) = mean(combit4((block*x)-(block-1):(block*x)));
    st5(x) = mean(combit5((block*x)-(block-1):(block*x)));
    
    sthrs2(x) = mean(combithrs2((block*x)-(block-1):(block*x)));    % smoothed threshold
    sthrs3(x) = mean(combithrs3((block*x)-(block-1):(block*x)));
    sthrs4(x) = mean(combithrs4((block*x)-(block-1):(block*x)));
    sthrs5(x) = mean(combithrs5((block*x)-(block-1):(block*x)));

    sgain2(x) = mean(combigain2((block*x)-(block-1):(block*x)));    % smoothed gain
    sgain3(x) = mean(combigain3((block*x)-(block-1):(block*x)));
    sgain4(x) = mean(combigain4((block*x)-(block-1):(block*x)));
    sgain5(x) = mean(combigain5((block*x)-(block-1):(block*x)));
end

utctime=metReset+seconds(smet(1:length(smet)));

%% plotting

plot(utctime, stn2, utctime, stn3, utctime, stn4, utctime, stn5);