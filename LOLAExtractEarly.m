combimet=[];
combinoise2=[];
combinoise3=[];
combinoise4=[];
combinoise5=[];

combithrs2=[];
combithrs3=[];
combithrs4=[];
combithrs5=[];

combit2=[];
combit3=[];
combit4=[];
combit5=[];

combigain2=[];
combigain3=[];
combigain4=[];
combigain5=[];

combitempnoise2=[];
combitempnoise3=[];
combitempnoise4=[];
combitempnoise5=[];

combinoisesm2=[];
combinoisesm3=[];
combinoisesm4=[];
combinoisesm5=[];

combitempnoisesm2=[];
combitempnoisesm3=[];
combitempnoisesm4=[];
combitempnoisesm5=[];

metReset = datetime(2019,3, 26, 20, 07, 37)-seconds(287928554);
ref=81;
% startR = 106;
% endR = 113;
% fileIndex = [startR:endR];
% prefix = "LRO_NO_";
% suffix = ".csv";
% fileList=strings(size(fileIndex));
% for record = 1:length(fileIndex)
%     fileList(record) = append(prefix, num2str(fileIndex(record)), suffix);
% end

avggain2 = 49.06;
avggain3 = 49.97;
avggain4 = 50.92;
avggain5 = 50.71;

avgthrs2 = 13;
avgthrs3 = 13;
avgthrs4 = 12;
avgthrs5 = 14;

fileList=["LRO_NO_06.csv" "LRO_NO_07.csv" "LRO_NO_08.csv" "LRO_NO_09.csv" "LRO_NO_10.csv" "LRO_NO_11.csv" "LRO_NO_12.csv" "LRO_NO_13.csv" "LRO_SM_01.csv" "LRO_SM_02.csv" "LRO_SM_03.csv" "LRO_SM_04.csv" "LRO_SM_05.csv" "LRO_SM_06.csv" "LRO_SM_07.csv" "LRO_SM_08.csv" "LRO_SM_09.csv" "LRO_SM_10.csv"...
        "LRO_SM_11.csv" "LRO_SM_12.csv" "LRO_SM_13.csv" "LRO_SM_14.csv" "LRO_SM_15.csv" "LRO_SM_16.csv" "LRO_SM_17.csv" "LRO_SM_18.csv" "LRO_SM_19.csv" "LRO_SM_20.csv"...
        "LRO_SM_21.csv" "LRO_SM_22.csv" "LRO_SM_23.csv" "LRO_SM_24.csv" "LRO_SM_25.csv" "LRO_SM_26.csv" "LRO_ES_01.csv" "LRO_ES_02.csv" "LRO_ES_03.csv" "LRO_ES_04.csv" "LRO_ES_05.csv" "LRO_ES_06.csv"...
        "LRO_ES_07.csv" "LRO_ES_08.csv" "LRO_ES_09.csv" "LRO_ES_10.csv" "LRO_ES_11.csv" "LRO_ES_12.csv" "LRO_ES_13.csv"];
for i=1:length(fileList)
    
file=fileList(i)
SOURCE_DATA = readmatrix(file);
recordSize=size(SOURCE_DATA);
LOLA_DATA=zeros(recordSize(1), recordSize(2));
z=1;

for i=1:length(SOURCE_DATA)                                 %filter for laser off(73)/dark side(77)/baseline gain&thr data only
%    if SOURCE_DATA(i,77)>93 && SOURCE_DATA(i, ref) >80000 
   if SOURCE_DATA(i,77)>93
           LOLA_DATA(z, :) = SOURCE_DATA(i, :);
           z=z+1;
       
   end
end
    
LOLA_DATA(z:length(LOLA_DATA), :) = [];
    

%% 

met = (LOLA_DATA(:,1));
solinc = (LOLA_DATA(:,77));
nois2 = (LOLA_DATA(:,ref-1));
nois3 = (LOLA_DATA(:,ref)); %81 after ES43
nois4 = (LOLA_DATA(:,ref+1));
nois5 = (LOLA_DATA(:,ref+2));
tempnoi2 = zeros(size(nois2));
tempnoi3 = zeros(size(nois3));
tempnoi4 = zeros(size(nois4));
tempnoi5 = zeros(size(nois5));
ht2 = (LOLA_DATA(:,29)); 
ht3 = (LOLA_DATA(:,30));
ht4 = (LOLA_DATA(:,31)); 
ht5 = (LOLA_DATA(:,32));
thrs2 = (LOLA_DATA(:,ref+4));
thrs3 = (LOLA_DATA(:,ref+5));
thrs4 = (LOLA_DATA(:,ref+6));
thrs5 = (LOLA_DATA(:,ref+7));
gain2 = (LOLA_DATA(:,ref+9));
gain3 = (LOLA_DATA(:,ref+10));
gain4 = (LOLA_DATA(:,ref+11));
gain5 = (LOLA_DATA(:,ref+12));


avgt2 = 19;
avgt3 = 16;
avgt4 = 17;
avgt5 = 19;



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

thrsArr = {combithrs2, combithrs3, combithrs4, combithrs5};
tempArr = {combit2, combit3, combit4, combit5};
noiseArr = {combinoise2, combinoise3, combinoise4, combinoise5};

[tempNoiseArr] = TempNoiseExtraction(tempArr, noiseArr);
[thrsNoiseArr] = ThrNoiseExtraction(thrsArr, tempNoiseArr);


combitempnoise2 = thrsNoiseArr{1};
combitempnoise3 = thrsNoiseArr{2};
combitempnoise4 = thrsNoiseArr{3};
combitempnoise5 = thrsNoiseArr{4};

sz=(length(combimet));
block=50000;
modulus = mod(sz, block);

smet = zeros((sz-modulus)/block, 1);
sn2 = smet; sn3 = smet; sn4 = smet; sn5 = smet;
stn2 = smet; stn3 = smet; stn4 = smet; stn5 = smet;
st2 = smet; st3 = smet; st4 = smet; st5 = smet;

sthrs2 = zeros((sz-modulus)/block, 1);
sthrs3 = zeros((sz-modulus)/block, 1);
sthrs4 = zeros((sz-modulus)/block, 1);
sthrs5 = zeros((sz-modulus)/block, 1);

sgain2 = zeros((sz-modulus)/block, 1);
sgain3 = zeros((sz-modulus)/block, 1);
sgain4 = zeros((sz-modulus)/block, 1);
sgain5 = zeros((sz-modulus)/block, 1);


for x=1:length(smet)
    smet(x) = mean(combimet((block*x)-(block-1):(block*x)));
    
    sn2(x) = mean(combinoise2((block*x)-(block-1):(block*x)));
    sn3(x) = mean(combinoise3((block*x)-(block-1):(block*x)));
    sn4(x) = mean(combinoise4((block*x)-(block-1):(block*x)));
    sn5(x) = mean(combinoise5((block*x)-(block-1):(block*x)));
    
    stn2(x) = mean(combitempnoise2((block*x)-(block-1):(block*x)));
    stn3(x) = mean(combitempnoise3((block*x)-(block-1):(block*x)));
    stn4(x) = mean(combitempnoise4((block*x)-(block-1):(block*x)));
    stn5(x) = mean(combitempnoise5((block*x)-(block-1):(block*x)));
    
    st2(x) = mean(combit2((block*x)-(block-1):(block*x)));
    st3(x) = mean(combit3((block*x)-(block-1):(block*x)));
    st4(x) = mean(combit4((block*x)-(block-1):(block*x)));
    st5(x) = mean(combit5((block*x)-(block-1):(block*x)));
    
    sthrs2(x) = mean(combithrs2((block*x)-(block-1):(block*x)));
    sthrs3(x) = mean(combithrs3((block*x)-(block-1):(block*x)));
    sthrs4(x) = mean(combithrs4((block*x)-(block-1):(block*x)));
    sthrs5(x) = mean(combithrs5((block*x)-(block-1):(block*x)));

    sgain2(x) = mean(combigain2((block*x)-(block-1):(block*x)));
    sgain3(x) = mean(combigain3((block*x)-(block-1):(block*x)));
    sgain4(x) = mean(combigain4((block*x)-(block-1):(block*x)));
    sgain5(x) = mean(combigain5((block*x)-(block-1):(block*x)));
end



utctime=metReset+seconds(smet(1:length(smet)));




