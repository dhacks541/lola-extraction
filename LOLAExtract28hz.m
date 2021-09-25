fileNum = '122870032';
missionPhase = 'LRO_ES_02';
pathToScript = fullfile(pwd, 'rdr28hz');
rdrfile = strcat('/att/gpfsfs/briskfs01/project/PGDA/LOLA/data/LOLA_RDR/', missionPhase, '/LOLARDR_', fileNum, '.DAT');
destFile = strcat(missionPhase, '_', fileNum, '_28hz.csv');
cmdStr = [pathToScript ' ' rdrfile ' > ' destFile];
system(cmdStr);
cmdStr = ['mv ' destFile  ' /att/nobackup/dhackett'];
system(cmdStr);

pathToScript = fullfile(pwd, 'mergeSingle');
edrfile = strcat('/att/gpfsfs/briskfs01/project/PGDA/LOLA/data/LOLA_EDR/', missionPhase, '/LOLAEDR', fileNum, '.DAT');
cmdStr = [pathToScript ' ' edrfile ' ' rdrfile ' ' fileNum ' ' missionPhase];
system(cmdStr);