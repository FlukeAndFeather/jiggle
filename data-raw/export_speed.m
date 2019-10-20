% Load PRH first
% Off by an hour due to DST
aoi = find(DN >= datenum('27-July-2016 13:21') & DN < datenum('27-July-2016 13:37'));
aoi_speed = speed.JJ(aoi);
% Location of jiggle/data-raw
datarawloc = uigetdir();
fileID = fopen([datarawloc '/speeds.txt'],'w');
fprintf(fileID,'%f\n', aoi_speed);
fclose(fileID);
