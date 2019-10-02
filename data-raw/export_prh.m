%% Load .mat files
jiggle_dir = uigetdir('', 'Choose jiggle root folder');

% load prh
load([jiggle_dir '/data-raw/Example Data/mn160727-11 10Hzprh.mat'])
% load Adata
load([jiggle_dir '/data-raw/Example Data/mn160727-11Adata.mat'])

%% Subset
% Examine depth profile
figure
h = plot(-p);
% Brush subsection for export
while true
  w = waitforbuttonpress; 
  if w == 1 % keyboard press
    key = get(gcf,'currentcharacter'); 
    if key == 13 % return key
      break
    end
  end
end
brushed = h.BrushData;
aoi = find(brushed);
aoi_A = aoi * Afs / fs;
% verify area of interest is accurate
figure
plot(-p(aoi));

%% Export NetCDF
% Create .nc file
ncfile = [jiggle_dir '/data-raw/mn160727-11 10Hzprh.nc'];
ncid = netcdf.create(ncfile, 'CLOBBER');

% Define dimensions (time, timeA, axis)
timeid = netcdf.defDim(ncid, 'time', length(aoi));
timeAid = netcdf.defDim(ncid, 'timeA', length(aoi) * Afs / fs);
axisid = netcdf.defDim(ncid, 'axis', 3);

% Define variables (time, depth, pitch, roll, head, A, fs, Afs)
timeid = netcdf.defVar(ncid, 'time', 'NC_DOUBLE', timeid);
depthid = netcdf.defVar(ncid, 'depth', 'NC_FLOAT', timeid);
pitchid = netcdf.defVar(ncid, 'pitch', 'NC_FLOAT', timeid);
rollid = netcdf.defVar(ncid, 'roll', 'NC_FLOAT', timeid);
headid = netcdf.defVar(ncid, 'head', 'NC_FLOAT', timeid);
timeAid = netcdf.defVar(ncid, 'timeA', 'NC_DOUBLE', timeAid);
Aid = netcdf.defVar(ncid, 'A', 'NC_FLOAT', [timeAid axisid]);
fsid = netcdf.defVar(ncid, 'fs', 'NC_INT', []);
Afsid = netcdf.defVar(ncid, 'Afs', 'NC_INT', []);
netcdf.endDef(ncid)

% Put variables
netcdf.putVar(ncid, timeid, DN(aoi));
netcdf.putVar(ncid, depthid, p(aoi));
netcdf.putVar(ncid, pitchid, pitch(aoi));
netcdf.putVar(ncid, rollid, roll(aoi));
netcdf.putVar(ncid, headid, head(aoi));
timeA = linspace(DN(aoi(1)), DN(aoi(end)), length(aoi) * Afs / fs);
netcdf.putVar(ncid, timeAid, timeA);
netcdf.putVar(ncid, Aid, A(aoi_A, :));
netcdf.putVar(ncid, fsid, fs);
netcdf.putVar(ncid, Afsid, Afs);

% Close file
netcdf.close(ncid)

% Sanity check file by checking fs written correctly
ncfs = ncread(ncfile, 'fs');
if ncfs == fs
  disp(['NetCDF file exported to ' ncfile])
else
  disp 'Error exporting NetCDF file'N
end
