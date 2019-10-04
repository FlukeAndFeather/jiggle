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
change_fs = @(x, fs1, fs2) (x - 1) * fs2 / fs1 + 1;
Aaoi = change_fs(aoi(1), fs, Afs):change_fs(aoi(end), fs, Afs);
% verify area of interest is accurate
figure
plot(-p(aoi));

%% Export NetCDF
% Create .nc file
ncfile = [jiggle_dir '/data-raw/mn160727-11 10Hzprh.nc'];
ncid = netcdf.create(ncfile, 'CLOBBER');

% Define dimensions (time, timeA, axis)
timedim = netcdf.defDim(ncid, 'time', length(aoi));
Atimedim = netcdf.defDim(ncid, 'timeA', length(Aaoi));
axisdim = netcdf.defDim(ncid, 'axis', 3);

% Define variables (time, depth, pitch, roll, head, A, fs, Afs)
timevar = netcdf.defVar(ncid, 'time', 'NC_DOUBLE', timedim);
depthvar = netcdf.defVar(ncid, 'depth', 'NC_FLOAT', timedim);
pitchvar = netcdf.defVar(ncid, 'pitch', 'NC_FLOAT', timedim);
rollvar = netcdf.defVar(ncid, 'roll', 'NC_FLOAT', timedim);
headvar = netcdf.defVar(ncid, 'head', 'NC_FLOAT', timedim);
Atimevar = netcdf.defVar(ncid, 'Atime', 'NC_DOUBLE', Atimedim);
Avar = netcdf.defVar(ncid, 'A', 'NC_FLOAT', [Atimedim axisdim]);
fsvar = netcdf.defVar(ncid, 'fs', 'NC_INT', []);
Afsvar = netcdf.defVar(ncid, 'Afs', 'NC_INT', []);
netcdf.endDef(ncid)

% Put variables
netcdf.putVar(ncid, timevar, DN(aoi));
netcdf.putVar(ncid, depthvar, p(aoi));
netcdf.putVar(ncid, pitchvar, pitch(aoi));
netcdf.putVar(ncid, rollvar, roll(aoi));
netcdf.putVar(ncid, headvar, head(aoi));
Atime = linspace(DN(aoi(1)), DN(aoi(end)), length(Aaoi));
netcdf.putVar(ncid, Atimevar, Atime);
netcdf.putVar(ncid, Avar, A(Aaoi, :));
netcdf.putVar(ncid, fsvar, fs);
netcdf.putVar(ncid, Afsvar, Afs);

% Close file
netcdf.close(ncid)

% Sanity check file by checking fs written correctly
ncfs = ncread(ncfile, 'fs');
if ncfs == fs
  disp(['NetCDF file exported to ' ncfile])
else
  disp 'Error exporting NetCDF file'N
end
