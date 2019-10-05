%% Load .mat files
jiggle_dir = uigetdir('', 'Choose jiggle root folder');

% load prh
load([jiggle_dir '/data-raw/Example Data/mn160727-11 10Hzprh.mat'])
% load Adata
load([jiggle_dir '/data-raw/Example Data/mn160727-11Adata.mat'])

%% Export NetCDF
% Create .nc file
ncfile = [jiggle_dir '/data-raw/mn160727-11 10Hzprh.nc'];
ncid = netcdf.create(ncfile, 'CLOBBER');

% Define dimensions (time, Atime, axis)
change_fs = @(x, fs1, fs2) (x - 1) * fs2 / fs1 + 1;
Astart = change_fs(find(tagon, 1), fs, Afs);
Aend = change_fs(find(tagon, 1, 'last') + 1, fs, Afs) - 1;
Atagon = Astart:Aend;
timedim = netcdf.defDim(ncid, 'time', sum(tagon));
Atimedim = netcdf.defDim(ncid, 'timeA', length(Atagon));
axisdim = netcdf.defDim(ncid, 'axis', 3);

% Define variables (time, depth, pitch, roll, head, A, fs, Afs)
timevar = netcdf.defVar(ncid, 'time', 'NC_DOUBLE', timedim);
depthvar = netcdf.defVar(ncid, 'depth', 'NC_FLOAT', timedim);
pitchvar = netcdf.defVar(ncid, 'pitch', 'NC_FLOAT', timedim);
rollvar = netcdf.defVar(ncid, 'roll', 'NC_FLOAT', timedim);
headvar = netcdf.defVar(ncid, 'head', 'NC_FLOAT', timedim);
Awvar = netcdf.defVar(ncid, 'Aw', 'NC_FLOAT', [timedim axisdim]);
Mwvar = netcdf.defVar(ncid, 'Mw', 'NC_FLOAT', [timedim axisdim]);
Gwvar = netcdf.defVar(ncid, 'Gw', 'NC_FLOAT', [timedim axisdim]);
Atimevar = netcdf.defVar(ncid, 'Atime', 'NC_DOUBLE', Atimedim);
Avar = netcdf.defVar(ncid, 'A', 'NC_FLOAT', [Atimedim axisdim]);
fsvar = netcdf.defVar(ncid, 'fs', 'NC_INT', []);
Afsvar = netcdf.defVar(ncid, 'Afs', 'NC_INT', []);
netcdf.endDef(ncid)

% Put variables
netcdf.putVar(ncid, timevar, DN(tagon));
netcdf.putVar(ncid, depthvar, p(tagon));
netcdf.putVar(ncid, pitchvar, pitch(tagon));
netcdf.putVar(ncid, rollvar, roll(tagon));
netcdf.putVar(ncid, headvar, head(tagon));
netcdf.putVar(ncid, Awvar, Aw(tagon, :));
netcdf.putVar(ncid, Mwvar, Mw(tagon, :));
netcdf.putVar(ncid, Gwvar, Gw(tagon, :));
Aperiod = 1 / 24 / 3600 / Afs;
DNstart = DN(find(tagon, 1));
DNend = DN(find(tagon, 1, 'last') + 1) - Aperiod;
Atime = linspace(DNstart, DNend, length(Atagon));
netcdf.putVar(ncid, Atimevar, Atime);
netcdf.putVar(ncid, Avar, A(Atagon, :));
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
