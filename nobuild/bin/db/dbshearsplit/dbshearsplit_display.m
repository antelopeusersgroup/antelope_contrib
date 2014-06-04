function showsplitting( split, database, varargin )
% SHOWSPLITTING( split, database, varargin ) displays waveforms 
%    and particle motion plots for the event possessing the given 
%    split.  It calls a fortran program which retrieves the data from
%    an antelope database and then generates figures showing radial and 
%    transverse waveforms before and after splitting, closeups of 
%    the effects of shifting, and particle motion plots.  Finally,
%    it generates a contour plot of the variance between shear-wave 
%    splitting parameters.  All of these can then be optionally saved 
%    to postscript files for safekeeping.
%
%    split is the origin id of the event to be examined in the
%    database provided.
%
%    database is the full database name, including path.
%    
%    varargin is the variable options for the function.  SHOWSPLITTING
%    will currently accept an option of 'print' to print out figures
%    to postscript files using the color postscript 2 driver.

basename='matlab_data/';

% First, we create a temporary directory.  This will later hold text
% files containing the waveforms we need.
warning off MATLAB:MKDIR:DirectoryExists;
mkdir( basename );

% Next, we execute the fortran code that rotates and transforms these
% waveforms and dumps these to the appropriate textfiles.
splitstr = num2str(split);

% errfile='showsplitting.log';
errfile='/dev/null';
system(['dbshearsplit_display -d ' database ' ' splitstr '>' errfile ]);

% Read in specs from specs file
specid = fopen([basename splitstr '.specs']);
if specid == -1
    disp(['showsplitting failed.  Please see ' errfile ' for details.'])
    return;
end
etime = fgets( specid ) ;
orid = str2num( fgets( specid ) );
sta = fgets( specid );
filter = fgets( specid );
fclose(specid);

% For display of multiple events, only show the contour plots.
if orid ~= -1 
    if filter == -1
        filter = 'none';
    end

    oridstr = num2str(orid);
    [result,timestr] = system(['epoch "+%e %B %Y (%j) %T %Z" ' etime]);

    ro = load ([basename splitstr '.ro']);
    to = load ([basename splitstr '.to']);
    rl = load ([basename splitstr '.rl']);
    tl = load ([basename splitstr '.tl']);
    scr = load ([basename splitstr '.scr']);
    sct = load ([basename splitstr '.sct']);
    scl = load ([basename splitstr '.scl']);
    xy1 = load ([basename splitstr '.xy1']);
    xy2 = load ([basename splitstr '.xy2']);
    pick = load([basename splitstr '.picks']);

    allwfs = cat( 1, ro(:,2), rl(:,2), to(:,2), tl(:,2) );

    ymax = max( allwfs ) * 1.05;
    ymin = min( allwfs ) * 1.05;

    % Normalize the time-window comparison vectors.
    scr(:,2) = scr(:,2)./max(abs(scr(:,2)));
    sct(:,2) = sct(:,2)./max(abs(sct(:,2)));
    scl(:,2) = scl(:,2)./max(abs(scl(:,2)));

    % Plot waveforms with picks
    figure(2)
    orient landscape
    clf
    subplot(2,1,1)
    plot(ro(:,1),ro(:,2),'g')
    axis tight
    ylim( [ymin ymax] );

    hold on
    yy = get(gca,'Ylim');
    dy = 0.1 * ( yy(2) - yy(1) );
    nyy(1) = yy(1) + dy;
    nyy(2) = yy(2) - dy;
    plot([pick(1) pick(1)],nyy,'r-')
    plot([pick(2) pick(2)],nyy,'b-')
    text(pick(1),nyy(2),' A')
    text(pick(2),nyy(2),' F')

    title(['Event ' oridstr ' Station ' sta ' Radial Waveform'], 'FontWeight', 'Bold')
    xlabel('time (seconds)','FontWeight','Bold')
    ylabel('velocity (nm/sec)','FontWeight','Bold')

    subplot(2,1,2)
    plot(to(:,1),to(:,2),'g')
    axis tight
    ylim( [ymin ymax] );

    hold on
    yy = get(gca,'Ylim');
    dy = 0.1 * ( yy(2) - yy(1) );
    nyy(1) = yy(1) + dy;
    nyy(2) = yy(2) - dy;
    plot([pick(1) pick(1)],nyy,'r-')
    plot([pick(2) pick(2)],nyy,'b-')
    text(pick(1),nyy(2),' A')
    text(pick(2),nyy(2),' F')

    title(['Event ' oridstr ' Station ' sta ' Transverse Waveform'], 'FontWeight', 'Bold')
    xlabel('time (seconds)','FontWeight','Bold')
    ylabel('velocity (nm/sec)','FontWeight','Bold')

    figure(3)
    clf
    orient landscape
    subplot(2,1,1)
    plot(rl(:,1),rl(:,2),'g')
    axis tight
    ylim( [ymin ymax] );

    hold on
    yy = get(gca,'Ylim');
    dy = 0.1 * ( yy(2) - yy(1) );
    nyy(1) = yy(1) + dy;
    nyy(2) = yy(2) - dy;
    plot([pick(1) pick(1)],nyy,'r-')
    plot([pick(2) pick(2)],nyy,'b-')
    text(pick(1),nyy(2),' A')
    text(pick(2),nyy(2),' F')

    title(['Event ' oridstr ' Station ' sta ' Radial Waveform (lagged) '], 'FontWeight', 'Bold')
    xlabel('time (seconds)','FontWeight','Bold')
    ylabel('velocity (nm/sec)','FontWeight','Bold')

    subplot(2,1,2)
    plot(tl(:,1),tl(:,2),'g')
axis tight
    ylim( [ymin ymax] );

    hold on
    yy = get(gca,'Ylim');
    dy = 0.1 * ( yy(2) - yy(1) );
    nyy(1) = yy(1) + dy;
    nyy(2) = yy(2) - dy;
    plot([pick(1) pick(1)],nyy,'r-')
    plot([pick(2) pick(2)],nyy,'b-')
    text(pick(1),nyy(2),' A')
    text(pick(2),nyy(2),' F')

    title(['Event ' oridstr ' Station ' sta ' Transverse Waveform (lagged) '], 'FontWeight', 'Bold')
    xlabel('time (seconds)','FontWeight','Bold')
    ylabel('velocity (nm/sec)','FontWeight','Bold')

    % Next, plot relevent time windows before and after shifting
    figure(4)
    clf
    orient landscape
    subplot(2,2,1)

    plot(scr(:,1),scr(:,2),'b')
    hold on
    plot(sct(:,1),sct(:,2),'r--')
    title(['Event ' oridstr ' Station ' sta ' Before Shifting '], 'FontWeight', 'Bold')
    legend('fast','slow')
    xlabel('time (seconds)','FontWeight','Bold')
    ylabel('velocity (normalized)','FontWeight','Bold')
    axis tight

    subplot(2,2,2)
    plot(scr(:,1),scr(:,2),'b')
    hold on
    plot(scl(:,1),scl(:,2),'r--')
    title(['Event ' oridstr ' Station ' sta ' After Shifting '], 'FontWeight', 'Bold')
    legend('fast','slow')
    xlabel('time (seconds)','FontWeight','Bold')
    ylabel('velocity (normalized)','FontWeight','Bold')
    axis tight

    % Finally, plot particle motion plots for the above
    subplot(2,2,3)
    plot(xy1(:,1),xy1(:,2),'b')
    axis tight
    %title(['Event ' oridstr ' Station ' sta ' Particle Motion Before Shifting'], 'FontWeight', 'Bold')
    title(['Particle Motion Before Shifting'], 'FontWeight', 'Bold')
    xlabel('Component B','FontWeight','Bold')
    axis tight
    ylabel('Component A','FontWeight','Bold')

    subplot(2,2,4)
    plot(xy2(:,1),xy2(:,2),'b')
    axis tight
    %title(['Event ' oridstr ' Station ' sta ' Particle Motion After Shifting'], 'FontWeight', 'Bold')
    title(['Particle Motion After Shifting'], 'FontWeight', 'Bold')
    xlabel('Component B','FontWeight','Bold')
    ylabel('Component A','FontWeight','Bold')
    axis tight

    % Print the lovely figures to a postscript file for keeping.
    if ( ( length(varargin) > 0 ) && ( strcmp(varargin(1),'print' ) ) )
        print('-f2','-dpsc2', [splitstr '.ps'] );
        print('-f3','-dpsc2', '-append', [splitstr '.ps'] );
        print('-f4','-dpsc2', '-append', [splitstr '.ps'] );
    end
end

dbshearsplit_contour

if ( ( length(varargin) > 0 ) && ( strcmp(varargin(1),'print' ) ) )
    orient landscape
    print('-f1','-dpsc2', '-append', [splitstr '.ps'] );
end

% Once we're done reading in this data, we can remove the directory
% to clean things up.
rmdir( basename, 's' );

function dbshearsplit_contour( varargin )
% DBSHEARSPLIT_CONTOUR( printoption ) displays a contour map of the variance
% of the shear-wave splitting parameters and marks the minimum of
% the variance with an asterisk.  If the optional parameter is provided and
% is set to 'ps', the figure is saved to the file 'variance.ps'. if the 
% first parameter is set to 'png', the figure is saved to the file 
% 'variance.png'.  Otherwise, the figures are not retained.

baselocation='matlab_data/';
set(0,'defaultaxesfontsize',12);
set(0,'defaulttextfontsize',12);

data = load ([baselocation 'contour.var']);
dp = load( [baselocation 'contour.specs']);
xstar = dp(1);
ystar = dp(2);
dt = dp(3);
ndf = dp(4);
dim2 = 181;
dim1 = length(data) / dim2;

x = 0:dt:(dim1-1)*dt;
y = -90:1:(dim2-91);

% We need to do some trickery here to shift the coordinate system from
% 0-180 to -90 to 90.
data = cat(1,data(((90*dim1)+1):length(data)), data(1:(90*dim1)) );

% Go from a 1D array to a 2D array
var = reshape(data,dim1,dim2)';

% The original shear-wave splitting code plotted on the interval from 
% -90 to 90.  We need to correct our ystar point to reflect this. xstar is
% fine.
if  ystar > 90 
    ystar = ystar - 360;
end

% Next, plot relevent time windows before and after shifting
figure(1)
clf
orient portrait

c=colormap('hot');
cc = flipud(c);
colormap(cc);

pcolor(x,y,var);
shading interp;
hold on
contour(x,y,var,'k:');
plot([xstar xstar],[ystar ystar],'k*');

title(['Variance of Shear-Wave Splitting Parameters (ndf=' num2str(ndf) ')'], 'FontWeight', 'Bold', 'FontSize', 15);
axis tight
xlabel('Lag (seconds)','FontWeight','Bold')
ylabel('Azimuth (degrees)','FontWeight','Bold')

colorbar

% Print the lovely figure to a postscript file for keeping.
if ( ( length(varargin) > 0 ) && ( strcmp(varargin(1),'ps' ) ) )
    print('-f1','-dpsc2', 'variance.ps' );
else
    if ( ( length(varargin) > 0 ) && ( strcmp(varargin(1),'png' ) ) ) 
        print('-f1','-dpng', 'variance.png' );
    end
end

