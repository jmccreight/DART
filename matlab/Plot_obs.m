% Plot_obs
%
%

% Data Assimilation Research Testbed -- DART
% Copyright 2004-2007, Data Assimilation Research Section
% University Corporation for Atmospheric Research
% Licensed under the GPL -- www.gpl.org/licenses/gpl.html
%
% <next few lines under version control, do not edit>
% $URL$
% $Id$
% $Revision$
% $Date$

rad2deg = 45/atan(1);

KIND_U   =  1;
KIND_V   =  2;
KIND_PS  =  3;
KIND_T   =  4;
KIND_VR  = 12;
KIND_REF = 13;
KIND_U10 = 14;
KIND_V10 = 15;
KIND_T2  = 16;
KIND_Q2  = 17;
KIND_TD2 = 18;

var = input('Input obs to plot (VR, REF): ');
if strcmp(var,'REF')
   var_units = 'dBZ';
end
if strcmp(var,'VR')
   var_units = 'm/s';
end
lev = input('Input lev (m): ');
tol = input('Input margin (m): ');

map_proj = {'lambert', 'ups', 'mercator'};

if ( exist('Prior_Diag.nc','file') ~= 0 )
  fname = 'Prior_Diag';
elseif ( exist('Posterior_Diag.nc','file') ~= 0 )
  fname = 'Posterior_Diag';
elseif ( exist('True_State.nc','file') ~= 0 )
  fname = 'True_State';
else
  fname = input('Enter the name of the netCDF file containing domain information: ');
end

if strcmp(fname(1:8),'wrfinput')

     nc = netcdf( fname , 'nowrite' ) ;

     stdlat1 = nc.TRUELAT1(:);
     stdlat2 = nc.TRUELAT2(:);
     cen_lat = nc.CEN_LAT(:);
     cen_lon = nc.CEN_LON(:);
     mp = nc.MAP_PROJ(:);

     close(nc);

     id = 1;

     xlon = getnc(fname, 'XLONG');
     xlat = getnc(fname, 'XLAT');

else

     stdlat1 = getnc(fname, 'TRUELAT1');
     stdlat2 = getnc(fname, 'TRUELAT2');
     cen_lat = getnc(fname, 'CEN_LAT');
     cen_lon = getnc(fname, 'CEN_LON');
     mp = getnc(fname, 'MAP_PROJ');

     num_domains = size(mp,1);

if (num_domains > 1)

   disp(['Number of domains: ',int2str(num_domains)])
   id = input('Input domain id: ');

else

   id = 1;

end

     xlon = getnc(fname, ['XLON_d0',int2str(id)]);
     xlat = getnc(fname, ['XLAT_d0',int2str(id)]);

end

we = size(xlon, 2);
sn = size(xlat, 1);
minlat = min(xlat(:)); maxlat = max(xlat(:));
minlon = min(xlon(:)); maxlon = max(xlon(:));

if(exist('a','var') == 0)
   a = ReadASCIIObsSeq('obs_seq.final');
end

days = -1;
secs = -1;
j = 0;
disp('Available times')
disp('day     second')
for i = 1:a.num_obs
   if (a.days(i) ~= days | a.secs(i) ~= secs)
     j = j + 1;
     day(j) = a.days(i);
     sec(j) = a.secs(i);
     beg(j) = i;
     days = a.days(i);
     secs = a.secs(i);
     disp([int2str(days),' ',int2str(secs),' ',int2str(j)])
   end
end
beg(j+1) = a.num_obs + 1;

j = input(['Enter period # of interest ( 1 - ',int2str(j),' ): ']);

m = min([2 a.num_copies]);

string1 = sprintf('%s (%s)  %d (m)  day = %d  sec = %d  ',var,var_units,lev,a.days(beg(j)),a.secs(beg(j)));

metadata = {'observations', 'prior ensemble mean'};

for pane = 1:m,

iu=0;
iv=0;
it=0;
iu10=0;
iv10=0;
it2=0;
itd2=0;
ips=0;
ivr=0;
iref=0;

field = zeros(sn,we);

field(:,:) = NaN ;

figure(pane);
%subplot(1,m,pane);

axesm(map_proj{mp(id)},'Origin',[0 cen_lon(id) 0],'MapParallels',[stdlat1(id) stdlat2(id)],...
      'MapLatLimit',[minlat maxlat],'MapLonLimit',[minlon maxlon]); framem;

[xlim ylim]=mfwdtran([xlat(1,1) xlat(sn,we)],[xlon(1,1) xlon(sn,we)]);
set(gca,'xlim',[min(xlim(:)) max(xlim(:))]);
set(gca,'ylim',[min(ylim(:)) max(ylim(:))]);

plotm(coast,'color',[0 0 0]);
plotm(usalo('statebvec'),'color',[0 0 0]);
plotm(usalo('conusvec'),'color',[0 0 0]);

%title({string1,a.metadata(pane)}, 'Fontsize',12)
title([string1,metadata{pane}], 'Fontsize',12)

for i = beg(j):min([(beg(j+1)-1) a.num_obs]),
%for i = 1:180,

   lon = rad2deg*a.loc(i,1);
   lat = rad2deg*a.loc(i,2);
   hei = a.loc(i,3);

   if ( a.kind(i) == KIND_U )

     iu = iu + 1;

   scatterm(lat,lon,'xb')

   elseif ( a.kind(i) == KIND_V )

     iv = iv + 1;

   elseif ( a.kind(i) == KIND_T )

     it = it + 1;

   scatterm(lat,lon,'+r')

   elseif ( a.kind(i) == KIND_U10 )

     iu10 = iu10 + 1;

     uwind = a.obs(pane,i);

   elseif ( a.kind(i) == KIND_V10 )

     iv10 = iv10 + 1;

     if ( (uwind ~= -888888.0) && (a.obs(pane,i) ~= -888888.0) )
%        quiverm(lat,lon,a.obs(pane,i),uwind)
     end

   elseif ( a.kind(i) == KIND_T2 )

     if (a.obs(pane,i) ~= -888888.0)
       textm(lat,lon,[num2str(round(a.obs(pane,i)-273.15)),' '],...
     'VerticalAlignment','bottom',...
     'HorizontalAlignment','right')
     end

     it2 = it2 + 1;

   scatterm(lat,lon,'og')

   elseif ( a.kind(i) == KIND_TD2 )

     if (a.obs(pane,i) ~= -888888.0)
       textm(lat,lon,[num2str(round(a.obs(pane,i)-273.15)),' '],...
     'VerticalAlignment','top',...
     'HorizontalAlignment','right')
     end

     itd2 = itd2 + 1;

   scatterm(lat,lon,'og')

   elseif ( a.kind(i) == KIND_PS )

     if (a.obs(pane,i) ~= -888888.0)
       textm(lat,lon,[' ',num2str(round(a.obs(pane,i)/100))],...
     'VerticalAlignment','bottom',...
     'HorizontalAlignment','left')
     end

     ips = ips + 1;

   scatterm(lat,lon,'og')

   elseif ( a.kind(i) == KIND_VR )

     ivr = ivr + 1;

     if strcmp(var,'VR')
        if((hei > (lev-tol)) && (hei < (lev+tol)))
          scatterm(lat,lon,16,a.obs(pane,i),'s','filled')
        end
     end

   elseif ( a.kind(i) == KIND_REF )

     iref = iref + 1;

     if strcmp(var,'REF')
        if(hei > (lev-tol) && hei < (lev+tol))
     if(lon > 180)
        lon = lon - 360;
     end
     [C k] = min(reshape((xlat - lat).^2 + (xlon - lon).^2,we*sn,1));
%     field(k) = a.obs(pane,i);
     if a.obs(pane,i) ~= 0.0
        field(k) = 10.0*log10(a.obs(pane,i));
     else
     field(k) = -16.0;
     end
        end
     end

   else

     disp(['Kind for obs ',int2str(i),' is ',int2str(a.kind(i))])

   end

   hold on

end

if min(min(field)) ~= max(max(field))
   if strcmp(var,'REF')
     eval('dbz_colors')
   end
   h = pcolorm(xlat,xlon,field);
%   caxis([min(iso(:)),max(iso(:))]);
end

cb = colorbar('vert'); set(cb,'Fontsize',12);

   wysiwyg

disp(sprintf('# of U %d', iu))
disp(sprintf('# of V %d', iv))
disp(sprintf('# of T %d', it))
disp(sprintf('# of Vr %d', ivr))
disp(sprintf('# of Ref %d', iref))
disp(sprintf('# of U10 %d', iu10))
disp(sprintf('# of V10 %d', iv10))
disp(sprintf('# of T2 %d', it2))
disp(sprintf('# of TD2 %d', itd2))
disp(sprintf('# of PS %d', ips))

end
