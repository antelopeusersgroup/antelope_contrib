function [d, t] = load_seispp_gather(fname);
% Matlab function to load dat files written by seispp program 
% export_to_matlab with the -h option.  With that option line one
% if the file has required attributes to describe a time series to
% reconstruct time.   
% 
% input - fname is file name to be read
%
% output:
%  d - matrix of data read from fname.  Number of rows is number of samples
%      in the gather.
%  t - computed vector of times for d.  Length of t is same as rows i d.
%%%%%%%%%%%%%%%%%%%%%%
fid=fopen(fname,'r');
[t0,dt,ns,nseis]=fscanf(fid,"%lf%lf%d%d");
d=zeros(ns,nseis);
% internet source says this is a way to define a variable number of columns
% There may be a simpler way
fmt=repmat('%lf',1,nseis);
for i=1:ns
  d(i,:)=fscanf(fid,fmt);
end
% fill t vector
t=zeros(ns,1);
for i=1:ns
  t(i)=t0+(i-1)*dt;
end
