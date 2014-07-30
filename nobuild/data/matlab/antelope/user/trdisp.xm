function trdisp( tr, nmax )

nrecs = dbnrecs( tr );
s = db2struct( tr );

figure

nplots = min( nmax, nrecs );

for i=1:nplots
	subplot( nplots, 1, i )
	tr.record = i-1;
	data = trextract_data(tr);
	plot(data)
	ylabel(dbgetv(tr,'sta'))
end

return 
