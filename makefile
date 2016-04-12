CORES = 8 # default

data/rep.csv: R/data.R
	R --no-save --no-restore uds_reestimation/uds.R
	R --no-save --no-restore < $< --args 1981 2008 > $@

R/analysis.Rout: R/analysis.R
	mkdir -p figures
	mkdir -p output
	R --no-save --no-restore < $< --args $(CORES) > $@

upload:
	rsync -azv --delete --files-from=upload.txt $(PWD) zmj102@aci-b.aci.ics.psu.edu:work/mvpv

download:
	rsync -azv --files-from=download.txt zmj102@aci-b.aci.ics.psu.edu:work/mvpv $(PWD)

clean:
	rm R/*.Rout
	rm figures/*
	rm output/*
	rm data/rep.csv
