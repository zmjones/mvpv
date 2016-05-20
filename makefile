data/rep.csv: R/data.R data/uds_xpolity.csv
	R CMD BATCH --no-save --no-restore --args 1981 2008 R/data.R

data/uds_xpolity.csv: R/uds.R
	R CMD BATCH --no-save --no-restore R/uds.R

R/analysis.Rout: R/analysis.R
	mkdir -p figures
	R CMD BATCH --no-save --no-restore R/analysis.R

upload:
	rsync -azv --delete --files-from=upload.txt $(PWD) zmj102@aci-b.aci.ics.psu.edu:work/mvpv

download:
	rsync -azv --files-from=download.txt zmj102@aci-b.aci.ics.psu.edu:work/mvpv $(PWD)

clean:
	rm R/*.Rout
	rm figures/*
	rm output/*
	rm data/rep.csv
