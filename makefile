CORES = 8 # default

data/rep.csv: R/data.R
	R --no-save --no-restore < $< --args 1981 2010 TRUE > $@

R/analysis.Rout: R/analysis.R
	mkdir figures
	R --no-save --no-restore < $< --args $(CORES) > $@

upload:
	find . -name '*.R' -o -name '*.csv' -o -name '*.txt' -o -name '*.dta' \
               -o -name '*.sh' -o -name '*.xlsx' -o -name '*.dat' > upload.txt
	rsync -azv --delete --files-from=upload.txt $(PWD) zmj102@aci-b.aci.ics.psu.edu:work/mvpv/

download:
	rsync -azv --include=*/ --include=*.png --include=*.rda --exclude=* zmj102@aci-b.aci.ics.psu.edu:work/mvpv/ $(PWD)

clean:
	rm R/*.Rout
	rm figures/*
	rm output/*
	rm data/rep.csv
