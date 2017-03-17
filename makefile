RCMD := R CMD BATCH --no-save --no-restore

data/uds_xpolity.csv: R/uds.R
	$(RCMD) R/uds.R

data/rep.csv: R/data.R data/uds_xpolity.csv
	$(RCMD) R/data.R
