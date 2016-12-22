data/rep.csv: R/data.R data/uds_xpolity.csv
	R CMD BATCH --no-save --no-restore R/data.R

data/uds_xpolity.csv: R/uds.R
	R CMD BATCH --no-save --no-restore R/uds.R
