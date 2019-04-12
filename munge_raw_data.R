library(lubridate)
library(data.table)
library(parallel)

setwd('~/Downloads/SCSG data')

pa_files <- list.files()

pa_import <- function(pa_file){
		#metadata from filename
		latlong <- strsplit(regmatches(pa_file, gregexpr("(?<=\\().*?(?=\\))", pa_file, perl=T))[[1]], " ")[[1]]
		lat <- latlong[1]
		long <- latlong[2]

		#station id
		sid <- gsub("SCSG_|Scsg_", "", substring(pa_file, 1,7))

		s1 <- fread(pa_file)
		if(ncol(s1)<9){
			s1 <- as.data.table(read.csv(pa_file))
			s1[, X := NULL]
			setnames(s1, "Humidity_.", 'Humidity_%')
		}
		s1[, V11 := NULL]
		s1[, timestamp:=ymd_hms(created_at, tz="America/Los_Angeles")]
		s1[, entry_id:=NULL]
		s2 <- s1[, c('timestamp', 'Temperature_F', 'Humidity_%', colnames(s1)[colnames(s1) %like% 'm3']), with=F]
		s2l <- melt(s2, id.var='timestamp')
		s2l[, round_dt := round_date(timestamp, unit='10 mins')]
		s3 <- dcast.data.table(s2l, round_dt~variable, 'value', fun.aggregate = function(x) round(mean(x),2))
		s3[, lat:=lat]
		s3[, long:=long]
		s3[, station_id:=sid]
		s3[, sensor:= strsplit(pa_file, " ")[[1]][4]]
}

all_pa <- mclapply(pa_files[pa_files %like% "Primary"], pa_import, mc.cores = 12)

#bad column names
proper_col_names <- colnames(all_pa[[1]][1])
setnames(all_pa[[9]], proper_col_names)
setnames(all_pa[[12]], proper_col_names)

all_data <- do.call(rbind, all_pa)

saveRDS(all_data, '~/Dropbox/Ongoing/SouthGatePA/primary_scsgdata.rds')