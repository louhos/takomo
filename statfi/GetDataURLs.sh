# First time
wget -x -S -o logfile_Get_database_first_time.txt -i http://pxweb2.stat.fi/database/StatFin/StatFin_rap.csv

# Hourly update
wget --limit-rate=500k -x -N -o logfile_run_hourly_to_update.txt -i http://pxweb2.stat.fi/database/StatFin/StatFin_rap.csv




