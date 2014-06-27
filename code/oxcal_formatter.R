# read data in, three columns Name = lab code, Date = radiocarbon age, Uncertainty = error
dates <- read.csv('E:\\My Documents\\My UW\\Research\\1206 M2 excavation\\Dating\\Date_table_from_paper_on_1989_dig_C14_forOxCal.csv', stringsAsFactors = FALSE)

names(dates) <- c("Name", "Date", "Uncertainty")

# construct OxCal format
oxcal_format <- paste0('R_Date(\"',  gsub("^\\s+|\\s+$", "", dates$Name), '\",', dates$Date, ',', dates$Uncertainty, ');')
# inspect
cat(oxcal_format)

# write formatted dates to text file
write.table(oxcal_format, file = 'oxcal_format.txt', row.names = FALSE, col.names = FALSE, quote = FALSE)

# find location of text file
getwd()

# Now 95% ready for pasting into OxCal batch conversion
# at https://c14.arch.ox.ac.uk/oxcal/OxCal.html

# In OxCal, File -> New then click the table view button (fourth along the top, no tool tips on hover sadly!) and paste in the dates between this (without #)

# Plot()
# {
# ...insert dates here...
# };

# Then edit format and settings to return results in BP (before running) and get median and sigma (after running)
# using IntCal 13 curve
# then click File -> Run, then File -> Save As to get calibrated dates in a CSV