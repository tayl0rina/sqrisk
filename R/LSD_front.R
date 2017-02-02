##### Script which will be the frontline of the program
## Input the librarys required here
## And all the datafiles that are necessary
## So that people using this can easily change their data files to be what they require instead

###########################################
############ libraries ####################
###########################################

library(maptools)
library(dismo)
library(rgdal)
#library(inla)




###########################################
############# data ########################
###########################################

folder <- 'C:/Users/p991915/Desktop/LSD_Data'
prevalence <- read.csv(paste0(folder, '/LSD_spare/trAoutPrev_Exp_LSD.csv'), header = TRUE, row.names = 1, stringsAsFactors = FALSE)
alltraces <- read.csv(paste0(folder, '/Cows_for_GIS_2015.csv'), header = TRUE, stringsAsFactors = FALSE)
livestock <- read.table(paste0(folder, '/CattlePopn20150701.txt'), header = TRUE, sep = '\t')
codes <- read.csv(paste0(folder, '/CountryCodes.csv'), header = TRUE, stringsAsFactors = FALSE)

year <- 2015


###########################################
############ make data pretty #############
###########################################


# for this case study just looking at imports to the UK

#prevalence data
importPrev <- prevalence['GBR',]

#traces data
# get rid of movement from UK to UK - later change this to be movement from one country to the same country
UKmovement <- alltraces$Country.of.Origin.Code =='GB'
tracesUK <- alltraces[!UKmovement,]

#extract from traces just the data we need - location from, destination postcode, quantity and year
traces <- data.frame(tracesUK$Country.of.Origin.Name, tracesUK$Business.postal.code.1, tracesUK$Qty..INTRA.,
                     rep(year, length(tracesUK[,1])), rep(NA, length(tracesUK[,1])), stringsAsFactors = FALSE)
names(traces) <- c('Origin.Name', 'Destination.Postcode', 'Quantity', 'Year', 'Origin.Code')


#match up data - traces has two character country code (or name), while ISO code is 3 characters as used elsewhere
#first a fix for countries that have changed names
traces$Origin.Name[traces$Origin.Name == 'Czech Republic'] <- 'Czechia'

for(i in 1:length(traces[,1])){
  name <- which(codes$Country.or.area.name == traces$Origin.Name[i])
  traces$Origin.Code[i] <- codes[names,3]
}


names(livestock) <- c('Identifier', 'Number.Cattle', 'Easting', 'Northing')

data("wrld_simpl")


## separate source files to:
#plot the current distribution of livestock
#plot where cattle are being sent to - do we want to do down the network map thing Alex was doing?
#work out species distribution when required
#sort out the shape files part
#calculate the risk on the different scales
