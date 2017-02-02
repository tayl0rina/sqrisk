# Many things will change but this is the beginning of my package on spatial quantitative risk assessment for Europe
# Begin with a case study of Lumpy Skin disease for cattle imported in to the UK

# As this is a package, everything should really be written as functions.
# But for now let's just get some things written

# LSD for UK - what do I need?
# Import data for cattle coming in to the UK - from traces
# Prevalence data from spare for LSD in different countries
# Location of where the cattle go to - on a postcode or county level?
# the number of cattle in that area
# the shape files for plotting data on a spatial scale
# the transmission rate for LSD in cattle through different routes
# a function that calculates the risk at the correct spatial scale
# a function that plots the risk using the shape file


############## load data #############

# Assume that I have some data to load
# need to read about how to load data within a package - both what is saved there and how to have the user input data
cattle_movement <- read.csv('traces.csv')
prev <- read.csv('prevalence_spare.csv')
susc_cows <- read.csv('cattle_density.csv')

#really need to see what the data would actually look like to do anything with it.
