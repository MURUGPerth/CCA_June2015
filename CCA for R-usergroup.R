To process Field Survey Data - use CCA
Soils from ten trees at ten different sites were collected.
These sites were paired i.e. a disturbed patch adjacent to a natural forest patch.
Soils from five healthy and five unhealthy/cankered trees were collected.
These soils were baited for the presence of the fine-root feeder Phytophthora.
We wanted to see if there were more Phytophthoras isolated from unhealthy/cankered trees.

#Set working directory.
#setwd("/Users/Highstat/applicat/HighlandStatistics/Courses/Data")
setwd("~/MURUG_2015/CCA-Field data")

#Import the data from a tab delimited ascii file
Fsurvey <- read.csv(file = "Fieldsurvey-R-group.csv",
                    header = TRUE,
                    dec = ".")
names(Fsurvey)
str(Fsurvey)

#Install the Vegan Communty Ecology Package
install.packages("vegan")

#Load packages from R and support functions that we wrote
source("HighstatLibV6.R")
library(lattice)  #For fancy multipanel graphs
library(vegan)

#R on a Mac does not like that the total of row of a spreadsheet is 0 (i.e. no Phytophtora isolated from a specific tree)
# These rows were deleted from the spreadsheet. Not sure if the following commands will work on a PC.

na.omit(Fsurvey)
na.omit(patch)

# The cca-command requires two subsets of the data
# First, create a subset with the patch details
patch <- Fsurvey[, 2:3]
# Then create a subset with the Phytophora's isolated details.
phytie <- Fsurvey[, 4:8]

patch
phytie

# The format of the cca-command
## my.cca <- cca(response.dataframe ~ predictor.var1 + predictor.var2 +…, data=predictor.dataframe  
  
my.cca <- cca(phytie ~ PATCH_DISTURB + TREE_HEALTH, data = patch)

# For a quick look
plot(my.cca)
# Quick look to see if anything significant
anova(my.cca)
# Better to try this
anova(my.cca, by="axis", permutations=999)
anova(my.cca, by='axis', permutations=999)

# And try this
M1 <- lm(P_CINNAM ~ factor(TREE_HEALTH)*factor(PATCH_DISTURB), data = Fsurvey)
M1
anova(M1)

plot(my.cca, xlim=c(-5,5), ylim=c(-5,5))

# Spread out the axis a bit
plot(my.cca, xlim=c(-1.5,1.5), ylim=c(-1.5,1.5),
     xlab='Site status', ylab='Tree health')



