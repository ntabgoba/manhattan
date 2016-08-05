# Predicting prices of apartments in Manhattan
# Manhattan Annualized Sales File.  All Sales For 2009 (January 2009 to December 2009). 			

# Set working dir and input data
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
setwd("/Users/ntabgoba/Desktop/manhatta")
list.files()
hattan <- read.csv("2009_manhattan.csv",header = TRUE, skip = 4)
dim(hattan)
head(hattan)
hatta <- as_data_frame(hattan)
View(hatta)
# Select columns relevant to analysis
mhatta <- select(hatta, NEIGHBORHOOD, TAX.CLASS.AT.PRESENT, BLOCK, BUILDING.CLASS.AT.PRESENT, ZIP.CODE,
                 RESIDENTIAL.UNITS, COMMERCIAL.UNITS, TOTAL.UNITS, LAND.SQUARE.FEET, GROSS.SQUARE.FEET, 
                 YEAR.BUILT, TAX.CLASS.AT.TIME.OF.SALE, BUILDING.CLASS.AT.TIME.OF.SALE, SALE.PRICE, SALE.DATE)

# rename the columns
mhattan <- rename(mhatta, neighbd = NEIGHBORHOOD, tclass_present = TAX.CLASS.AT.PRESENT, block = BLOCK , 
                  bclass_present = BUILDING.CLASS.AT.PRESENT,  zip = ZIP.CODE, res_units = RESIDENTIAL.UNITS,
                  com_units = COMMERCIAL.UNITS ,total_units = TOTAL.UNITS, land_ft = LAND.SQUARE.FEET, 
                  gross_ft = GROSS.SQUARE.FEET, year_built = YEAR.BUILT, tclass_sale = TAX.CLASS.AT.TIME.OF.SALE, 
                  bclass_sale = BUILDING.CLASS.AT.TIME.OF.SALE ,sale_price = SALE.PRICE ,sale_date = SALE.DATE)

