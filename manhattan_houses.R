# Predicting prices of apartments in Manhattan
# Manhattan Annualized Sales File.  All Sales For 2009 (January 2009 to December 2009). 			
# https://www1.nyc.gov/assets/finance/downloads/.../2009_manhattan.xls
# Set working dir and input data
setwd("/Users/ntabgoba/Desktop/manhatta")
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
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

# change column data types to their actual types
mhattan_c <- transmute(mhattan,
                    neighbd,
                    tclass_present,
                    block,
                    bclass_present,
                    zip,
                    res_units,
                    com_units,
                    total_units,
                    land_ft = parse_number(mhattan$land_ft),
                    gross_ft = parse_number(mhattan$gross_ft),
                    year_built,
                    tclass_sale = parse_factor(mhattan$tclass_sale,levels = c(1,2,3,4)),
                    bclass_sale,
                    sale_price = parse_number(mhattan$sale_price),
                    sale_date = parse_date(mhattan$sale_date, "%m/%d/%y")
)
str(mhattan_c)

max(mhattan_c$year_built)
min(mhattan_c$year_built)
# EXPLORATORY.
# year vs houses
ggplot(data = mhattan_c) +
        geom_bar( mapping = aes(x = year_built,fill = neighbd,size=5,na.rm = TRUE)) +
        coord_cartesian(xlim = c(1900,2011))
# total_units sold per year of built, at each tclass_sale
#
ggplot(data = mhattan_c) +
        geom_bar(mapping = aes(x = year_built, y = total_units,fill = tclass_sale), stat = "identity")+
        coord_cartesian(xlim = c(1900,2011))
# commercial vs residential 
#Between commercial and residential which were mostly speculated and later sold?
mhattan_cr <- mhattan_c  %>%
        filter(res_units >= 1 & res_units < 150,com_units >=1) 
ggplot(data = mhattan_cr, mapping = aes(x = res_units, y = com_units, color = tclass_sale),na.rm = TRUE) +
        geom_point(position = "jitter") +
        geom_smooth(se = TRUE, color = "yellow") +
        ggtitle("Commercial compared to Residential units, per Tax Class at Sale") +
        facet_wrap(~neighbd)

