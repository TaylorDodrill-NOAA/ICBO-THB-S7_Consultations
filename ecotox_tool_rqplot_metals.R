library(RColorBrewer)
library(readxl)
library(ggplot2)
library(stringr)
arsenic <- read_excel(path = "C:/Users/Taylor.Dodrill/Downloads/AquaticReport PR Metals 01.15.2024 (1).xlsx", sheet='Arsenic', guess_max = 10000)
cadmium <- read_excel(path = "C:/Users/Taylor.Dodrill/Downloads/AquaticReport PR Metals 01.15.2024 (1).xlsx", sheet='Cadmium', guess_max = 10000)
chromiumvi <- read_excel(path = "C:/Users/Taylor.Dodrill/Downloads/AquaticReport PR Metals 01.15.2024 (1).xlsx", sheet='ChromiumVI', guess_max = 10000)
copper <- read_excel(path = "C:/Users/Taylor.Dodrill/Downloads/AquaticReport PR Metals 01.15.2024 (1).xlsx", sheet='Copper', guess_max = 10000)


#RQs using fresh water cyanide criteria for Washington
# update this for arsenic and other metals once you update the spreadsheet with appropriate criteria (including saltwater, FW, PS)
# don't know if the criteria in the aquatic report PR metals will be the same (probably not), so will need to read in new spreadsheets
# use the above spreadsheets with formulas to caclulate appropriate RQs
# waiting for complete list of proposed criteria from EPA - we have this for WA, but not OR or CTCLUSI
# then pull plotting code from cyanide script
FWAcuteCN <- subset(cyanide, cyanide$'Acute or Chronic?'=='Acute'&cyanide$'Media Type'=='Fresh water') 
FWAcuteCN$'Taxon'=factor(FWAcuteCN$'Taxon', levels=c('Amphibians', 'Salmonid','Sturgeon','Other Fish','Invertebrates','Algae&Plants')) #sort order for taxa
FWAcuteCN$'Effect'=factor(FWAcuteCN$'Effect',levels=c('Behavior','Reproduction','Growth & Development','Mortality','Population','Ecosystem process')) #sort order for effects (Y axis)
FWAcuteCN$'Endpoint'=factor(FWAcuteCN$'Endpoint', levels = c('No effect','NOEC','NR-ZERO','LC0','Decrease','Change','Increase','LETC','LOEC','MATC',
                                                             'EC01','EC05','EC10','EC15','EC20','EC25','EC40','EC50','ET50','IC50','EC60','EC75','EC80',
                                                             'EC85','EC90','EC95','EC99','LC01','LC05','LC10','LT10','LC100','LC15','LC20','LC50','LT50',
                                                             'LC85','LC90','LC95','LC99','NR-LETH','BCF')) #sort order for endpoints (LCxx, etc)
FWAcuteCN$'Response Magnitude'=factor(FWAcuteCN$'Response Magnitude', levels = c('No effect','NOEC','NR-ZERO','Decrease','Change','Increase','LETC',
                                                                                 'LOEC','MATC','0','1','5','10','15','20','25','40','50','60','75','80','85',
                                                                                 '90','95','99','100','NR-LETH','BCF'))