library(shiny)
library(data.table)
library(dplyr)
library(tidyr)
library(rsconnect)

allocation <- fread(file = 'returns.csv')

allocation <- allocation %>%
              filter(Year %in% c(1993:2019))

converter <- function(x){
  x = gsub('%', '', x)
  x = as.numeric(x)
  x = x/100
  return (x)
}

allocation <- allocation %>% 
              mutate(newstockval = converter(allocation$`US Stock Market`), 
                     newbondval = converter(allocation$`Total US Bond Market`),
                     newEUROval = converter(allocation$`European Stocks`),
                     newASIAval = converter(allocation$`Pacific Stocks`),
                     newIGval = converter(allocation$`Long-Term Corporate Bonds`),
                     newHYval = converter(allocation$`High Yield Corporate Bonds`)
                     ) %>%
              select(Year, newstockval, newbondval, newEUROval, newASIAval,
                     newIGval, newHYval)

newalloc <- allocation %>%
              summarise(ID = c('USstocks', 'USbonds',
                               'EUstocks', 'APstocks',
                               'IGbonds', 'HYbonds'),
                        ER = c(mean(newstockval), mean(newbondval),
                               mean(newEUROval), mean(newASIAval),
                               mean(newIGval), mean(newHYval)),
                        SD = c(sd(newstockval), sd(newbondval),
                             sd(newEUROval), sd(newASIAval),
                             sd(newIGval), sd(newHYval)))
