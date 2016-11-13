# World Map - replace 'indactorName' with desired dataset
library(ggplot2)
library(maps)
library(readr)

# Choosing which indicator to plot
indicatorName <- "Net income from abroad (current USdollar)"
indicatorYear <- 1980

indicators <- read_csv("../input/Indicators.csv")

filtered <- indicators[indicators$IndicatorName==indicatorName & indicators$Year==indicatorYear,]

# cleaning country names so that data maps correctly
correction <- c("Antigua and Barbuda"="Antigua", "Bahamas, The"="Bahamas", "Brunei Darussalam"="Brunei", "Cabo Verde"="Cape Verde", "Congo, Dem. Rep."="Democratic Republic of the Congo", "Congo, Rep."="Republic of Congo", "Cote d'Ivoire"="Ivory Coast", "Egypt, Arab Rep."="Egypt", "Faeroe Islands"="Faroe Islands", "Gambia, The"="Gambia", "Iran, Islamic Rep."="Iran", "Korea, Dem. Rep."="North Korea", "Korea, Rep."="South Korea", "Kyrgyz Republic"="Kyrgyzstan", "Lao PDR"="Laos", "Macedonia, FYR"="Macedonia", "Micronesia, Fed. Sts."="Micronesia", "Russian Federation"="Russia", "Slovak Republic"="Slovakia", "St. Lucia"="Saint Lucia", "St. Martin (French part)"="Saint Martin", "St. Vincent and the Grenadines"="Saint Vincent", "Syrian Arab Republic"="Syria", "Trinidad and Tobago"="Trinidad", "United Kingdom"="UK", "United States"="USA", "Venezuela, RB"="Venezuela", "Virgin Islands (U.S.)"="Virgin Islands", "Yemen, Rep."="Yemen")

for (c in names((correction))) {
  filtered[filtered$CountryName==c,"CountryName"] = correction[c]
}

map.world <- merge(x=map_data(map="world"),
                   y=filtered[,c("CountryName","Value")],
                   by.x="region",
                   by.y="CountryName",
                   all.x=TRUE)
map.world <- map.world[order(map.world$order),]

p <- ggplot(map.world) +
     geom_map(map=map.world, aes(map_id=region, x=long, y=lat, fill=Value)) + 
     scale_fill_gradient(low = "brown3", high = "green", guide = "colourbar") +
     coord_equal() +
     theme(axis.line=element_blank(),
           axis.text.x=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks=element_blank(),
           axis.title.x=element_blank(),
           axis.title.y=element_blank(),
           panel.background=element_blank(),
           panel.border=element_blank(),
           panel.grid.major=element_blank(),
           panel.grid.minor=element_blank(),
           plot.background=element_blank(),
           legend.title=element_blank(),
           legend.position="bottom") +
     ggtitle(paste0(indicatorName, " in ", indicatorYear))
ggsave("map.png", p, width=7, height=4, units="in")





# Ranknig by S&P Performance
library(readr)
library(dplyr)
library(ggplot2)

db <- read_csv("../input/Indicators.csv")

db %>% 
filter(IndicatorCode == "CM.MKT.INDX.ZG") -> mData

# Filter for countries w/ complete datasets
mData %>% 
  filter(Year == 1990) %>% 
  select(CountryCode) -> conSet

cData = left_join(conSet, mData, by = "CountryCode")

#' Calculate Cummulative % Growth
cData %>%
  group_by(CountryCode) %>% 
  mutate(Growth = 1 + Value / 100) %>% 
  mutate(CumGrowth = cumprod(Growth)) %>% 
  ungroup() %>% 
  select(CountryName, Year, CumGrowth) -> growth

# Modify start point for consistency between plots
start = as.data.frame(cbind(unique(growth$CountryName), rep(1989, 21), 
                            rep(1, 21)), stringsAsFactors = FALSE)
colnames(start) = colnames(growth)
start$Year = as.integer(start$Year)
start$CumGrowth = as.numeric(start$CumGrowth)
growth = rbind(start, growth)

#' Sort for 10 best performing countries
growth %>% 
  filter(Year == 2014) %>% 
  arrange(desc(CumGrowth)) %>% 
  select(CountryName) %>% 
  slice(1:10) -> top5Cont

top5 = left_join(top5Cont, growth, "CountryName")

#' Sort for 10 worst performing countries
growth %>% 
  filter(Year == 2014) %>% 
  arrange(CumGrowth) %>% 
  select(CountryName) %>% 
  slice(1:10) -> bot5Cont

bot5 = left_join(bot5Cont, growth, "CountryName")

# Plotting top 10 countries
print(ggplot(data=top5, 
             aes(x=Year, y=(CumGrowth - 1) * 100, 
                 group=CountryName, colour=CountryName)) + 
      geom_line(size = 1.2) + 
      ggtitle("Top 10 Performing S&P Global Equity Indices") + 
      ylab("Cummulative Percentage Growth"))

# Plotting worst 10 countries
print(ggplot(data=bot5, 
      aes(x=Year, y=(CumGrowth - 1) * 100, 
          group=CountryName, colour=CountryName)) + 
      geom_line(size = 1.2) + 
      ggtitle("10 Worst Performing S&P Global Equity Indices") + 
      ylab("Cummulative Percentage Growth"))