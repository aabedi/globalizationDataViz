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