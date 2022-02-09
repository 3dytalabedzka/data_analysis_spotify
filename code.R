pdf.options(encoding='CP1250')
library(xtable)
library(ggplot2)
library(corrplot) 
library(stringr)
library(plyr)
library(nortest)
library(lmtest)
library(data.table)

data <- read.csv("Spotify-2000.csv")
n <- length(data$Year)

set.seed(123)                         # ustawienie ziarna dla tych samych wyników
data$Year[sample(1:n,0.01 * n)] <- NA # dodanie ok 1% braków danych
data <- data[,-c(1,14)]               # usunięcie kolumny indeksów i speechiness

colnames(data)[c(3,5,8,11)] <- c("Genre", "BPM", "Loudness","Duration")
names <- c(colnames(data[,8:13]), "")
t <- rbind(summary(data[,1:7]), names)
t <- rbind(t, cbind(summary(data[,8:13]), NA))
tab <- xtable(t, caption = "Podsumowanie danych", label = "tab:summary1")
print(tab, scalebox = 0.61, include.rownames = FALSE, hline.after= c(-1:0, 7:8, 14))

data$Duration <- str_replace(data$Duration, ",", "")
class(data$Duration) <- "integer"

missing <- c(2008,2001,2010,2011,2013,2014,1970,2017,1973,1976,1977,1979,1968,1984,1985,1987,1993,1998,2012)
missing[1:10]
missing[11:19]

data[is.na(data$Year),4] <- missing

intervals <- c("[1950,1960)","[1960,1970)","[1970,1980)","[1980,1990)","[1990,2000)","[2000,2010)","[2010,2020)")
intervals[1:4]
intervals[5:7]
year0 = 1940
for (i in 1:7){
  data$Year[data$Year >= year0+i*10 & data$Year < year0 + (i+1)*10] <- intervals[i]
}

names <- c(colnames(data[,8:13]), "")
t <- rbind(summary(data[,1:7]), names)
t <- rbind(t, cbind(summary(data[,8:13]), NA))
tab <- xtable(t, caption = "Podsumowanie danych po zmianach", label = "tab:summary2")
print(tab, scalebox = 0.61, include.rownames = FALSE, hline.after= c(-1:0, 6:7, 13))

corrplot(cor(data[,-c(1,2,3,4)]),method = "number")
corrplot(cor(data[,-c(1,2,3,4)]),method = "number",tl.col = "black", col=colorRampPalette(c("#D82C20","white","#026645"))(200), number.cex=2, tl.cex=2, cl.cex=1.8)

ggplot(data, aes(x=Valence, y=Danceability)) + geom_point(colour="#D82C20", size=0.3) + theme(text = element_text(size=10), axis.text.y = element_text(size=10), axis.text.x = element_text(size=10))

ggplot(data, aes(x=BPM, y=Danceability)) + geom_point(colour="#D82C20", size=0.3) + theme(text = element_text(size=10), axis.text.y = element_text(size=10), axis.text.x = element_text(size=10))

ggplot(data, aes(x=Loudness, y=Energy)) + geom_point(colour="#EC5A96", size=0.3) + theme(text = element_text(size=10), axis.text.y = element_text(size=10), axis.text.x = element_text(size=10))

ggplot(data, aes(x=Acousticness, y=Energy)) + geom_point(colour="#EC5A96", size=0.3) + theme(text = element_text(size=10), axis.text.y = element_text(size=10), axis.text.x = element_text(size=10))

lmFit <- lm(data$Danceability~data$Valence)
residuals <- lmFit$residuals
ad.test(residuals)
dwtest(lmFit)

genre_count <- count(data,"Genre")
genre_count[order(genre_count$freq,decreasing=T),][c(1:5,144:149),]

genres <- c("rock", "pop", "rap", "blues","dance", "country", "metal", "hip hop", "indie", "latin", "jazz","soul","mellow")

genres[1:6]
genres[7:12]
genres[13]

data2 <- copy(data)

# duplikowanie i kategoryzowanie gatunków
for (gen in genres){
  if (sum(duplicated(data2[,-3])) == 0){
    dupl <- data2[grepl(gen,data2$Genre) & grepl(paste0(genres[-match(gen,genres)],collapse="|"),data2$Genre),]
    data2$Genre[grepl(gen,data2$Genre)] <- gen
    data2 <- rbind(data2,dupl)
  } else{
    data2$Genre[grepl(gen,data2$Genre)] <- gen
  }
}

data2$Genre[data2$Genre == "hip hop"] <- "rap"
data2$Genre[data2$Genre %in% c("electro house","electronica")] <- "dance"

rare_genres <- count(data2,"Genre")[count(data2,"Genre")$freq < 10,"Genre"]
data2$Genre[data2$Genre %in% rare_genres] <- "other"

count(data2,"Genre")[order(count(data2,"Genre")$freq,decreasing=T),]

mean_danceability <- sapply(unique(data2$Genre), function(x) {mean(data2$Danceability[data2$Genre == x])})
mean_danceability[order(mean_danceability,decreasing=T)][1:5]

mean_valence <- sapply(unique(data2$Genre), function(x) {mean(data2$Valence[data2$Genre == x])})
mean_valence[order(mean_valence,decreasing=T)][1:5]

mean_energy <- sapply(unique(data2$Genre), function(x) {mean(data2$Energy[data2$Genre == x])})
mean_energy[order(mean_energy,decreasing=T)][1:5]

mean_popularity <- sapply(unique(data2$Genre), function(x) {mean(data2$Popularity[data2$Genre == x])})
mean_popularity[order(mean_popularity,decreasing=T)][1:5]

ggplot(data=data2[data2$Genre %in% c("rap","disco","big beat","funk","mellow"),], aes(x=Genre,y=Danceability)) + geom_boxplot(alpha=0.2)

ggplot(data=data2[data2$Genre %in% c("rap","disco","blues","country","british invasion"),], aes(x=Genre,y=Valence)) + geom_boxplot(alpha=0.2)

ggplot(data=data2[data2$Genre %in% c("big beat","metal","disco","dance", "rap"),], aes(x=Genre,y=Energy)) + geom_boxplot( alpha=0.2)

ggplot(data=data2[data2$Genre %in% c("mellow", "british invasion", "permanent wave", "soul", "metal"),], aes(x=Genre,y=Popularity)) + geom_boxplot( alpha=0.2)

ggplot(data, aes(x="", y=Year, fill=Year)) + geom_bar(width=1, stat="identity") + coord_polar("y", start=0) + theme_void() + scale_fill_manual(values = c("#F9C6C5", "#EC5A96", "#B73A3A", "#5C0001","#AF916D", "#77A950" ,"#026645"))

m_dan <- sapply(intervals[-1], function(x) {mean(data$Danceability[data$Year == x])} )
m_dan <- rbind(m_dan, sapply(intervals[-1], function(x) {median(data$Danceability[data$Year == x])} ) )
m_dan <- rbind(m_dan, sapply(intervals[-1], function(x) {var(data$Danceability[data$Year == x])} ) )
row.names(m_dan) <- c("Średnia", "Mediana", "Wariancja")
m_tab_d <- xtable(m_dan, caption = "Średnie, mediany i wariancje taneczności utworów dla dekad", label = "tab:mean1")
print(m_tab_d, scalebox = 0.8)

m_val <- sapply(intervals[-1], function(x) {mean(data$Valence[data$Year == x])} )
m_val <- rbind(m_val, sapply(intervals[-1], function(x) {median(data$Valence[data$Year == x])} ) )
m_val <- rbind(m_val, sapply(intervals[-1], function(x) {var(data$Valence[data$Year == x])} ) )
row.names(m_val) <- c("Średnia", "Mediana","Wariancja")
m_tab_v <- xtable(m_val, caption = "Średnie, mediany i wariancje pozytywności utworów dla dekad", label = "tab:mean2")
print(m_tab_v, scalebox = 0.8)

m_ene <- sapply(intervals[-1], function(x) {mean(data$Energy[data$Year == x])} )
m_ene <- rbind(m_ene, sapply(intervals[-1], function(x) {median(data$Energy[data$Year == x])} ) )
m_ene <- rbind(m_ene, sapply(intervals[-1], function(x) {var(data$Energy[data$Year == x])} ) )
row.names(m_ene) <- c("Średnia", "Mediana","Wariancja")
m_tab_e <- xtable(m_ene, caption = "Średnie, mediany i wariancje energiczności utworów dla dekad", label = "tab:mean3")
print(m_tab_e, scalebox = 0.8)

m_pop <- sapply(intervals[-1], function(x) {mean(data$Popularity[data$Year == x])} )
m_pop <- rbind(m_pop, sapply(intervals[-1], function(x) {median(data$Popularity[data$Year == x])} ) )
m_pop <- rbind(m_pop, sapply(intervals[-1], function(x) {var(data$Popularity[data$Year == x])} ) )
row.names(m_pop) <- c("Średnia", "Mediana","Wariancja")
m_tab_p <- xtable(m_pop, caption = "Średnie, mediany i wariancje popularności utworów dla dekad", label = "tab:mean4")
print(m_tab_p, scalebox = 0.8)

ordered_artists <- count(data,"Artist")[order(count(data,"Artist")$freq,decreasing=T),]
ordered_artists <- ordered_artists[ordered_artists$freq > 10,]
popular_artist <- ordered_artists$Artist

data_popular <- data[data$Artist %in% popular_artist,]

mean_danceability <- sapply(popular_artist, function(x) {mean(data_popular$Danceability[data_popular$Artist == x])})
mean_danceability[order(mean_danceability,decreasing=T)][1:5]

mean_valence <- sapply(popular_artist, function(x) {mean(data_popular$Valence[data_popular$Artist == x])})
mean_valence[order(mean_valence,decreasing=T)][1:5]

mean_energy <- sapply(popular_artist, function(x) {mean(data_popular$Energy[data_popular$Artist == x])})
mean_energy[order(mean_energy,decreasing=T)][1:5]

mean_popularity <- sapply(popular_artist, function(x) {mean(data_popular$Popularity[data_popular$Artist == x])})
mean_popularity[order(mean_popularity,decreasing=T)][1:5]

ggplot(data=data[data$Artist %in% c("Michael Jackson","Creedence Clearwater Revival","Ed Sheeran", "Prince", "De Dijk"),], aes(x=Artist,y=Danceability)) + geom_boxplot(alpha=0.2)

ggplot(data=data[data$Artist %in% c("Creedence Clearwater Revival","ABBA", "Bee Gees", "Michael Jackson", "The Rolling Stones"),], aes(x=Artist,y=Valence)) + geom_boxplot(alpha=0.2)

ggplot(data=data[data$Artist %in% c("Metallica","Red Hot Chili Peppers","Muse", "Golden Earring", "Nirvana"),], aes(x=Artist,y=Energy)) + geom_boxplot(alpha=0.2)

ggplot(data=data[data$Artist %in% c("Ed Sheeran", "Red Hot Chili Peppers","Coldplay", "Adele", "Elton John"),], aes(x=Artist,y=Popularity)) + geom_boxplot(alpha=0.2)

artist_count <- count(data,"Artist")
artist_count[order(artist_count$freq,decreasing=T),][c(1:10),]