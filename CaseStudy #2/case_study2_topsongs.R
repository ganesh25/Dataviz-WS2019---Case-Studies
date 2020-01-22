library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(formattable)
library(wordcloud)
library(RWeka)
library(qdap)
library(tm)
library(readxl)


data_featuresdf_2 <- read_excel("~/Downloads/featuresdf-2.xls")
 

### to check the top 10 famous songs of 2017 ####

top10_music<- data_featuresdf_2 %>%
  arrange(desc(artists)) %>%
  slice(1:10) 

head(top10_music)

ggplot(top10_music,aes(x=artists,y=name))+geom_bar(stat = "identity",  fill = "tomato2", width = 0.6 ) 

#### similarly we shall check top 20 songs of 2017

top20_music<- data_featuresdf_2 %>%
  arrange(desc(artists)) %>%
  slice(1:20) 

ggplot(top20_music,aes(x=artists,y=name))+geom_bar(stat = "identity",  fill = "tomato2", width = 0.6 ) 

##### Let's determine the artists who have more than one song on the Top 100 Songs List  
top_artists <- data_featuresdf_2 %>%
  group_by(artists)  %>%
  summarise(n_apperance = n()) %>%
  filter(n_apperance > 1) %>%
  arrange(n_apperance)

top_artists$artists <- factor(top_artists$artists, levels = top_artists$artists[order(top_artists$n_apperance)]) # in order to visualise the list in descending order 

ggplot(top_artists, aes(x = artists, y = n_apperance)) +
  geom_bar(stat = "identity",  fill = "tomato2", width = 0.6 ) + 
  labs(title = "Top Artists of 2017", x = "Artists", y = "Number of Apperance on the Top 100") +
  theme(plot.title = element_text(size=15,hjust=-.3,face = "bold"), axis.title = element_text(size=12)) +
  geom_text(aes(label=n_apperance), hjust = 2, size = 3, color = 'white') +
  coord_flip()


###correlation matrix between loudiness, liveliness, energy, sound for the top songs of 2017
###In order to understand the correlation between variables, we use `corrplot` function, which is one of the data visualization functions.

library(corrplot)
data_featuresdf_2 <- read_excel("~/Downloads/featuresdf-2.xls")
sapply(data_featuresdf_2, class)
sapply(data_featuresdf_2, is.character)
data_featuresdf_2_num<- as.data.frame(apply(data_featuresdf_2, 2, as.numeric))
data_songs_num <- data_featuresdf_2_num[,-(1:3)]
mtCor <- cor(data_songs_num)
corrplot(mtCor, method = "ellipse", type = "upper", tl.srt = 45)

###from the result we say that there is positive correlation between energy and loudness of the songs

### The Chainsmokers

###Chainsmoker's songs emphasiszed in energy and danceability. we can see the level of energy and dancebility for the chainsmoker songs by the following plot.
tsongs <- filter(data_featuresdf_2, artists %in% c("The Chainsmokers"))
tsong21<-  tsongs[, c(2,4,5,9,10,12,13)] 
ggplot(tsongs, aes(x = energy, y = danceability, color='artists' )) +    
  geom_bar(stat = "identity",  fill = "tomato2", width = 0.6 )
                                                                        
### to check the liveness and the energy for all the songs of the top10_music of 2017
ggplot(top10_music, aes(x = liveness, y = danceability, color = 'artists' )) +
  geom_bar(stat = "identity",  fill = "tomato2", width = 0.6 ) 



### The key is the track is in. Integers map to pitches using standard Pitch Class notation.
### we are observing which artists have higher key among the top20 songs.
### generally the class notation start from 0 till 11 
count_of_the_top_10_songs<- top10_music%>%
  select(key, artists) %>%
  group_by(artists) %>%
  summarise(key = n())

ggplot(count_of_the_top_10_songs, aes(x = artists, y = key,)) +
  geom_bar(stat = "identity",  fill = "tomato2", width = 0.6 ) 

###the total count of the songs by each artists for the year 2017
count_of_the_songs_of_each_artists <- data_featuresdf_2 %>%
  select(name, artists) %>%
  group_by(artists) %>%
  summarise(name = n())

ggplot(count_of_the_songs_of_each_artists, aes(x = artists, y = name, color = 'name' )) +
  geom_bar(stat = "identity",  fill = "tomato2", width = 0.6 ) 


### let us look at the duration of the top 20 songs for 2017
### step convert the duration_ms to minutes 

#### ????????????????????????? we need to work on it!!!!
top20_music_duration <-top20_music %>% 
  select(name,duration_ms) %>% 
  arrange(desc(name)) %>%
  mutate(duration = (c(duration_ms/10000)), avg_duration = mean(duration_ms/1000))

ggplot(top20_music_duration, aes(x = artists, y = duration_ms)) +
  geom_bar(stat = "identity",  fill = "tomato2", width = 0.6 ) + 
  labs(title = "Top Artists of 2017", x = "Artists", y = "Duration of the song") +
  theme(plot.title = element_text(size=15,hjust=-.3,face = "bold"), axis.title = element_text(size=12)) 




### lest us check the tempo of top 10 songs 
### the overall tempo of the songs 
### tempo is the speed or pace of a given piece and derives directly from the average beat duration. 
### from the plot we see that the 


top10_tempo_time <- top10_music %>% 
  select(name,artists,tempo,time_signature) %>% 
  arrange((name)) 

ggplot(top10_tempo_time, aes(name)) +
  geom_line(aes(y=tempo, group=1)) +
  xlab("Song Track") + ylab("Tempo") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


