#prepare the libraries, although I forgot which actually needed and ones that don't
#because when I re-ran the script, some libraries can't be installed
#despite having all function working. hmm. 

library(sp)
library(raster)
library(maptools)
library(mapproj)
library(ggmap)
library(plyr)
library(ggplot2)
library(rgdal)
library(ggmap)
library(scales)
library(ggrepel)
library(DeducerSpatial)


#get the indonesia map data (i'm sorry spatial analysis people
#i don't know the correct term of the data type
#please do let me know)
#at the level of province (1)

indo <-  getData('GADM', country = "IDN", level =1 )
#you don't have to plot it, but you can
#it'll just take a while to plot
indo_UTM <- indo

#check province names in the dataset
indo_UTM@data$NAME_1

#set as new variable
NAME_1<-indo_UTM@data$NAME_1
#set new variable with proportion data per province
#in this case, data from BPS (Indonesian Central Agency on Statistics)
#on proportion of people age >10 who read fictional books ("buku cerita") in 2012
#per province.
#use the order of provinces in "NAME_1"

prop.read<-c(6.56,
             4.26,
             3.58,
             5.22,
             5.51,
             4.51,
             6.49,
             6.12,
             4.59,
             4.39,
             4.52,
             5.57,
             5.54,
             5.83,
             6.77,
             9.16,
             4.39,
             5.39,
             5.12,
             6,
             6.26,
             4.14,
             6.27,
             6.6,
             5.43,
             6.31,
             3.61,
             5.4,
             4.11,
             6.27,
             4.55,
             3.85,
             6.97
             
)

#make new dataframe connecting NAME_1 and prop.read
count_df<-data.frame(NAME_1, prop.read)
indo_UTM@data$NAME_1
indo_UTM@data$id <- rownames(indo_UTM@data)
indo_UTM@data <- join(indo_UTM@data,count_df,by="NAME_1")
head(indo_UTM@data)
indo_df <- fortify(indo_UTM)
head(indo_df)
indo_df <- join(indo_df,indo_UTM@data,by="id")

options(scipen=999)

#add several coordinate points of the highest,
#mean, and lowest number of a retail bookshops in the provinces
#(using data from wikipedia of the retail bookshop)
#https://id.wikipedia.org/wiki/Gramedia_(toko_buku)

setwd("D:\\Personal\\github")
ds<-read.csv("map_practice1.csv")
summary(ds$retail_book_shop)
#minimum = 0
#mean = 5.2
#max = 34

#check which provinces are these
View(ds)
#min = Bengkulu, Gorontalo
#mean = Sulawesi Selatan, Kalimantan Timur
#max = Jakarta

#find coordinates of each of these
#I usually use google earth


lat_y <- c(-3.792862,
           0.699910,
           -3.666615,
           0.538630,
           -6.208763
       )

long_x <- c(102.261012,
            122.446601,
            119.975292,
            116.419413,
            106.845599
      )

coords_names <- c("Bengkulu (0)",
                  "Gorontalo (0)",
                  "Sulsel (4)",
                  "Kaltim (4)",
                  "Jakarta (34)"
)

groups <-c("low",
          "low",
          "med",
          "med",
          "high")

#make the coordinate of interes dataset
df.xy = data.frame(lat_y,long_x,coords_names, groups)

#let's plot the map

ggplot() +
  geom_polygon(data = indo_df, aes(x = long, y = lat, group = group, 
                                   fill = prop.read), color = "grey", size = 0.25) +
  coord_fixed(1.1) + #i spent so much time early this year to fix my map projection
                     #so happy i found this technique
                     #thank you R community
  scale_fill_distiller(name="", palette = "PuRd", #let's use pink because this is not for work! yay! (boss likes greyscale)
                       trans="reverse",
                       breaks = pretty_breaks(n = 5))+
  labs(title="Proportion of people age >10 who read fictional books in 2012
and provinces with high/med/low number of retail bookshops")+
  xlab("") +
  ylab("")+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.text=element_text(size=14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14)
  )+ #now adding the second dataset for points
  geom_point(data=df.xy,size=3, aes(long_x,lat_y,colour=group))+
  geom_label_repel(aes(long_x,lat_y,label=coords_names))

#interesting, Kaltim that has one of the highest proportion
#only have med level of bookshops
#while Jakarta, province with one of the highest number of bookshops
#have one of the lowest proportion of people who read fictional books

#just out of curiousity, does proportion of people who reads
#and number of retail book shop is correlated?
#let's use my new fav, Indrajeet Patil's package

library(vtable)
library(ggstatsplot)

vtable(ds)

ggscatterstats(
  data = ds,
  x = fiction_book,
  y = retail_book_shop,
  xlab = "Proportion of people who reads fictional book",
  ylab = "Number of retail book shop",
  title = "Does number of retail book shop correlated with number of people who reads in Indonesia?",
  messages = FALSE
)

#noopee. that's interesting. hmm. 