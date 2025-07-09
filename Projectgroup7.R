###########################################
#1.BAR GRAPH
library(ggplot2)
library(dplyr)
library(leaflet)

df<-read.csv("C:/Users/Bittucharan/Downloads/Affordable_Rental_Housing_Developments.csv")
df<-df[-1, ]

df$Property.Type<-factor(df$Property.Type)

df_counts<-df %>%group_by(Property.Type)%>%summarise(Count=n())%>%arrange(desc(Count))

df$Property.Type<-factor(df$Property.Type,levels=df_counts$Property.Type)

df1<-df%>%group_by(Property.Type)%>%summarise(Count=n())%>%mutate(Percentage=(Count/sum(Count))*100)

ggplot(df1,aes(x=Property.Type, y=Count))+
  geom_bar(fill="purple",stat="identity")+
  geom_text(aes(label=paste0(round(Percentage),"%"),y=Percentage+2),vjust=0,size=3,color="black",hjust=-24)+
  labs(title="Bar chart of Property Type",x="Property Type",y="Count")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  coord_flip()
########################################
#2.HISTOGRAM
library(ggplot2)

names(df)<-make.names(names(df))

ggplot(df,aes(x=Community.Area.Number))+
  geom_histogram(fill="skyblue",color="black",bins=20)+
  labs(title="Histogram of Community Area Number",x="Community Area Number",y="Frequency")+
  scale_x_continuous(breaks = seq(0, max(df$Community.Area.Number),by=10),limits=c(0,max(df$Community.Area.Number)))
#######################################
#3.GEOSPATIAL
leaflet_map<-leaflet(df)%>%
  addTiles() %>%
  addMarkers(lng=~Longitude,lat=~Latitude,popup=~Property.Type)
print(leaflet_map)
#######################################
#4.SCATTER PLOT
library(ggplot2)
names(df)<-make.names(names(df))
ggplot(data=df,aes(x=Longitude,y=Latitude))+
  geom_point()+ 
  labs(title="Scatter Plot of Latitude vs. Longitude",x="Longitude",y="Latitude")
######################################
#5.HEAT MAP
library(ggplot2)
heatmap<-df%>%group_by(Property.Type,Community.Area.Name)%>%summarise(Count=n())%>%
  ungroup()%>%mutate(Property.Type=factor(Property.Type),
         Community.Area.Name=factor(Community.Area.Name))

ggplot(heatmap,aes(x=Community.Area.Name,y=Property.Type,fill=Count))+
  geom_tile(color="#D2B48C")+
  scale_fill_gradient(low="#D2B48C",high="brown")+
  labs(title="Affordable Rental Housing Developments",x="Community Area",
       y="Property Type",
       fill="Count")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1))