# Clustering and Replacing Tribe Players
# John Kearns
# 04/10/2021

# load packages
library(tidyverse)
library(readr)
source("~/Functions/Mode1.R")
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(NbClust)
library(kableExtra)

# set wd
setwd("~/2021/NBN/Clustering Project")

# bring in players in transfer portal
conferences <- c("ACC", "Big-12", "Big-Ten", "Pac-12", "SEC", "AAC",
                 "C-USA", "MAC", "M-West", "A-10", "A-East", "A-Sun",
                 "Big-East", "Big-west", "Big-Sky", "B-South", "CAA",
                 "Horizon", "Ivy", "MAAC", "MEAC", "MVC", "NEC", "OVC",
                 "Patriot", "SBC", "SLC", "Southern", "Summit", "SW-AC",
                 "WAC", "WCC")

year <- 2021

# transfer portal information from https://247sports.com/Season/2021-Basketball/TransferPortal/?status=uncommitted\
# accessed 4/10/2021
transfer_portal <- read_csv("transfer_portal.txt")[-c(1:4,1363:1525),] %>% 
  filter(!(Guard%in%c("Center","Forward","Pos","Prediction","Eligible","Transfer"))) # remove rows that do not contain player information

index = c(0,which(transfer_portal$Guard=="?")) # get the indentifier for when the information for specific player ends
transfers = data.frame()
for(i in 2:length(index)){
  player = transfer_portal$Guard[(index[i-1]+1):index[i]]
  if(length(player)==7){
    player = c(player[1:5],"Immediate",player[6:7]) # need to have same number of columns for all players
  }
  if(length(player)==6){
    player = c(player[1:5],"Immediate",NA,"?") # need to have same number of columns for all players
  }
  if(length(player)==9){ 
    next # dont want players that are basically committed
  }
  transfers=rbind(transfers,player)
}
transfers = transfers[,-2]
colnames(transfers)=c("name","height_weight","rating","position","eligibility","school","destination")

transfers = transfers %>% 
  rowwise() %>% 
  mutate(height=str_split(height_weight," / ")[[1]][1],
         weight=str_split(height_weight," / ")[[1]][2],
         rating=as.numeric(str_split(rating," ")[[1]][1]),
         position_group=substr(position,nchar(position),nchar(position))) %>% 
  select(-height_weight)

# get player data from Bart Torvik
player_data_2020 = read.csv("player_data_2020.csv",header = FALSE)[,-66]
player_data_2021 = read.csv("player_data_2021.csv",header = FALSE)[,-66]
header = read.csv("header.csv",header=FALSE)

colnames(player_data_2020) = header$V2
colnames(player_data_2021) = header$V2

player_data_2020 = player_data_2020 %>% 
  mutate(ht=gsub("/","-",ht))
player_data_2021 = player_data_2021 %>% 
  mutate(ht=gsub("/","-",ht))

# harmonize schools between 247 Sports and BT
old = setdiff(transfers$school,player_data_2021$team)[-12]
new = c("Mississippi St.","Arizona St.","Iowa St.","Kansas St.","South Florida","Penn St.","Columbia","UT Arlington","Miami FL","Appalachian St.","Dartmouth","Washington St.","Ohio St.","Mississippi","Oregon St.","Indiana St.","Georgia St.","North Carolina St.","Princeton","Cornell","Brown","Harvard","Tennessee Martin","Louisiana Lafayette")
transfers$school = str_replace_all(transfers$school,setNames(new,old))

# how many players in transfer portal are not in player data
setdiff(transfers$name,player_data_2021$player_name)

# try and fuzzy match the names
transfers$match = NA
for(i in 1:length(transfers$name)){
  match = agrep(transfers$name[i],player_data_2021$player_name[player_data_2021$team==transfers$school[i]],value=TRUE,ignore.case=FALSE,max.distance=.2)
  if(length(match)>0){
    transfers$name[i] = match
    transfers$match[i] = 1
  }
}

for(i in which(is.na(transfers$match))){
  match = agrep(transfers$name[i],player_data_2020$player_name[player_data_2020$team==transfers$school[i]],value=TRUE,ignore.case=FALSE,max.distance=.2)
  if(length(match)>0){
    transfers$name[i] = match
    transfers$match[i] = 1
  }
}

transfers$name[which(transfers$name=="Myles Warren")] = "Myles Fitzgerald-Warren"
transfers$match[which(transfers$name=="Myles Fitzgerald-Warren")] = 1

transfers$name[which(transfers$name=="Roger \"RJ\" Wilson Jr")] = "R.J. Wilson"
transfers$match[which(transfers$name=="R.J. Wilson")] = 1

# get two season average of players
player_data_2021$num = as.numeric(player_data_2021$num)
player_data = bind_rows(player_data_2020,player_data_2021)

old = unique(player_data$position)[-9]
new = c("PF","C","CG","SG","SF","PF","PG","PG")
player_data$position_simple = str_replace_all(player_data$position,setNames(new,old))
player_data = player_data %>% 
  rowwise() %>% 
  mutate(position_group=substr(position_simple,nchar(position_simple),nchar(position_simple)))

# get averages for all players
player_data_avg = player_data %>% 
  select(player_name,team,ORtg,eFG,TS_per,ORB_per,DRB_per,AST_per,TO_per,FT_per,twoP_per,TP_per,TPA,
         blk_per,stl_per,ftr,ht,porpag,adjoe,`Rec Rank`,` ast/tov`,` rimmade/(rimmade+rimmiss)`,
         ` midmade/(midmade+midmiss)`,` dunksmade/(dunksmade+dunksmiss)`,` pick`,
         pid,` drtg`,adrtg,` dporpag`,` stops`,` bpm`,` obpm`,` dbpm`,` dunksmiss+dunksmade`,` rimmade+rimmiss`,` midmade+midmiss`,
         ` obpm`,` dbpm`,` gbpm`,position,position_simple,position_group,Min_per) %>% 
  group_by(player_name,team) %>% 
  summarize(
    player_name=Mode1(player_name,na.rm=TRUE),
    team=Mode1(team,na.rm=TRUE),
    twoP_per = mean(twoP_per,na.rm=TRUE),
    ORB_per=mean(ORB_per,na.rm=TRUE),
    DRB_per=mean(DRB_per,na.rm=TRUE),
    AST_per=mean(AST_per,na.rm=TRUE),
    TO_per=mean(TO_per,na.rm=TRUE),
    FT_per=mean(FT_per,na.rm=TRUE),
    TP_per=mean(TP_per,na.rm=TRUE),
    blk_per=mean(blk_per,na.rm=TRUE),
    stl_per=mean(stl_per,na.rm=TRUE),
    ftr=mean(ftr,na.rm=TRUE),
    ht=Mode1(ht,na.rm=TRUE),
    ` rimmade+rimmiss`=mean(` rimmade+rimmiss`/Min_per,na.rm=TRUE),
    ` midmade+midmiss`=mean(` midmade+midmiss`/Min_per,na.rm=TRUE),
     ` dunksmiss+dunksmade`=mean(` dunksmiss+dunksmade`/Min_per,na.rm=TRUE),
    position=Mode1(position,na.rm=TRUE),
    position_simple=Mode1(position_simple,na.rm=TRUE),
    position_group=Mode1(position_group,na.rm=TRUE)
  )

# replace nan with 0
for(i in 1:ncol(player_data_avg)){
  player_data_avg[,i] = ifelse(is.nan(unlist(player_data_avg[,i]))|is.infinite(unlist(player_data_avg[,i])),0,unlist(player_data_avg[,i]))
}

player_data_avg = player_data_avg %>% 
  rowwise() %>% 
  mutate(ht=(12*as.numeric(str_split(ht,"-")[[1]][1]))+as.numeric(str_split(ht,"-")[[1]][2]))


# scale variables to remove units
cluster_data_all = player_data_avg 

cluster_index = cluster_data_all[,c("player_name","team","ht","position","position_simple","position_group")] 
cluster_data = cluster_data_all[,-which(colnames(cluster_data_all)%in%c("player_name","team","ht","position","position_simple","position_group"))]
row.names(cluster_data) = paste0(cluster_index$player_name,"_",cluster_index$team)

cluster_data = scale(cluster_data)

# choosing distance method
distance <- get_dist(cluster_data)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

cluster_sample = cluster_data[sample(1:nrow(cluster_data),1000,replace = F),]
# what is best number of clusters to use?
fviz_nbclust(cluster_sample, pam, method = "wss")
# suggests around 6 or 7

fviz_nbclust(cluster_data, pam, method = "silhouette")
# suggests 3

fviz_nbclust(cluster_data, kmeans,nstart=25, method = "gap_stat",nboot=500)
# suggests 3


nbclust_out <- NbClust(
  data = cluster_data,
  distance = "euclidean",
  min.nc = 5, # minimum number of clusters
  max.nc = 20, # maximum number of clusters
  method = "kmeans" # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"
)

# create a dataframe of the optimal number of clusters
nbclust_plot <- data.frame(clusters = nbclust_out$Best.nc[1, ])
# select only indices which select between 2 and 5 clusters
nbclust_plot <- subset(nbclust_plot, clusters >= 2 & clusters <= 20)

# wide suggestions, so will just go with 7 (looks to be the best fit)

# create plot
ggplot(nbclust_plot) +
  aes(x = clusters) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Number of clusters", y = "Frequency among all indices", title = "Optimal number of clusters") +
  theme_minimal()

# run kmeans clustering
pam7 <- pam(cluster_data[-2719,],7)

# evaluate cluster
(BSS <- pam7$betweenss)
(TSS <- pam7$totss)
BSS / TSS * 100

sil <- silhouette(pam7$cluster, dist(cluster_data))
fviz_silhouette(sil)

# matching william and mary players with transfers
cluster_info <- data.frame(cluster_data_all[-2719,],
                           cluster = as.factor(pam7$cluster)
)

row.names(cluster_info) = NULL
cluster_info %>% filter(team=="William & Mary") %>% select(player_name,cluster) %>%  arrange(cluster) %>%
  kbl(col.names = c("Player","Cluster")) %>%
  kable_paper(full_width = F)

transfers = transfers %>% 
  mutate(key=paste0(name,"_",school))

cluster_info1 = left_join(cluster_info %>% 
                            mutate(key=paste0(player_name,"_",team)),
                          transfers %>% select(key,rating),by="key")

recruits = cluster_info1 %>% 
  filter(key %in% transfers$key)

fviz_cluster(pam7, data = cluster_data[-2719,],
             geom = "point",
             ellipse.type = "convex"
)
