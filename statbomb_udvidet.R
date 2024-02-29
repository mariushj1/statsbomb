library(tidyverse)
library(ggsoccer)
library(StatsBombR)
library(shiny)
library(shinythemes)
# Sourcer funktioner til scriptet
source("util.R")

# Indhenter data
FComp <- FreeCompetitions() %>%
  filter(competition_id %in% c(72))

# Indhenter matches
Matches <- FreeMatches(FComp)

# Kvindernes VM 2023, competitionid=107
VMMatches=Matches %>% filter(season.season_id==107)

StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T)

StatsBombData = allclean(StatsBombData)


# find en kamp
dkmatch <- StatsBombData %>% filter(match_id==3893795)
dkmshot <- dkmatch %>% filter(type.name=="Shot")

# angle and dist to goal
dkmshotsel <- dkmshot[,c(74,77,25,26,21,22,10,150,151,156,157)]
dkmshotsel <- dkmshotsel %>% rowwise() %>% mutate(angle=mangle(unlist(location)))
dkmshotsel <- dkmshotsel %>% rowwise() %>% mutate(dist=disttogoal(unlist(location)))
dkpshotsel <- dkmshotsel %>% filter(team.id==853)
chpshotsel <- dkmshotsel %>% filter(team.id==1207)

# Angiver valg af skud og spiller
skud <- 1
player <- "Kathrine Møller Kühl"
input <- TRUE
hold <- "Denmark Women's"

# Laver en ny dataframe kun med valgte spiller
spillerSkud <- dkmshotsel %>% 
  filter(player.name == player)

# Laver ny dataframe til freeze frame
testshot <- spillerSkud[skud,]
testff <- testshot$shot.freeze_frame[[1]]
testff <- testff %>% rowwise() %>% mutate(x=location[1],
                                          y=location[2])
mulig_spilning <- testff %>% 
  filter(teammate == "TRUE")

# Laver trekanter for alle medspillere og gemmer i liste
tri_4_shot <- list()
for (i in 1:nrow(mulig_spilning)) {
  temp_df <- dfForSingleShotTri(unlist(mulig_spilning$location[i]))
  tri_4_shot[[i]] <- temp_df
}

# Gemmer modstandsholdets navn
modstander <- as.character(unique(dkmshotsel[dkmshotsel$team.name != hold,"team.name"]))

Spiller_lokation <- mulig_spilning[,c(1,4)]
Spiller_lokation <- rbind(Spiller_lokation,testshot[,c(7,4)])

# Laver tom dataframe
df_itri <- data.frame()
for (i in 1:nrow(Spiller_lokation)) {
  
  # Define the vertices of the triangle (example vertices)
  temp_df <- unlist(Spiller_lokation$location[i])
  triangle_vertices <- matrix(c(temp_df[1], temp_df[2], 120, 36, 120, 44), ncol = 2, byrow = TRUE)

  # Apply the function to each point in the dataframe
  inside_triangle <- apply(testff[,7:8], 1, function(point) point_inside_triangle(point, triangle_vertices))

  # Count the number of points inside the triangle
  num_points_inside <- sum(inside_triangle)

  tempdf1 <- data.frame("player.name"=Spiller_lokation$player.name[i],"Spiller i tri"=num_points_inside)

  df_itri <- rbind(df_itri,tempdf1)

}

# Laver ny dataframe kun med spillere med samme eller færre personer i deres trekant
df_itri_te <- df_itri %>% 
  filter(Spiller.i.tri <= as.numeric(df_itri[df_itri$player.name == "Kathrine Møller Kühl",][2]))

df_itri_te1 <- merge(df_itri_te,Spiller_lokation, by = "player.name")

# Beregner distancen fra spillerne til målet, og gemmer i dataframe
df_itri_te1$dist <- NA
for (i in 1:nrow(df_itri_te1)) {
  templist <- unlist(df_itri_te1$location[i])
  df_itri_te1$dist[i] <- sqrt((120-templist[1])^2+(40-templist[2])^2)
  
}

# Gemmer afstanden for alle
df_itri_te2 <- df_itri_te1 %>% 
  filter(dist <= as.numeric(df_itri_te1[df_itri_te1$player.name == "Kathrine Møller Kühl",][4]),
         player.name != "Kathrine Møller Kühl")

# Gemmer alle modstandere
modstander1 <- testff %>% 
  filter(teammate == FALSE) %>% 
  select(player.name,location)

# Beregner modstanden fra vores spiller til alle modstandere 
for (i in 1:nrow(df_itri_te2)) {
  a <- unlist(df_itri_te1[df_itri_te1$player.name=="Kathrine Møller Kühl",3])
  b <- unlist(df_itri_te2[i,3])
  result <- sapply(1:nrow(modstander1), function(j) {
  dist2d(a,b,unlist(modstander1$location[j]))
  })
  
  tempdf2 <- data.frame(result)
  colnames(tempdf2) <- df_itri_te2[i,1]
  modstander1 <- cbind(modstander1,tempdf2)
}

# Definerer vores threshold til afstanden for modspiller
df_itri_te2$skudsuc <- NA
thold <- 0.2

for (i in 1:nrow(df_itri_te2)) {
  # finder coll i modstander1
  col <- which(colnames(modstander1) == df_itri_te2[i,1])
  # ser om noget data er under thold
  TF <- any(modstander1[col] < thold)
  # indsætter dataen i df_itri_te2
  df_itri_te2$skudsuc[i] <- TF
  }

# Resultatet om der skulle have været en aflevering
ifelse(any(df_itri_te2$skudsuc==TRUE),"Skulle have været en aflevering","Skulle være et skud")

