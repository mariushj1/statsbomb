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

tri_4_shot <- list()
for (i in 1:nrow(mulig_spilning)) {
  temp_df <- dfForSingleShotTri(unlist(mulig_spilning$location[i]))
  tri_4_shot[[i]] <- temp_df
}

# Laver trekanten
dftriforshot <- dfForSingleShotTri(unlist(testshot$location))
triangleAlpha <- ifelse(input == FALSE, 0, 0.5)


hold <- input$hold
modstander <- as.character(unique(dkmshotsel[dkmshotsel$team.name != hold,"team.name"]))

# Laver ggplot 
ggplot(testff) +
  annotate_pitch (
    dimensions = pitch_statsbomb,
    colour = "white",
    fill = "steelblue4") +
  geom_text(data=testff,aes(x=x,y=y,label=player.name, color = "#ffffff"), size=3.5,vjust=1)+
  geom_text(data=testshot,aes(x=location.x,y=location.y,label=player.name, color = "#ffffff"), size=4.5,vjust=1) +
  geom_point(data=testff,aes(x = x, y = y, color=teammate), size = 2) +
  scale_color_manual(values = c("TRUE" = "#549e3e", "FALSE" = "#d13038"),
                     labels = c("TRUE" = hold, "FALSE" = modstander),
                     name = "") +
  geom_point(data=testshot,aes(x = location.x, y = location.y), size = 4) + 
  geom_segment(data=testshot,aes(x = location.x, 
                                 y = location.y, 
                                 xend = shot.end_location.x,
                                 yend=shot.end_location.y),
               colour = "yellow",
               size = 1) +
  labs(title = paste0("Skudforsøg nr ", skud, ". for ", player),
       subtitle = paste0(hold, " vs ", modstander)) +
  geom_polygon(data = dftriforshot,aes(x=sx,y=sy),alpha=triangleAlpha) +
  theme_pitch() +
  direction_label() +
  coord_flip(xlim = c(75, 121)) +
  scale_y_reverse() +
  theme(plot.title = element_text(face = "bold")) +
  guides(
    fill = guide_legend(
      title = "",
      override.aes = aes(label = "")
    )
  )
