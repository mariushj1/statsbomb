
Spiller_lokation <- mulig_spilning[,c(1,4)]
Spiller_lokation <- rbind(Spiller_lokation,testshot[,c(7,4)])

df_itri <- data.frame()
for (i in 1:nrow(Spiller_lokation)) {
#Define the vertices of the triangle (example vertices)
  temp_df <- unlist(Spiller_lokation$location[i])
triangle_vertices <- matrix(c(temp_df[1], temp_df[2], 120, 36, 120, 44), ncol = 2, byrow = TRUE)

#Function to check if a point is inside the triangle
point_inside_triangle <- function(point, triangle_vertices) {
  # Extract vertices
  A <- triangle_vertices[1, ]
  B <- triangle_vertices[2, ]
  C <- triangle_vertices[3, ]
  
  #Compute barycentric coordinates
  lambda1 <- ((B[2] - C[2]) * (point[1] - C[1]) + (C[1] - B[1]) * (point[2] - C[2])) /
    ((B[2] - C[2]) * (A[1] - C[1]) + (C[1] - B[1]) * (A[2] - C[2]))
  lambda2 <- ((C[2] - A[2]) * (point[1] - C[1]) + (A[1] - C[1]) * (point[2] - C[2])) /
    ((B[2] - C[2]) * (A[1] - C[1]) + (C[1] - B[1]) * (A[2] - C[2]))
  lambda3 <- 1 - lambda1 - lambda2
  
  #Check if the point is inside the triangle
  return(all(c(lambda1, lambda2, lambda3) >= 0))
}

#Apply the function to each point in the dataframe
inside_triangle <- apply(testff[,7:8], 1, function(point) point_inside_triangle(point, triangle_vertices))

#Count the number of points inside the triangle
num_points_inside <- sum(inside_triangle)

tempdf1 <- data.frame("player.name"=Spiller_lokation$player.name[i],"Spiller i tri"=num_points_inside)

df_itri <- rbind(df_itri,tempdf1)

}

df_itri_te <- df_itri %>% 
  filter(Spiller.i.tri <= as.numeric(df_itri[df_itri$player.name == "Kathrine Møller Kühl",][2]))

df_itri_te1 <- merge(df_itri_te,Spiller_lokation, by = "player.name")

df_itri_te1$dist <- NA
for (i in 1:nrow(df_itri_te1)) {
  templist <- unlist(df_itri_te1$location[i])
  df_itri_te1$dist[i] <- sqrt((120-templist[1])^2+(40-templist[2])^2)
  
}

df_itri_te2 <- df_itri_te1 %>% 
  filter(dist <= as.numeric(df_itri_te1[df_itri_te1$player.name == "Kathrine Møller Kühl",][4]),
         player.name != "Kathrine Møller Kühl")

modstander1 <- testff %>% 
  filter(teammate == FALSE) %>% 
  select(player.name,location)

for (i in 1:nrow(df_itri_te2)) {
  a <- unlist(df_itri_te1[df_itri_te1$player.name=="Kathrine Møller Kühl",3])
  b <- unlist(df_itri_te2[i,3])
  result <- sapply(1:nrow(modstander1), function(j) {
  dist2d(a,b,unlist(modstander1$location[j]))
  })
  
  
}
dist2d <- function(a,b,c){
  v1 <- b-c
  v2 <- a-b
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
}

print(dist2d(c(1,10),c(2,2),c(3,3)))
