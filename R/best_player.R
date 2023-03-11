
best_player_epv_avg <- function(epv, which_skill, min_attempts, top_X) {
  a <- aggregate(epv_added ~ skill*team*player_name, subset(epv, skill==which_skill), mean)
  colnames(a) <- c("skill","team", "player_name", "epv_added_avg")
  b <- aggregate(count ~ skill*team*player_name, subset(epv, skill==which_skill), sum)
  colnames(b) <- c("skill","team", "player_name", "attempts")
  c <- merge(a,b)
  c <- subset(c, attempts > min_attempts)
  c <- c[order(-c$epv_added),]
  best_player <- c[1:top_X,1:5]
  best_player$epv_added_avg <- round(best_player$epv_added_avg, 3)
  best_player <- best_player %>% relocate(player_name, team, skill, attempts, epv_added_avg)
  return(best_player)
}
