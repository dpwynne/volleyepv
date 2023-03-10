touches <- aggregate(count ~ team*player_name, mens_test, sum)
colnames(touches)[3] <- "totaltouches"
sets <- aggregate(count ~ team*player_name, subset(mens_test, skill=="Set"), sum)
colnames(sets)[3] <- "sets"
rec_dig <- aggregate(count ~ team*player_name, subset(mens_test, skill %in% c("Dig", "Reception")), sum)
colnames(rec_dig)[3] <- "rec_dig"
a <- aggregate(count ~ team*player_name, subset(mens_test, skill == "Attack"), sum)
b <- aggregate(count ~ team*player_name, subset(mens_test, skill == "Attack" & start_zone %in% c("2", "9")), sum)
colnames(b)[3] <- "Opposite"
c <- aggregate(count ~ team*player_name, subset(mens_test, skill == "Attack" & start_zone %in% c("4", "8", "7")), sum)
colnames(c)[3] <- "Outside"
d <- aggregate(count ~ team*player_name, subset(mens_test, skill == "Attack" & (start_zone == 3 | attack_code %in% c("X1", "X7", "XM", "X2", "CF", "CD"))), sum)
colnames(d)[3] <- "Middle"


total <- merge(touches, sets, all.x = TRUE)
total <- merge(total, rec_dig, all.x = TRUE)
total <- merge(total, a, all.x = TRUE)
total <- merge(total, b, all.x = TRUE)
total <- merge(total, c, all.x = TRUE)
total <- merge(total, d, all.x = TRUE)

total[is.na(total)] <- 0

total$p2 <- total$Opposite/total$count
total$p3 <- total$Middle/total$count
total$p4 <- total$Outside/total$count


total$position <- NA
total$position <- ifelse(total$sets / total$totaltouches > 0.5, "Setter", total$position)
total$position <- ifelse(is.na(total$position) & total$rec_dig / total$totaltouches > 0.5, "L/DS", total$position)
total$position <- ifelse(is.na(total$position) & total$p2 > total$p3 & total$p2 > total$p4, "Opposite", total$position)
total$position <- ifelse(is.na(total$position) & total$p3 > total$p2 & total$p3 > total$p4, "Middle", total$position)
total$position <- ifelse(is.na(total$position) & total$p4 > total$p3 & total$p4 > total$p2, "Outside", total$position)
total$position <- ifelse(is.na(total$position), "unsure", total$position)
total <- select(total, team, player_name, position)

mens_test <- merge(mens_test, total, all.x = TRUE)
