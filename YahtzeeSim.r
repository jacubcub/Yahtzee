#install.packages("Rdice")
library(Rdice)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

y1ct <- 0
y2ct <- 0
y3ct <- 0
count <- 0

for (i in 1:25000){
  #First Roll
  dt1 <- as.numeric(dice.roll(6,5,1)$results)
  md1 <- getmode(dt1)
  new_dice <- length(dt1[dt1 != md1])
  md_count <- 5 - new_dice
  
  #Second Roll
  if (md_count < 5){
  dt2 <- as.numeric(dice.roll(6,new_dice,1)$results)
  md2 <- getmode(dt2)
    if (length(dt2[dt2 == md2]) > md_count & md1 != md2){
      new_dice <- 5 - length(dt2[dt2 == md2])
      md_count <- 5 - new_dice
    }else{
      md_count <- md_count + length(dt2[dt2 == md1])
      new_dice <- 5 - md_count
    }
  } else {print("Yahtzee after 1")
    y1ct <- y1ct + 1}
  
  #Third Roll
  if (md_count < 5){
    dt3 <- as.numeric(dice.roll(6,new_dice,1)$results)
    md3 <- getmode(dt3)
    if (length(dt3[dt3 == md3]) > md_count & md2 != md3){
      new_dice <- 5 - length(dt3[dt3 == md3])
      md_count <- 5 - new_dice
    }else{
      md_count <- md_count + length(dt3[dt3 == md2])
      new_dice <- 5 - md_count
    }
  } else {print("Yahtzee after 2")
    y2ct <- y2ct + 1}
  if (md_count ==5) {print("Yahtzee after 3")
    y3ct <- y3ct + 1}
count <- count + 1
}

print(c(y1ct,y2ct,y3ct))
