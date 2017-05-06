library(RCurl)
library(XML)
library(xml2)
library(chron)

LMB2017 = read.csv("LMB2017.csv")

#TEAMS
cam <- "cam"
pue <- "pue"
agu <- "agu"
mxo <- "mxo"
mva <- "mva"
mty <- "mty"
oax <- "oax"
qui <- "qui"
tab <- "tab"
leo <- "leo"
vaq <- "vaq"
dur <- "dur"
vra <- "vra"
yuc <- "yuc"
slt <- "slt"
tij <- "tij"

#MONTH AND DAY
mm <- "05"
dd <- "05"

#ALL GAMES AWAY AND HOME TEAMS ASIGMENT
aw1 <- cam
ho1 <- pue
aw2 <- agu
ho2 <- mxo
aw3 <- mva
ho3 <- mty
aw4 <- oax
ho4 <- qui
aw5 <- tab
ho5 <- leo
aw6 <- vaq
ho6 <- dur
aw7 <- vra
ho7 <- yuc
aw8 <- slt
ho8 <- tij

#GET ALL BOXSCORES
g1 <- read_xml(gsub(" ","",paste("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_",mm,"/day_",dd,"/gid_2017_",mm,"_",dd,"_",aw1,"aaa_",ho1,"aaa_1/rawboxscore.xml")))
g2 <- read_xml(gsub(" ","",paste("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_",mm,"/day_",dd,"/gid_2017_",mm,"_",dd,"_",aw2,"aaa_",ho2,"aaa_1/rawboxscore.xml")))
g3 <- read_xml(gsub(" ","",paste("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_",mm,"/day_",dd,"/gid_2017_",mm,"_",dd,"_",aw3,"aaa_",ho3,"aaa_1/rawboxscore.xml")))
g4 <- read_xml(gsub(" ","",paste("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_",mm,"/day_",dd,"/gid_2017_",mm,"_",dd,"_",aw4,"aaa_",ho4,"aaa_1/rawboxscore.xml")))
g5 <- read_xml(gsub(" ","",paste("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_",mm,"/day_",dd,"/gid_2017_",mm,"_",dd,"_",aw5,"aaa_",ho5,"aaa_1/rawboxscore.xml")))
g6 <- read_xml(gsub(" ","",paste("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_",mm,"/day_",dd,"/gid_2017_",mm,"_",dd,"_",aw6,"aaa_",ho6,"aaa_1/rawboxscore.xml")))
g7 <- read_xml(gsub(" ","",paste("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_",mm,"/day_",dd,"/gid_2017_",mm,"_",dd,"_",aw7,"aaa_",ho7,"aaa_1/rawboxscore.xml")))
g8 <- read_xml(gsub(" ","",paste("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_",mm,"/day_",dd,"/gid_2017_",mm,"_",dd,"_",aw8,"aaa_",ho8,"aaa_1/rawboxscore.xml")))

#BOX FUNCTION TO EXTRACT DATA
game1 <- box(g2,"")
game2 <- box(g2,"")
game3 <- box(g3,"")
game4 <- box(g4,"")
game5 <- box(g5,"")
game6 <- box(g6,"")
game7 <- box(g7,"")
game8 <- box(g8,"")

#UPDATE ACTUAL DATAFRAME
LMB2017 <- rbind(LMB2017,game1,game2,game3,game4,game5,game6,game7)
write.csv(LMB2017,file="LMB2017.csv")

#GET DATE
date <- function(x){
  a <- xml_find_all(x, "/boxscore/@game_id")
  val <- trimws(xml_text(a))
  val <- substring(val,1,10)
  val <- gsub("/","-",val)
  return(val)
}
#GET AWAY TEAM
awayteam <- function(x){
  a <- xml_find_all(x, "/boxscore/team/@team_code")
  val <- trimws(xml_text(a[1]))
  return(toupper(val))
}
#GET HOME TEAM
hometeam <- function(x){
  a <- xml_find_all(x, "/boxscore/team/@team_code")
  val <- trimws(xml_text(a[2]))
  return(toupper(val))
}
#GET ATTENDANCE
attendance <- function(x){
  a <- xml_find_all(x, "/boxscore/@attendance")
  val <- trimws(xml_text(a))
  val <- as.integer(gsub(",","", val))
  return(val)
}
#GET TIME
time <- function(x){
  a <- xml_find_all(x, "/boxscore/@elapsed_time")
  val <- trimws(xml_text(a))
  val <- substring(val,1,4)
  val <- gsub(" ","",paste(val,":00"))
  val <- 60 * 24 * as.numeric(times(val))
  return(val)
}
#GET RUNS SCORED BY HOME TEAM
home_runs <- function(x){
  a <- xml_find_all(x, "/boxscore/linescore/@home_team_runs")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#GET RUNS SCORED BY AWAY TEAM
away_runs <- function(x){
  a <- xml_find_all(x, "/boxscore/linescore/@away_team_runs")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#GET HITS BY HOME TEAM
home_hits <- function(x){
  a <- xml_find_all(x, "/boxscore/linescore/@home_team_hits")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#GET HITS BY AWAY TEAM
away_hits <- function(x){
  a <- xml_find_all(x, "/boxscore/linescore/@away_team_hits")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#ALL TOGHETER IN A DATAFRAME
box <- function(x,obs){
  date <- date(x)
  at <- awayteam(x)
  ht <- hometeam(x)
  att <- attendance(x)
  tim <- time(x)
  ar <- away_runs(x)
  ah <- away_hits(x)
  hr <- home_runs(x)
  hh <- home_hits(x)
  
  if(ht == "AGS" || ht == "DUR" || ht == "SLT" || ht == "VAQ" || ht == "MXO" || ht == "TIJ" || ht == "MVA" || ht == "MTY"){
    zone <- "NTE"
  }else if(ht == "CAM" || ht == "LEO" || ht == "OAX" || ht == "PUE" || ht == "QUI" || ht == "TAB" || ht == "VRA" || ht == "YUC"){
    zone <- "SUR"
  }
  game <- data.frame(DATE = date,VISITA=at,LOCAL=ht,ZONA=zone,CV = ar,CL = hr,HV = ah,HL=hh,ATT=att,TIME=tim,OBS=obs)
  return(game)
}