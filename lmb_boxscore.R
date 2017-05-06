library(RCurl)
library(XML)
library(XML2)

fname = file.choose()
LMB_2017 = read.csv(fname)


j1 <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_05/day_02/gid_2017_05_02_camaaa_leoaaa_1/rawboxscore.xml")
j2 <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_05/day_02/gid_2017_05_02_mtyaaa_vaqaaa_1/rawboxscore.xml")
j3 <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_05/day_02/gid_2017_05_02_mvaaaa_duraaa_1/rawboxscore.xml")
j4 <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_05/day_02/gid_2017_05_02_mxoaaa_sltaaa_1/rawboxscore.xml")
j5 <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_05/day_02/gid_2017_05_02_oaxaaa_yucaaa_1/rawboxscore.xml")
j6 <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_05/day_02/gid_2017_05_02_pueaaa_tabaaa_1/rawboxscore.xml")
j7 <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_05/day_02/gid_2017_05_02_tijaaa_aguaaa_1/rawboxscore.xml")
j8 <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_05/day_02/gid_2017_05_02_vraaaa_quiaaa_1/rawboxscore.xml")


game1 <- box(j1)
game2 <- box(j2)
game3 <- box(j3)
game4 <- box(j4)
game5 <- box(j5)
game6 <- box(j6)
game7 <- box(j7)
game8 <- box(j8)

LMB_2017 <- rbind(LMB_2017,game1,game2,game3,game4,game5,game6,game7,game8)

date <- function(x){
  a <- xml_find_all(x, "/boxscore/@game_id")
  val <- trimws(xml_text(a))
  val <- substring(val,1,10)
  val <- gsub("/","-",val)
  return(val)
}

awayteam <- function(x){
  a <- xml_find_all(x, "/boxscore/team/@team_code")
  val <- trimws(xml_text(a[1]))
  return(toupper(val))
}

hometeam <- function(x){
  a <- xml_find_all(x, "/boxscore/team/@team_code")
  val <- trimws(xml_text(a[2]))
  return(toupper(val))
}

attendance <- function(x){
  a <- xml_find_all(x, "/boxscore/@attendance")
  val <- trimws(xml_text(a))
  val <- as.integer(gsub(",","", val))
  return(val)
}

time <- function(x){
  a <- xml_find_all(x, "/boxscore/@elapsed_time")
  val <- trimws(xml_text(a))
  val <- substring(val,1,4)
  val <- gsub(" ","",paste(val,":00"))
  val <- 60 * 24 * as.numeric(times(val))
  return(val)
}

home_runs <- function(x){
  a <- xml_find_all(x, "/boxscore/linescore/@home_team_runs")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}

away_runs <- function(x){
  a <- xml_find_all(x, "/boxscore/linescore/@away_team_runs")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}

home_hits <- function(x){
  a <- xml_find_all(x, "/boxscore/linescore/@home_team_hits")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}

away_hits <- function(x){
  a <- xml_find_all(x, "/boxscore/linescore/@away_team_hits")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}

box <- function(x){
  date <- date(x)
  at <- awayteam(x)
  ht <- hometeam(x)
  att <- attendance(x)
  tim <- time(x)
  ar <- away_runs(x)
  ah <- away_hits(x)
  hr <- home_runs(x)
  hh <- home_hits(x)
  
  if(at == "AGS" || at == "DUR" || at == "SLT" || at == "VAQ" || at == "MXO" || at == "TIJ" || at == "MVA" || at == "MTY"){
    zone <- "NTE"
  }else if(at == "CAM" || at == "LEO" || at == "OAX" || at == "PUE" || at == "QUI" || at == "TAB" || at == "VRA" || at == "YUC"){
    zone <- "SUR"
  }
  game <- data.frame(DATE = date,VISITA=at,LOCAL=ht,ZONA=zone,CV = ar,CL = hr,HV = ah,HL=hh,ATT=att,TIME=tim,OBS="")
  return(game)
}