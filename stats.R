LMB2017$DATE <- as.Date(LMB2017$DATE, "%Y-%m-%d")
LMB2017$DATE <- as.character(LMB2017$DATE)
summary(LMB2017)

LMB2017$CT <- LMB2017$CV + LMB2017$CL
LMB2017$HT <- LMB2017$HV + LMB2017$HL

LMB2017_SUR <- sqldf::read.csv.sql("LMB2017",sql="select * from LMB2017 where ZONA = 'SUR'")
LMB2017_NTE <- sqldf::read.csv.sql("LMB2017",sql="select * from LMB2017 where ZONA = 'NTE'")
summary(LMB2017_NTE)
summary(LMB2017_SUR)

avg_time_att <- sqldf::read.csv.sql("LMB2017",sql="select DATE as date, 
                            avg(TIME) as time, avg(ATT) as att from LMB2017 group by DATE")

runs_total_day <- sqldf::read.csv.sql("LMB2017",sql="select DATE as date, 
                            avg(CT) as runs_total from LMB2017 group by DATE")

dates <- sqldf::read.csv.sql("LMB2017",sql="select distinct(date) from LMB2017")
runs_avg_day <- sqldf::read.csv.sql("LMB2017",sql="select avg(CT) as avg_total from LMB2017")
runs_avg_day_nt <- sqldf::read.csv.sql("LMB2017",sql="select avg(CT) as avg_total from LMB2017_NTE")
runs_avg_day_sr <- sqldf::read.csv.sql("LMB2017",sql="select avg(CT) as avg_total from LMB2017_SUR")

plot(x=runs_total_day$date, y=runs_total_day$runs_total)

plot(x=runs_total_day$date , y=runs_total_day$runs_total)
lines(x=runs_total_day$date , y=runs_total_day$runs_total)
abline(h=runs_avg_day,col="red")
abline(h=runs_avg_day_nt,col="blue")
abline(h=runs_avg_day_sr,col="green")

att_t_a <- sqldf::read.csv.sql("LMB2017",sql="select LOCAL, sum(ATT) as total, 
                           avg(ATT) as promedio
                           from LMB2017 group by LOCAL")

att_avg <- sqldf::read.csv.sql("LMB2017",sql="select avg(ATT) as promedio from LMB2017")

att_med <- sqldf::read.csv.sql("LMB2017",sql="select avg(ATT) as promedio from LMB2017")

att_avg <- sqldf::read.csv.sql("LMB2017",sql="select avg(ATT) as promedio from LMB2017")

att_top <- sqldf::read.csv.sql("LMB2017",sql="select LOCAL, sum(ATT) as total, 
                           avg(ATT) as promedio
                           from LMB2017 group by LOCAL having promedio > (
                               select avg(ATT) as promedio from LMB2017)")

att_top2 <- sqldf::read.csv.sql("LMB2017",sql="select *
                                from LMB2017 where TIME < (
                                  select avg(TIME) from LMB2017) and ATT > 
                                    (select avg(ATT) from LMB2017) and CT <
                                      (select avg(CT) from LMB2017)")

prueba <- sqldf::read.csv.sql("LMB2017", sql= "select fecha, count(distinct LOCAL) as openings from 
                              LMB2017 group by fecha")

len <- sqldf::read.csv.sql("LMB2017",sql="select LOCAL, avg(duracion) as promedio
                           from LMB2017 group by LOCAL")

att_tot <- sqldf::read.csv.sql("LMB2017",sql="select sum(ATT) as total
                           from LMB2017")

durmin <- sqldf::read.csv.sql("LMB2017",sql="select visita, LOCAL, duracion, fecha from LMB2017 
                              where duracion = (select min(duracion) from LMB2017)")

durmax <- sqldf::read.csv.sql("LMB2017",sql="select visita, LOCAL, duracion, fecha from LMB2017 
                              where duracion = (select max(duracion) from LMB2017)")

att_mex <- sqldf::read.csv.sql("LMB2017",sql="select LOCAL, ATT
                               from LMB2017 where LOCAL = 'MEX'")

att_lag <- sqldf::read.csv.sql("LMB2017",sql="select LOCAL, ATT, fecha
                               from LMB2017 where LOCAL = 'LAG'")

x <- sqldf::read.csv.sql("LMB2017",sql="select visita, LOCAL, ATT, fecha
                           from LMB2017 where ATT = 8800")

min <- sqldf::read.csv.sql("LMB2017",sql="select LOCAL, min(ATT), fecha
                           from LMB2017 group by LOCAL")


att_mva <- sqldf::read.csv.sql("LMB2017",sql="select distinct * from LMB2017 where LOCAL = 'MVA'")
att_ags <- sqldf::read.csv.sql("LMB2017",sql="select distinct * from LMB2017 where LOCAL = 'AGS'")
att_sal <- sqldf::read.csv.sql("LMB2017",sql="select distinct * from LMB2017 where LOCAL = 'SAL'")
att_tig <- sqldf::read.csv.sql("LMB2017",sql="select distinct * from LMB2017 where LOCAL = 'TIG'")
att_tij <- sqldf::read.csv.sql("LMB2017",sql="select distinct * from LMB2017 where LOCAL = 'TIJ'")
att_mex <- sqldf::read.csv.sql("LMB2017",sql="select distinct * from LMB2017 where LOCAL = 'MEX'")
att_oax <- sqldf::read.csv.sql("LMB2017",sql="select distinct * from LMB2017 where LOCAL = 'OAX'")
att_lag <- sqldf::read.csv.sql("LMB2017",sql="select distinct * from LMB2017 where LOCAL = 'LAG'")
att_mty <- sqldf::read.csv.sql("LMB2017",sql="select distinct * from LMB2017 where LOCAL = 'MTY'")
att_cdc <- sqldf::read.csv.sql("LMB2017",sql="select distinct * from LMB2017 where LOCAL = 'CDC'")
att_cam <- sqldf::read.csv.sql("LMB2017",sql="select distinct * from LMB2017 where LOCAL = 'CAM'")
att_yuc <- sqldf::read.csv.sql("LMB2017",sql="select distinct * from LMB2017 where LOCAL = 'YUC'")
att_ver <- sqldf::read.csv.sql("LMB2017",sql="select distinct * from LMB2017 where LOCAL = 'VER'")
att_tab <- sqldf::read.csv.sql("LMB2017",sql="select distinct * from LMB2017 where LOCAL = 'TAB'")
att_pue <- sqldf::read.csv.sql("LMB2017",sql="select distinct * from LMB2017 where LOCAL = 'PUE'")
att_rey <- sqldf::read.csv.sql("LMB2017",sql="select distinct * from LMB2017 where LOCAL = 'REY'")


summary(att_mex)
