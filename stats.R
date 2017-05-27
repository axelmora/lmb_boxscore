LMB2017x <- LMB2017
LMB2017x$DATE <- as.Date(LMB2017x$DATE, "%Y-%m-%d")
LMB2017x$DATE <- as.character(LMB2017x$DATE)
summary(LMB2017x)

LMB2017x$CT <- LMB2017x$CV + LMB2017x$CL
LMB2017x$HT <- LMB2017x$HV + LMB2017x$HL

LMB2017x_SUR <- sqldf::read.csv.sql("LMB2017x",sql="select * from LMB2017x where ZONA = 'SUR'")
LMB2017x_NTE <- sqldf::read.csv.sql("LMB2017x",sql="select * from LMB2017x where ZONA = 'NTE'")
summary(LMB2017x_NTE)
summary(LMB2017x_SUR)

w_local <- sqldf::read.csv.sql("LMB2017x",sql="select LOCAL, count(LOCAL) from LMB2017x where CL > CV group by LOCAL")
l_local <- sqldf::read.csv.sql("LMB2017x",sql="select LOCAL, count(LOCAL) from LMB2017x where CL < CV group by LOCAL")
w_visita <- sqldf::read.csv.sql("LMB2017x",sql="select VISITA, count(VISITA) from LMB2017x where CL < CV group by VISITA")
l_visita <- sqldf::read.csv.sql("LMB2017x",sql="select VISITA, count(VISITA) from LMB2017x where CL > CV group by VISITA")

avg_time_att <- sqldf::read.csv.sql("LMB2017x",sql="select DATE as date, 
                            avg(TIME) as time, avg(ATT) as att from LMB2017x group by DATE")

runs_avg_day <- sqldf::read.csv.sql("LMB2017x",sql="select avg(CT) as runs_total from 
                                    LMB2017x group by DATE")

dates <- sqldf::read.csv.sql("LMB2017x",sql="select distinct(date) from LMB2017x")

runs_avg_tot <- sqldf::read.csv.sql("LMB2017x",sql="select avg(CT) as avg_total from LMB2017x")
runs_avg_nt <- sqldf::read.csv.sql("LMB2017x",sql="select avg(CT) as avg_total from LMB2017x_NTE")
runs_avg_sr <- sqldf::read.csv.sql("LMB2017x",sql="select avg(CT) as avg_total from LMB2017x_SUR")

plot(dates, runs_avg_day)

plot(x=dates$DATE, y=runs_avg_day$runs_total)
lines(x=runs_total_day$date , y=runs_total_day$runs_total)
abline(h=runs_avg_day,col="red")
abline(h=runs_avg_day_nt,col="blue")
abline(h=runs_avg_day_sr,col="green")

att_t_a <- sqldf::read.csv.sql("LMB2017x",sql="select LOCAL, sum(ATT) as total, 
                           avg(ATT) as promedio
                           from LMB2017x group by LOCAL")

att_avg <- sqldf::read.csv.sql("LMB2017x",sql="select avg(ATT) as promedio from LMB2017x")

att_med <- sqldf::read.csv.sql("LMB2017x",sql="select avg(ATT) as promedio from LMB2017x")

att_avg <- sqldf::read.csv.sql("LMB2017x",sql="select avg(ATT) as promedio from LMB2017x")

att_top <- sqldf::read.csv.sql("LMB2017x",sql="select LOCAL, sum(ATT) as total, 
                           avg(ATT) as promedio
                           from LMB2017x group by LOCAL having promedio > (
                               select avg(ATT) as promedio from LMB2017x)")

att_top2 <- sqldf::read.csv.sql("LMB2017x",sql="select *
                                from LMB2017x where TIME < (
                                  select avg(TIME) from LMB2017x) and ATT > 
                                    (select avg(ATT) from LMB2017x) and CT <
                                      (select avg(CT) from LMB2017x)")

prueba <- sqldf::read.csv.sql("LMB2017x", sql= "select fecha, count(distinct LOCAL) as openings from 
                              LMB2017x group by fecha")

len <- sqldf::read.csv.sql("LMB2017x",sql="select LOCAL, avg(duracion) as promedio
                           from LMB2017x group by LOCAL")

att_tot <- sqldf::read.csv.sql("LMB2017x",sql="select sum(ATT) as total
                           from LMB2017x")

durmin <- sqldf::read.csv.sql("LMB2017x",sql="select visita, LOCAL, duracion, fecha from LMB2017x 
                              where duracion = (select min(duracion) from LMB2017x)")

durmax <- sqldf::read.csv.sql("LMB2017x",sql="select visita, LOCAL, duracion, fecha from LMB2017x 
                              where duracion = (select max(duracion) from LMB2017x)")

att_mex <- sqldf::read.csv.sql("LMB2017x",sql="select LOCAL, ATT
                               from LMB2017x where LOCAL = 'MEX'")

att_lag <- sqldf::read.csv.sql("LMB2017x",sql="select LOCAL, ATT, fecha
                               from LMB2017x where LOCAL = 'LAG'")

x <- sqldf::read.csv.sql("LMB2017x",sql="select visita, LOCAL, ATT, fecha
                           from LMB2017x where ATT = 8800")

min <- sqldf::read.csv.sql("LMB2017x",sql="select LOCAL, min(ATT), fecha
                           from LMB2017x group by LOCAL")


att_mva <- sqldf::read.csv.sql("LMB2017x",sql="select distinct * from LMB2017x where LOCAL = 'MVA'")
att_ags <- sqldf::read.csv.sql("LMB2017x",sql="select distinct * from LMB2017x where LOCAL = 'AGS'")
att_sal <- sqldf::read.csv.sql("LMB2017x",sql="select distinct * from LMB2017x where LOCAL = 'SAL'")
att_tig <- sqldf::read.csv.sql("LMB2017x",sql="select distinct * from LMB2017x where LOCAL = 'TIG'")
att_tij <- sqldf::read.csv.sql("LMB2017x",sql="select distinct * from LMB2017x where LOCAL = 'TIJ'")
att_mex <- sqldf::read.csv.sql("LMB2017x",sql="select distinct * from LMB2017x where LOCAL = 'MEX'")
att_oax <- sqldf::read.csv.sql("LMB2017x",sql="select distinct * from LMB2017x where LOCAL = 'OAX'")
att_lag <- sqldf::read.csv.sql("LMB2017x",sql="select distinct * from LMB2017x where LOCAL = 'LAG'")
att_mty <- sqldf::read.csv.sql("LMB2017x",sql="select distinct * from LMB2017x where LOCAL = 'MTY'")
att_cdc <- sqldf::read.csv.sql("LMB2017x",sql="select distinct * from LMB2017x where LOCAL = 'CDC'")
att_cam <- sqldf::read.csv.sql("LMB2017x",sql="select distinct * from LMB2017x where LOCAL = 'CAM'")
att_yuc <- sqldf::read.csv.sql("LMB2017x",sql="select distinct * from LMB2017x where LOCAL = 'YUC'")
att_ver <- sqldf::read.csv.sql("LMB2017x",sql="select distinct * from LMB2017x where LOCAL = 'VER'")
att_tab <- sqldf::read.csv.sql("LMB2017x",sql="select distinct * from LMB2017x where LOCAL = 'TAB'")
att_pue <- sqldf::read.csv.sql("LMB2017x",sql="select distinct * from LMB2017x where LOCAL = 'PUE'")
att_rey <- sqldf::read.csv.sql("LMB2017x",sql="select distinct * from LMB2017x where LOCAL = 'REY'")


summary(att_mex)
