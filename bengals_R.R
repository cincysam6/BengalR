library(nflfastR)
library(dplyr)
library(ggplot2)
library(sqldf)


library(devtools)
devtools::install_github(repo = "maksimhorowitz/nflscrapR")
library(nflscrapR)

season_2020<-fast_scraper_schedules(2020, pp = FALSE)

Bengals_games<-season_2020%>%filter(away_team == "CIN"|home_team == "CIN")

wk_1_data <-fast_scraper(Bengals_games$game_id[1], source = "nfl", pp = FALSE)
wk_2_data<-fast_scraper(Bengals_games$game_id[2], source = "nfl", pp = FALSE)
wk_3_data<-fast_scraper(Bengals_games$game_id[3], source = "nfl", pp = FALSE)
wk_4_data<-fast_scraper(Bengals_games$game_id[4], source = "nfl", pp = FALSE)

Bengals_season<-rbind(wk_1_data,wk_2_data,wk_3_data,wk_4_data)

data<-Bengals_season

# Bengals QB Stats
data%>%
  group_by(passer_player_name)%>%
  filter(!is.na(passer_player_name))%>%
  filter(posteam == 'CIN')%>%
  summarise(yards = sum(yards_gained),
            air_yards = sum(air_yards),
            touchdowns = sum(touchdown),
            complete_passes= sum(complete_pass),
            pass_attempts = sum(pass_attempt),
            interceptions = sum(interception))%>%
  mutate(completion_pct = complete_passes/pass_attempts)%>%
select(passer_player_name,yards,touchdowns,complete_passes,pass_attempts,interceptions)

pass_location<-Bengals_season%>%filter(passer_player_name == 'J.Burrow')%>%select(play_id,yards_gained,pass_length,pass_location,air_yards)
pass_location_data<-sqldf("select pass_length, pass_location,count(*) as plays,sum(yards_gained) as yds_gained,sum(air_yards) as air_yards,sum(yards_gained)/count(*) as yds_per_play from pass_location group by pass_length, pass_location")

# QB Graphs
qb<-"J.Burrow"
qb_graph_data<-data %>% filter(passer_player_name == qb)%>%select(pass_length,pass_location,air_yards,yards_gained,complete_pass,desc)
ggplot(qb_graph_data, aes(pass_location,air_yards, colour = as.factor(complete_pass))) + 
  geom_point() + ggtitle(qb) + geom_hline(yintercept = 0 , linetype = "dashed")



pass_location_qb<-data%>%filter(passer_player_name == qb)%>%select(play_id,yards_gained,pass_length,pass_location,air_yards)
pass_location_data_qb<-sqldf("select pass_length, pass_location,count(*) as plays,sum(yards_gained) as yds_gained,sum(air_yards) as air_yards,sum(yards_gained)/count(*) as yds_per_play from pass_location_wr group by pass_length, pass_location")
pass_location_data_qb



# Bengals Running Stats
data%>%
  group_by(rusher_player_name)%>%
  filter(!is.na(rusher_player_name))%>%
  filter(posteam == 'CIN')%>%
  summarise(yards = sum(yards_gained),
            touchdowns = sum(rush_touchdown),
            rush_att = sum(rush_attempt),
            fumble = sum(fumble)
  )%>%
  mutate(yards_per_carry = yards/rush_att)%>%
  select(rusher_player_name,yards,touchdowns,rush_att,yards_per_carry,fumble)

# Bengals running charts
rb<-"J.Mixon"
rb_graph_data<-data %>% filter(rusher_player_name == rb & play_type == "run")%>%select(run_gap,run_location,yards_gained,complete_pass,desc)
rb_graph_data_v2<-sqldf("Select run_location,run_gap, yards_gained from rb_graph_data")
rb_graph_data_v2$run_loc<- paste(rb_graph_data_v2$run_location,rb_graph_data_v2$run_gap)
rb_graph_data_v2 <- rb_graph_data_v2 %>% dplyr::mutate(run_loc = factor(run_loc, levels =c("left end", "left tackle","left guard","middle NA","right guard","right tackle","right end")))

ggplot(rb_graph_data_v2, aes(run_loc,yards_gained)) + 
  geom_point() + ggtitle(rb) + geom_hline(yintercept = 0 , linetype = "dashed")



# Bengals Receiving Stats
data%>%
  group_by(receiver_player_name)%>%
  filter(!is.na(receiver_player_name))%>%
  filter(posteam == 'CIN')%>%
  summarise(yards = sum(yards_gained),
            touchdowns = sum(touchdown),
            receptions = sum(complete_pass),
            targets = sum(pass_attempt),
            tot_epa = sum(epa,na.rm = TRUE),
            avg_epa = mean(epa,na.rm = TRUE),
            aDOT = mean(air_yards),
            yards_after_catch = sum(yards_after_catch, na.rm = TRUE))%>%
  mutate(yards_per_catch = yards/receptions, completion_rate = receptions/targets)%>%
  select(receiver_player_name,yards,touchdowns,receptions,targets,aDOT,avg_epa,completion_rate,yards_per_catch,yards_after_catch,tot_epa)


# Receiver Graphs
wr_names<-unique(Bengals_season%>%filter(posteam=='CIN')%>%select(receiver_player_name))
wr_names
wr<-"A.Green"
wr_graph_data<-data %>% filter(receiver_player_name == wr)%>%select(pass_length,pass_location,air_yards,yards_gained,complete_pass,desc)
wr_graph_data

pass_location_wr<-data%>%filter(receiver_player_name == wr)%>%select(play_id,yards_gained,pass_length,pass_location,air_yards)
pass_location_data_wr<-sqldf("select pass_length, pass_location,count(*) as plays,sum(yards_gained) as yds_gained,sum(air_yards) as air_yards,sum(yards_gained)/count(*) as yds_per_play from pass_location_wr group by pass_length, pass_location")
pass_location_data_wr


ggplot(wr_graph_data, aes(pass_location,air_yards, colour = as.factor(complete_pass))) + 
  geom_point() + ggtitle(wr) + geom_hline(yintercept = 0 , linetype = "dashed")

# Bengals Defensive Stats

bengals_solo_tackles<-data%>%
  select(solo_tackle_1_player_name,solo_tackle_2_player_id,solo_tackle_1_team,solo_tackle_2_team)%>%
  filter(solo_tackle_1_team == "CIN"|solo_tackle_2_team =="CIN")

bengals_solo_tackles_boxscore<-sqldf("SELECT solo_tackle_1_player_name,count(*) as solo_tackles from bengals_solo_tackles group by solo_tackle_1_player_name order by solo_tackles desc")

bengals_assist_tackles<-data%>%
  select(assist_tackle_1_player_name,assist_tackle_2_player_id,assist_tackle_1_team,assist_tackle_2_team)%>%
  filter(assist_tackle_1_team == "CIN"|assist_tackle_2_team =="CIN")

bengals_tackles_for_loss_boxscore<-sqldf("SELECT tackle_for_loss_1_player_name,count(*) as tackles_for_loss from bengals_assist_tackles group by tackle_for_loss_1_player_name order by assist_tackles desc")



### TACKLES FOR LOSS
bengals_tackle_for_loss<-data%>%
  select(tackle_for_loss_1_player_name,tackle_for_loss_2_player_name,tackle_for_loss_1_team,tackle_for_loss_2_team)%>%
  filter(tackle_for_loss_1_team == "CIN"|tackle_for_loss_2_team =="CIN")

bengals_tackles_for_loss_boxscore<-sqldf("SELECT tackle_for_loss_1_player_name,count(*) as tackles_for_loss from wk_1_data group by tackle_for_loss_1_player_name order by tackles_for_loss desc")







# Go pull all the other data that you saw prior


clean_wk_1_data<-clean_pbp(wk_1_data)

pass_location<-wk_1_data%>%filter(passer_player_name == 'J.Burrow')%>%select(play_id,yards_gained,pass_length,pass_location,air_yards)

pass_location_data<-sqldf("select pass_length, pass_location,count(*) as plays,sum(yards_gained) as yds_gained,sum(air_yards) as air_yards from pass_location group by pass_length, pass_location")


# Looking at drives

drive_num<-5

drive_1<-wk_1_data%>%select(drive_play_count	,
                            yardline_100	,
                            posteam	,
                                               side_of_field 	,
                                                fixed_drive	,
                                               yards_gained	,
                                               yrdln	,
                                              down	,
                                             ydstogo	,
                                            desc, play_id)%>%filter(fixed_drive == drive_num & posteam == "CIN" & !is.na(down))%>%mutate(play_num = 1:n())


vertical.lines = seq(10,100,10)

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}


drive_chart<-ggplot(drive_1,aes(x=yardline_100,y=play_num, label = desc)) +  geom_text(aes(label=desc),vjust = 1, nudge_y = -0.1) + geom_point(size = 2) + scale_y_reverse(breaks = integer_breaks()) + geom_segment(aes(x=yardline_100,xend=yardline_100-yards_gained,y=play_num,yend=play_num),col='black') + scale_x_reverse(limits = c(100, 0),breaks=seq(0,100,10)) + geom_vline(xintercept = vertical.lines, linetype = 'dashed', color = 'white') + geom_vline(xintercept = 0, color = 'white', size = 3)
drive_chart  + theme(panel.background = element_rect(fill = "#33CC33"),
  # Hide panel borders and remove grid lines
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Change axis line
  axis.line = element_line(colour = "black")
)  + labs(title="The Bengals Offensive Drives",
           y ="play of drive", x = "yards to go")

## Generate information for the drive output

Drive_result<-paste("The result of the drive was a",unique(wk_1_data%>%filter(fixed_drive == drive_num)%>%select(fixed_drive_result)))
Drive_result

Drive_time<-paste("The drive took",unique(wk_1_data%>%filter(fixed_drive == drive_num)%>%select(drive_time_of_possession)))
Drive_time