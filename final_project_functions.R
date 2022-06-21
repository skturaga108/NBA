###############################

# "cleanData" function reads in the dataset and cleans it up
# make sure that the "all_seasons.csv" file is in your working directoy
# make sure the "dplyr" library is installed and called

cleanData = function() {
  
  library(ggplot2)
  library(dplyr)
  library(bslib)
  library(lpSolve)
  library(lpSolveAPI)
  
  setwd("C:\\Users\\jdrury\\R\\Week5")
  
  # download data (assumes file is in working directory), and classifies columns appropriately
  raw = read.csv(file="all_seasons.csv", header=T, sep=",", colClasses = c("character", "factor", 
                                                                           "numeric", "numeric", "numeric", "character", "factor", 
                                                                           "factor", "factor", "factor",rep("numeric", 10),"factor"))
  
  # updates column names for more intuitive data analysis
  names(raw)[c(1,2,4,5,11:20)] = c("player", "team", "height_cm", "weight_kg", "games_played", "points", "rebounds", 
                                    "assists", "net_rating", "offensive_reb_pct", "defensive_reb_pct", 
                                    "usage_pct","true_shooting_pct", "assist_pct")
  
  # create heights and weights in inches and pounds; re-order data; add total stats for each player in a season
  raw = raw %>%
    mutate("weight" = (weight_kg*2.204623), "height" = (height_cm*0.3937008), 
           "total_points" = (points*games_played), "total_rebounds" = (rebounds*games_played), "total_assists" = (assists*games_played))
  raw = raw[,c(1:5, 23, 22, 6:14, 24:26, 15:21)]
  
  raw = raw %>%
    select(-c("weight_kg", "height_cm"))
  
  # add total games played for each team and season, this will help calculate total team stats
  raw$total_games = NA
  raw$total_games = ifelse(raw$season == "1998-99", 50, 82)
  raw$total_games = ifelse(raw$season == "2011-12", 66, raw$total_games)
  raw$total_games = ifelse(raw$season == "2012-13" & raw$team == "BOS", 81, raw$total_games)
  raw$total_games = ifelse(raw$season == "2012-13" & raw$team == "IND", 81, raw$total_games)
  raw$total_games = ifelse(raw$season == "2020-21", 72, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "MIL", 73, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "TOR", 72, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "BOS", 72, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "IND", 73, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "MIA", 73, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "PHI", 73, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "BRK", 72, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "ORL", 73, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "WAS", 72, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "CHA", 65, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "CHI", 65, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "NYK", 66, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "DET", 66, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "ATL", 67, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "CLE", 65, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "LAL", 71, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "LAC", 72, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "DEN", 73, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "HOU", 72, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "OKC", 72, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "UTA", 72, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "DAL", 75, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "POR", 74, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "MEM", 73, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "PHX", 73, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "SAN", 71, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "SAC", 72, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "NOP", 72, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "MIN", 64, raw$total_games)
  raw$total_games = ifelse(raw$season == "2019-20" & raw$team == "GSW", 65, raw$total_games)
  
  # make sure teams that changed names are consistent (current name is kept)
  raw[raw$team=="NJN","team"] = "BKN"
  raw[raw$team=="VAN","team"] = "MEM"
  raw[raw$team=="CHH","team"] = "NOP"
  raw[raw$team=="NOH","team"] = "NOP"
  raw[raw$team=="NOK","team"] = "NOP"
  raw[raw$team=="SEA","team"] = "OKC"
  
  # change season column to numeric; keep year season began
  raw$season = substr(raw$season, 1, 4)
  raw$season = as.integer(raw$season)
  
  raw$team = droplevels(raw$team)
  
  return(raw)

}

data = cleanData()

mike = data.frame(
player = c(rep("Mike",14)),
season = c(1984:1997),
points = c(28.2,22.7,37.1,35.0,32.5,33.6,31.5,30.1,32.6,NA,26.9,30.4,29.6,28.7),
rebounds = c(6.5,3.6,5.2,5.5,8.0,6.9,6.0,6.4,6.7,NA,6.9,6.6,5.9,5.8),
assists = c(5.9,2.9,4.6,5.9,8.0,6.3,5.5,6.1,5.5,NA,5.3,4.3,4.3,3.5),
usage_pct = c(0.298,0.386,0.383,0.341,0.321,0.337,0.329,0.317,0.347,NA,0.332,0.333,0.332,0.337))

############################################

# the function "team_season" creates a dataframe for a team's points per game, rebounds per game, and assists per game
# arguments: t = team of interest; year_start = earliest year in matrix; year_end = most recent year in matrix

team_season = function(t,year_start, year_end) {
  
  years = c(year_start:year_end)
  
  for (y in seq_along(years)) {
  
  year_stats = data %>%
      filter(team==t, season==years[[y]]) %>%
      select(player, total_points, total_rebounds, total_assists, total_games) %>%
      mutate(team_total_pts = sum(total_points)/total_games, 
             team_total_rbs = sum(total_rebounds)/total_games, team_total_ast = sum(total_assists)/total_games) %>%
      summarize(pts = median(team_total_pts), rbs = median(team_total_rbs), ast = median(team_total_ast))
  
  year_stats = matrix(year_stats[1,1:3])
  year_stats = t(year_stats)
  
  if (y==1) {all_stats = rbind(year_stats)} else {all_stats = rbind(all_stats, year_stats)}
  }
  
  colnames(all_stats) = c("ppg", "rpg", "apg")
  rownames(all_stats) = c(years)
  return(all_stats)
}

team_season("CHI", 2015, 2020)


# the function "player_season" creates a dataframe for a player's points per game, rebounds per game, assists per game,
# true shooting percentage, and usage rate
# arguments: p = player of interest; year_start = earliest year in matrix; year_end = most recent year in matrix

player_season = function(p, year_start, year_end) {
  
  years = c(year_start:year_end)
  
  for (y in seq_along(years)) {
    
    year_stats = data %>%
      filter(player==p, season==years[[y]]) %>%
      select(points, rebounds, assists, true_shooting_pct, season)
    
    if (y==1) {all_stats = year_stats} else {all_stats = rbind(all_stats, year_stats)}
  }
  
  return(all_stats)
  
}


#################### graph to compare two players ###########################


comp_graph_data = function(p1, p2, start, end) {
  
  player_df1 = player_season(p1, start, end)
  player_df2 = player_season(p2, start, end)
  
  player_df = data.frame(x = c(rep(player_df1$season,3), rep(player_df2$season,3)),
                         y = c(player_df1$points, player_df1$rebounds, player_df1$assists, 
                               player_df2$points, player_df2$rebounds, player_df2$assists),
                         stats= c(rep("Points", nrow(player_df1)),
                                  rep("Rebounds", nrow(player_df1)),
                                  rep("Assists", nrow(player_df1)),
                                  rep("Points", nrow(player_df2)),
                                  rep("Rebounds", nrow(player_df2)),
                                  rep("Assists", nrow(player_df2))),
                         player = c(rep(p1,nrow(player_df1)*3),rep(p2,nrow(player_df2)*3)))
  
  
  return(player_df)

}



comp_graph = function(p1, p2, start, end) {

  player_df1 = player_season(p1, start, end)
  player_df2 = player_season(p2, start, end)
  
  player_df = data.frame(x = c(rep(player_df1$season,3), rep(player_df2$season,3)),
                         y = c(player_df1$points, player_df1$rebounds, player_df1$assists, 
                               player_df2$points, player_df2$rebounds, player_df2$assists),
                         stats= c(rep("Points", nrow(player_df1)),
                                  rep("Rebounds", nrow(player_df1)),
                                  rep("Assists", nrow(player_df1)),
                                  rep("Points", nrow(player_df2)),
                                  rep("Rebounds", nrow(player_df2)),
                                  rep("Assists", nrow(player_df2))),
                         player = c(rep(p1,nrow(player_df1)*3),rep(p2,nrow(player_df2)*3)))
  
  
  plot = ggplot(player_df, aes(x, y, col = stats)) + geom_point() +
    geom_smooth(se=F) + ggtitle(paste(p1," and ",p2,"'s Stats", sep="")) + xlab("Season") + ylab("Stat") +
    ylim(0,max(player_df[,2])) + xlim(start, end) + facet_grid(cols = vars(player))

  return(plot)
}






###### function to get average teams usage rate for that year for those teams 5 best players

avg_usage = function(s) {

results = c()

for (t in levels(data$team)) {
  
  team_usage = data %>%
    filter(season==s, team==t) %>%
    select(player, points, usage_pct) %>%
    arrange(desc(points))
  
  SUM = sum(team_usage[1:5,"usage_pct"])
  
  results = append(results, SUM)
  
}

out = mean(results, na.rm=T)
out = round(out,2)

return(out)

}



########## optimization problem #############################################################

########### in stall packages #################
# install.packages("lpSolveAPI", dependencies = T)
# install.packages("lpSolve", dependencies = T)
# library(lpSolveAPI)
# library(lpSolve)
###############################################


######## this function generates the best possible team (of a certain stat), constrained by usage ##################

top5 = function(one_season, optimized_stat="points", type="max", constraint=avg_usage(one_season)) {

  player_list = data %>%
    filter(season==one_season) %>%
    select(player, age, height, weight, points, rebounds, assists, net_rating, usage_pct, true_shooting_pct)
  
  var = player_list[,optimized_stat]
  usg = player_list$usage_pct
  x = matrix(1, nrow=dim(player_list)[1])
  
  model = make.lp(nrow=0, ncol=dim(player_list)[1])
  name.lp(model, name="Top 5")
  
  for (i in 1:dim(player_list)[1]) {
    set.type(model, columns=i, type="binary")
  }
  
  lp.control(model, sense=type)
  set.objfn(model, obj=var)
  
  add.constraint(model, x, "=", 5)
  add.constraint(model, usg, "<=", constraint)
  
  solve(model)
  get.objective(model) # optimal objective value
  get.variables(model) # optimal solution of decision variables
  model

return(player_list[as.logical(get.variables(model)),])

}
