# libs
install.packages("rvest", "dplyr")
require(dplyr)
require(rvest)
require(tidyr)
require(ggplot2)
require(plotly)
require(RColorBrewer)


# global config
options(stringsAsFactors = FALSE)

# scrape data
i <- 0
for (i in 2010:2018) {
  
  df <- data.frame(
  )
  url_base <- paste("http://en.fortunaliga.cz/rozpis-zapasu/", i, "?type=2&month=0&round=0", sep = "")
  dt <- url_base %>% read_html() %>% html_nodes(xpath='//*/table[1]') %>% html_table(fill=T)
  df <- data.frame(bind_rows(dt[[2]]))
  
  # clean data
  df1 <- df[grep(paste(i-1, "|", i, sep = ""),df$NA.), c(1,3,5,7)]
  colnames(df1) <- c(
    "date", 
    "homeTeam", 
    "result", 
    "awayTeam"
  )
  df1 <- extract(df1, homeTeam, into = c('homeTeam', 'homeTeamAbbr'), regex = '(.*)\\s+([[:alnum:]]+)$')
  df1 <- extract(df1, awayTeam, into = c('awayTeam', 'awayTeamAbbr'), regex = '(.*)\\s+([[:alnum:]]+)$')
  df1 <- extract(df1, result, into = c('homeGoals', 'awayGoals'), regex = '(.*):([[:digit:]]{1,2})$')
  df1$date <- gsub(".*([[:digit:]]{2}\\/[[:digit:]]{2}\\/[[:digit:]]{4})","\\1", df1$date)
  df1$date <- as.Date(df1$date, "%d/%m/%Y", tz = "Europe/Prague")
  a <- NULL
  for(j in 1:30) a <- c(a,c(rep(j,8)))
  df1$matchDay <- a
  assign(paste("df", i, sep = ""), df1)
  
}

# calculate points for a given season and match day
getCurrentTable <- function(s, r) {
  
  # subset season
  df <- get(paste("df", s, sep = ""))
  # subset current round
  d <- subset(df, matchDay <= r)
  
  # get teams
  teams <- levels(as.factor(droplevels(df)$homeTeam)) 
  
  # make empty table
  t <- vector("numeric", length(teams))
  t[1:length(teams)] <- 0
  names(t) <- teams
  
  # points logic
  for(i in 1:nrow(d)) {
    
    # home draw
    if(d[i,]$homeGoals == d[i,]$awayGoals) {
      # each get 1 point
      t[paste(d[i,]$homeTeam)] <- t[paste(d[i,]$homeTeam)] + 1
      t[paste(d[i,]$awayTeam)] <- t[paste(d[i,]$awayTeam)] + 1
    }
    # home win
    if(d[i,]$homeGoals > d[i,]$awayGoals) {
      # home team gets 3 points
      t[paste(d[i,]$homeTeam)] <- t[paste(d[i,]$homeTeam)] + 3
    }    
    # away win
    if(d[i,]$homeGoals < d[i,]$awayGoals) {
      # away team gets 3 points
      t[paste(d[i,]$awayTeam)] <- t[paste(d[i,]$awayTeam)] + 3
    }  
  }
  
  # print table
  ts <- sort(t)
  print(ts)
  
}    

# calculate season progress compared to match day point average
getProgressTable <- function(season) {
  dft <- data.frame()
  for (i in 1:30) {
    tbl <- getCurrentTable(season,i)
    tmp <- data.frame(name=names(tbl), pts=tbl, round=i, avg=mean(tbl), row.names=NULL)
    dft <- rbind(dft, tmp)
  }
  dft$diff <- dft$pts - dft$avg
  return(dft)
}

# aggregate season progress into single view
for (i in 2010:2018) {
  dft <- getProgressTable(i)
  assign(paste("dfprog", i, sep = ""), dft)
}

# aggregate all seasons into single view
dfprogX <- data.frame()
for (i in 2010:2018) {
  df <- get(paste("dfprog", i, sep = ""))
  df$season <- i
  dfprogX <- rbind(dfprogX, df)
}

# visualization

drawTeamProg <- function(team) {
  cols <- c(rep("#999999",28))
  teamPos <- match(team,levels(as.factor(dfprogX$name)))
  cols[teamPos] <- "#E69F00"
  ggplot(dfprogX, 
         aes(x=round, y=diff, group=name)) + 
    geom_line(aes(color=name))  +
    facet_wrap(~season) +
    labs(title = paste("Czech Football League Points Progress: ", team, sep = "")) +
    scale_color_manual(values = cols) +
    theme(legend.position="bottom") + 
    ylab("diff to points average")
}

drawTeamProgSeason <- function(season, team = NULL) {
  cols <- c(rep("#999999",16))

  g <- ggplot(data=get(paste("dfprog", season, sep = "")), aes(x=round, y=diff, group=name)) +
    geom_line(aes(color=name)) +
    labs(title = paste("Czech Football League Points Progress: ", if(!is.null(team)) team else "Top/Bottom 4 teams" , " in ", season, sep = "")) + 
    ylab("diff to points average")
  if (!is.null(team)) {
    teamPos <- match(team,levels(as.factor(get(paste("dfprog", season, sep = ""))$name)))
    cols[teamPos] <- "#E69F00"
    g + scale_color_manual(values=cols)
  } else {
    # TODO: highlight only top and bottom 4 teams str(dfprog2010[dfprog2010$round == 30,]$name)
    j <- 0
    for (i in get(paste("dfprog", season, sep = ""))[get(paste("dfprog", season, sep = ""))$round == 30,]$name[c(1:4, 13:16)]) {
      teamPos <- match(i,levels(as.factor(get(paste("dfprog", season, sep = ""))$name)))
      cols[teamPos] <- brewer.pal(8,"Set3")[j+1]
      j <- j+1
    }
    # color brewer
    g + scale_color_manual(values = cols)
  }
}


# demo
drawTeamProg("SK Slavia Praha")
drawTeamProg("FC Slovan Liberec")
drawTeamProg("AC Sparta Praha")
drawTeamProgSeason("2018")
drawTeamProgSeason("2016", "SK Slavia Praha")

# plotly
ggplotly(drawTeamProgSeason("2017"))
