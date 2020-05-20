library(tidyverse)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(shinydashboardPlus)

#Exploring player vs player results and point difference

#USING BIG STATS DATA####
#df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)
#write.csv(df, 'vb_matches.csv')
#setwd("~/R/Projects/tidy_tuesday/app")
df <- read.csv('vb_matches.csv')

#need seperate score columns for winning team and losing team
scor <- as.data.frame(str_split_fixed(df$score, ',', 3))
set_1_score <- as.data.frame(str_split_fixed(scor$V1, '-', 2))
set_2_score <- as.data.frame(str_split_fixed(scor$V2, '-', 2))
set_3_score <- as.data.frame(str_split_fixed(scor$V3, '-', 2))
set_1_score$V1 <- as.numeric(as.character(set_1_score$V1))
set_1_score$V2 <- as.numeric(as.character(set_1_score$V2))
set_2_score$V1 <- as.numeric(as.character(set_2_score$V1))
set_2_score$V2 <- as.numeric(as.character(set_2_score$V2))
set_3_score$V1 <- as.numeric(as.character(set_3_score$V1))
set_3_score$V2 <- as.numeric(as.character(set_3_score$V2))
set_1_score$V1[is.na(set_1_score$V1)] <- 0
set_1_score$V2[is.na(set_1_score$V2)] <- 0
set_2_score$V1[is.na(set_2_score$V1)] <- 0
set_2_score$V2[is.na(set_2_score$V2)] <- 0
set_3_score$V1[is.na(set_3_score$V1)] <- 0
set_3_score$V2[is.na(set_3_score$V2)] <- 0
df$w_set_1 <- set_1_score$V1
df$l_set_1 <- set_1_score$V2
df$w_set_2 <- set_2_score$V1
df$l_set_2 <- set_2_score$V2
df$w_set_3 <- set_3_score$V1
df$l_set_3 <- set_3_score$V2
df$date <- as.Date(df$date)
df$month <- month(as.POSIXlt(df$date, format = "%Y-%m-%d"))
df$year_month <- paste0(df$year, df$month)

#drop columns
drop <-
    c(
        'X',
        'w_p1_tot_attacks',
        'w_p1_tot_kills',
        'w_p1_tot_errors',
        'w_p1_tot_hitpct',
        'w_p1_tot_aces',
        'w_p1_tot_serve_errors',
        'w_p1_tot_blocks',
        'w_p1_tot_digs',
        'w_p2_tot_attacks',
        'w_p2_tot_kills',
        'w_p2_tot_errors',
        'w_p2_tot_hitpct',
        'w_p2_tot_aces',
        'w_p2_tot_serve_errors',
        'w_p2_tot_blocks',
        'w_p2_tot_digs',
        'l_p1_tot_attacks',
        'l_p1_tot_kills',
        'l_p1_tot_errors',
        'l_p1_tot_hitpct',
        'l_p1_tot_aces',
        'l_p1_tot_serve_errors',
        'l_p1_tot_blocks',
        'l_p1_tot_digs',
        'l_p2_tot_attacks',
        'l_p2_tot_kills',
        'l_p2_tot_errors',
        'l_p2_tot_hitpct',
        'l_p2_tot_aces',
        'l_p2_tot_serve_errors',
        'l_p2_tot_blocks',
        'l_p2_tot_dig',
        'l_p2_tot_digs',
        'score'
    )

df <- df[, !(names(df) %in% drop)]

#create team names
df$teama <- paste0(df$w_player1, '/', df$w_player2)
df$teamb <- paste0(df$l_player1, '/', df$l_player2)


#Add another column for player for melting
df$wply1 <- df$w_player1
df$wply2 <- df$w_player2
df$lply1 <- df$l_player1
df$lply2 <- df$l_player2

#melt for w_players
pl <-
    reshape2::melt(
        df,
        id.vars = c(
            "circuit",
            "tournament",
            "country",
            'year_month',
            'month',
            "year",
            "date",
            "gender"  ,
            "match_num",
            'teama',
            'teamb',
            "w_p1_birthdate",
            "w_p1_age",
            "w_p1_hgt",
            "w_p1_country",
            "w_p2_birthdate",
            "w_p2_age",
            "w_p2_hgt",
            "w_p2_country",
            "w_rank",
            "l_p1_birthdate"  ,
            "l_p1_age",
            "l_p1_hgt",
            "l_p1_country",
            "l_p2_birthdate",
            "l_p2_age",
            "l_p2_hgt"  ,
            "l_p2_country",
            "l_rank",
            "duration",
            "bracket",
            "round",
            'wply1',
            'wply2',
            'lply1',
            'lply2',
            'w_set_1',
            'l_set_1',
            'w_set_2',
            'l_set_2',
            'w_set_3',
            'l_set_3'
        )
    )


pl$wply1 <- as.character(pl$wply1)
pl$wply2 <- as.character(pl$wply2)
pl$lply1 <- as.character(pl$lply1)
pl$lply2 <- as.character(pl$lply2)
pl$circuit <- as.character(pl$circuit)



#add column wins
pl$w_match <-
    ifelse(pl$variable %in% c('w_player1', 'w_player2'), 1, 0)
pl$l_match <-
    ifelse(pl$variable %in% c('l_player1', 'l_player2'), 1, 0)


#total points from all sets
pl$value_points <-
    ifelse(
        grepl('w_player', pl$variable),
        pl$w_set_1 + pl$w_set_2 + pl$l_set_3,
        pl$w_set_1 + pl$l_set_2 + pl$l_set_3
    )
pl$opponent_points <-
    ifelse(
        grepl('w_player', pl$variable),
        pl$w_set_1 + pl$l_set_2 + pl$l_set_3,
        pl$w_set_1 + pl$w_set_2 + pl$l_set_3
    )

#create new columns to melt one player vs another specific player
pl$opponent_1 <-
    ifelse(pl$variable == 'w_player1' |
               pl$variable == 'w_player2',
           pl$lply1,
           pl$wply1)
pl$opponent_2 <-
    ifelse(pl$variable == 'w_player1' |
               pl$variable == 'w_player2',
           pl$lply2,
           pl$wply2)

#rename columns
pl <- pl %>% rename(variable_1 = variable, value_1 = value)

#melt again for l_players
pl <-
    reshape2::melt(
        pl,
        id.vars = c(
            'circuit',
            'tournament',
            'country',
            'year_month',
            'month',
            'year',
            'date',
            'gender',
            'match_num',
            'teama',
            'teamb',
            'w_p1_birthdate',
            'w_p1_age',
            'w_p1_hgt',
            'w_p1_country',
            'w_p2_birthdate',
            'w_p2_age',
            'w_p2_hgt',
            'w_p2_country',
            'w_rank',
            'l_p1_birthdate',
            'l_p1_age',
            'l_p1_hgt',
            'l_p1_country',
            'l_p2_birthdate',
            'l_p2_age',
            'l_p2_hgt',
            'l_p2_country',
            'l_rank',
            'duration',
            'bracket',
            'round',
            'wply1',
            'wply2',
            'lply1',
            'lply2',
            'w_set_1',
            'l_set_1',
            'w_set_2',
            'l_set_2',
            'w_set_3',
            'l_set_3',
            'variable_1',
            'value_1',
            'w_match',
            'l_match',
            'value_points',
            'opponent_points'
        )
    )


pl <- pl %>% rename(Opponent_Player = value)

#shiny time!
body <- dashboardBody(
    column(
        3,
        selectInput(
            'Select1',
            'Player',
            choices = sort(unique(pl$value_1)),
            selected = 'Kerri Walsh Jennings'
        )
    ),
    column(3,
           sliderInput("slider", "# of matches", 1, 60, 1)),
    column(
        3,
        awesomeCheckboxGroup(
            'Select2',
            'Circut',
            inline = T,
            status = 'warning',
            choices = sort(unique(pl$circuit)),
            selected = c('AVP', 'FIVB')
        )
    ),
    
    
    fluidRow(
        title = "Win % vs players",
        solidHeader = TRUE,
        fluidRow(column(12,
                        plotOutput("plot1")),
                 fluidRow(
                     column(12,
                            DT::dataTableOutput('table1'), style = "overflow-x: scroll;")
                 ))
    )
)

shinyApp(ui <-
             dashboardPage(dashboardHeader(),
                           dashboardSidebar(collapsed = T),
                           body),
         
         server <- function(input, output) {
             output$plot1 <- renderPlot({
                 pl %>%
                     filter(value_1 == input$Select1, circuit %in% input$Select2) %>%
                     group_by(Opponent_Player) %>%
                     summarise(
                         Total_Matches = sum(w_match) + sum(l_match),
                         won = sum(w_match),
                         lost = sum(l_match),
                         winpercent = sprintf("%0.2f%%", sum(w_match) / Total_Matches),
                         total_points = sum(value_points),
                         opponent_points = sum(opponent_points)
                     ) %>%
                     dplyr::filter(Total_Matches > input$slider) %>%
                     ggplot(aes(Opponent_Player, group = 1)) +
                     geom_line(aes(y = winpercent), color = "grey", size = 1.5) +
                     geom_point(
                         aes(y = winpercent),
                         shape = 21,
                         color = "black",
                         fill = "#69b3a2",
                         size = 6,
                         alpha = 0.4
                     ) +
                     theme(text = element_text(size = 20)) +
                     theme(
                         legend.title = element_blank(),
                         legend.position = "bottom",
                         axis.text.x = element_text(angle = 45, hjust = 1),
                         plot.title = element_text(hjust = 0.5),
                         plot.subtitle = element_text(hjust = 0.5)
                     ) + theme(axis.ticks.length = unit(.25, "cm")) +
                     labs(x = 'Opponent Player', y = 'Win %')
             })
             
             output$table1 <- DT::renderDataTable({
                 DT::datatable(
                     pl %>%
                         filter(value_1 == input$Select1, circuit %in% input$Select2) %>%
                         group_by(Opponent_Player) %>%
                         summarise(
                             Total_Matches = sum(w_match) + sum(l_match),
                             'Matches Won' = sum(w_match),
                             'Matches Lost' = sum(l_match),
                             'Win %' = sprintf("%0.2f%%", sum(w_match) / Total_Matches),
                             'Points Earned' = sum(value_points),
                             'Points Lost' = sum(opponent_points),
                             'Points Diff' = sum(value_points) - sum(opponent_points)
                         ) %>%
                         filter(Total_Matches > input$slider),
                     class = 'disply nowrap compact',
                     rownames = FALSE,
                     options = list(
                         lengthChange = FALSE,
                         columnDefs = list(list(
                             className = 'dt-center', targets = "_all"
                         )),
                         dom  = 'tflp',
                         stateSave = FALSE
                     )
                 )
             })
         })
