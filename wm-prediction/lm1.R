#!/usr/local/bin/Rscript

# read in raw files
tm_raw <- read.csv("/Users/pb/code/datalab-playground/wm-prediction/data/tm.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE)
games2014_raw <- read.csv("/Users/pb/code/datalab-playground/wm-prediction/data/games2014.csv", header = FALSE, sep = ",", quote = "\"", dec = ",", fill = TRUE)

# create clean tm data frame
tm <- as.data.frame(cbind(as.character(tm_raw$team_a),
                          as.character(tm_raw$team_b),
                          as.character(tm_raw$wettbewerb),
                          as.character(tm_raw$phase),
                          as.numeric(as.numeric(tm_raw$tore_a) - as.numeric(tm_raw$tore_b)),
                          as.numeric(as.character(format(as.Date(tm_raw$datum, "%d.%m.%Y"),
                                                         "%Y")))))

# combine tm with it's team-reverse copy, so we can group 2 teams later in any order
tmp <- as.data.frame(cbind(as.character(tm_raw$team_b),
                           as.character(tm_raw$team_a),
                           as.character(tm_raw$wettbewerb),
                           as.character(tm_raw$phase),
                           as.numeric(as.numeric(tm_raw$tore_b) - as.numeric(tm_raw$tore_a)),
                           as.numeric(as.character(format(as.Date(tm_raw$datum, "%d.%m.%Y"),
                                                          "%Y")))))

tm <- as.data.frame(rbind(tm, tmp))

# name columns properly
colnames(tm) <- c("team_a", "team_b", "cup", "phase", "diff", "year")

# get rid of factors
tm$team_a <- as.character(tm$team_a)
tm$team_b <- as.character(tm$team_b)
tm$cup <- as.character(tm$cup)
tm$phase <- as.character(tm$phase)
tm$diff <- as.numeric(as.character(tm$diff))
tm$year <- as.numeric(as.character(tm$year))

# standardise cup names (get rid of years)
tm$cup <- gsub("\\d+", "", tm$cup)

# remove invalid / non-useful rows
tm <- tm[!is.na(tm$year), ]
tm <- tm[tm$phase != "", ]

# collect weighted cups
# Weights are assigned depending on the importance of a cup, with WC as maximum
cups <- as.matrix(sort(unique(tm$cup)))
cups <- as.data.frame(cbind(cups, c(50, 50, 50,
                                    10, 10, 50,
                                    10, 10, 50,
                                    50, 50, 10,
                                    10, 10, 10,
                                    10, 100, 10,
                                    10, 10, 10,
                                    10, 10, 10,
                                    10, 10, 10,
                                    10, 10, 1000)))
colnames(cups) <- c("cup", "weight")

# collect weighted cup phases
# Weights are assigned depending on how close to the finals a phase is, with finals as
# maximum
phases <- sort(unique(tm$phase))
phases <- as.matrix(phases)
phases <- as.data.frame(cbind(phases, c(5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9,
                                        1, 1, 1, 1, 1, 1, 1, 5, 9, 9, 1, 1, 1, 1, 1, 1,
                                        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 7, 1, 1, 8, 4, 6,
                                        2, 9, 1, 1, 1, 1, 6, 7)))
colnames(phases) <- c("phase", "weight")

# create weight codes for match years. The logic of recoding is: the newer the game, the more
# relevant it is for the tournament to be predicted. Teams that have been recently good,
# are still likely to be good next time, though there are cycles in team "goodness",
# for example when new talents are yet to be developed while old stars are about to
# quit.
# Coding is done by giving a more recent year a higher weight, so 2013 has a higher
# weight than 1966
tm$year_weight <- tm$year - min(tm$year)

# create weight codes for cups / phases. By multiplying cup and phase weights,
# the magnitude (importance) of the cup is being kept and just enlarged by
# how far a team has come in the particular tournament.
tm$game_weight <- as.numeric(as.character(cups$weight[match(tm$cup, cups$cup)])) *
                  as.numeric(as.character(phases$weight[match(tm$phase, phases$phase)]))

# get rid of obsolete columns
tm$year <- NULL
tm$cup <- NULL
tm$phase <- NULL

# split tm data frame by teams A and B, so we can look up pair statistics quickly
splits <- split(tm, list(tm$team_a, tm$team_b))

# create clean 2014 games data frame
# Pairs are concatinated for auick lookup in the data frame with splits
colnames(games2014_raw) <- c("team_a", "team_b")
games2014 <- paste(games2014_raw$team_a, paste(".", games2014_raw$team_b, sep=""), sep="")

# --- not finished yet, just a test ---
# test a linear model for a pair
# --- a hierarchical, step-wise regression is needed to calculate most likely
# group outcomes and all the further stages up to the finals
# Alternatively a bayesian network can be considered.
df_bm <- as.data.frame(splits[games2014[3]])
colnames(df_bm) <- c("_1", "_2", "diff", "year_weight", "game_weight")
l_bm <- lm(diff ~ game_weight + year_weight, data=df_bm)
summary(l_bm)

# predict Brasil:Mexico, group game, world championship 2014
test <- data.frame(game_weight = 1000, year_weight = (2014 - 1930))
predict(l_bm, test)

# --- predicted result is positive (.58), which indicates that Brasil will win
# There are multiple issues in the model: p-value is high (.26) - there are
# not sufficient recent statistics of adequate games of these teams.
# A much better, improved model could generalise continent-wise (Brasil:Mexico+USA+Canada)

plot(l_bm)
