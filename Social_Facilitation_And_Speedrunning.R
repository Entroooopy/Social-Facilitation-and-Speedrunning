#make code available on github
#see it in repository - start with messy version, then clean up in next version
#once get import working, send to Amandou

library(rvest) #was to load in data, may not need anymore
library(jsonlite) #to load in data
library(dplyr) #to load in data
library(ggplot2) #for pretty graphs
library(car) #for variance inflation checking

#functions for easy re-running, copied to the top
view <- function(x) {
  View(x)
}
import <- function(game_id, category_id = NULL) {
  # Get game info to find categories
  base_url <- "https://www.speedrun.com/api/v1" #this is the base api link that we concat the rest onto
  
  # If no category specified, get the first one
  if (is.null(category_id)) {
    categories <- fromJSON(paste0(base_url, "/games/", game_id, "/categories"))
    category_id <- categories$data$id[1]
  } #i tried specifying categories and it didn't work, so I went with the first one / the default for all the games
  
  # Get scores
  leaderboard_url <- paste0(base_url, "/leaderboards/", game_id, 
                            "/category/", category_id, "?embed=players") #this is making the rest of the url
  leaderboard <- fromJSON(leaderboard_url) #this is making the object from the scores it just pulled up
  return(leaderboard$data$runs) #this is returning the df, so we can assign it to something
}
con <- function(game_id, expected_n, variance = 5) {
  leaderboard <- import(game_id)
  jeux_id <- leaderboard$run$game[1]
  cat_id <- leaderboard$run$category[1]
  leaderboard <- leaderboard[leaderboard$run$category == cat_id &
                               leaderboard$run$game == jeux_id,]
  if(nrow(leaderboard) > expected_n + variance | nrow(leaderboard) < expected_n - variance) {
    stop("Unexpected import number! The API may be messed up for this game :(")
  }
  return(leaderboard)
}
new_var <- function (time) {
  
  #average time as ratio of minimum, so that 1 = min, 2 = you took twice as long, etc.
  speed <- time / min(time)
  
  return(speed)
}
number <- function(df) {
  df <- df[order(as.Date(df$run$date)), ] #this orders the df by date
  df$submission_number <- 1:nrow(df) #this creates a new column and assigns a number to it, from 1 to the number of rows
  return(df) #this returns the df so that we can assign it to something!
}
flat <- function(df) {
  nouveau <- data.frame(
    game = df$run$game,
    place = df$place,
    date = df$run$date,
    time = df$run$times$primary_t,
    speed = df$speed,
    total = nrow(df),
    submission_number = df$submission_number
  )
  ids <- sapply(df$run$players, function(x) x$id) #extracting the id's as a vector from within the sub-sub-sub-df
  nouveau$player_id <- ids #creating a new column in 'nouveau' to hold this info
  return(nouveau) #returning nouveau so that we can assign it to something if we want
}
new_plot <- function(df, bind_x, bind_y) {
  picture <- ggplot(df, aes(x = .data[[bind_x]], y = .data[[bind_y]])) + 
    geom_point(alpha = .15) +
    labs(x = bind_x,
         y = bind_y) +
    stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth",
                color = "black")
  return(picture)
}
new_plot_colour <- function(df, bind_x, bind_y, colour) {
  picture <- ggplot(df, aes(x = .data[[bind_x]], y = .data[[bind_y]], colour = .data[[colour]])) + 
    geom_point(alpha = .15) +
    labs(x = bind_x,
         y = bind_y,
         color = colour) +
    stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth",
                color = "black")
  return(picture)
}
LM <- function(dv, iv, iv2 = NA, df) { #make a function
  #assumption checks
  if(is.na(iv2)) {
    plot <- new_plot(df, dv, iv)
    model <- lm(as.formula(paste(dv, "~", iv)), data = df)
  } else if (!is.na(iv2)) {
    plot <- new_plot_colour(df, dv, iv, iv2)
    if(is.numeric(df[[iv]])) {
      if(is.numeric(df[[iv2]])) {
        print(cor.test(df[[iv]], df[[iv2]]))
      }
    }
    model <- lm(as.formula(paste(dv, "~", iv, "+", iv2)), data = df)
    print(vif(model))
  }
  thing <- summary(model) #show regression and output, inc'l MSE
  print(thing)
  mean(thing$residuals^2)
  return(plot)
}
all <- function(link, expected_n, variance = 5) {
  the_data <- NA
  the_data <- con(link, expected_n, variance)
  if(the_data$run$game[1] %in% df$game) {
    stop("You already imported this game, silly!")
  }
  the_data$speed <- new_var(the_data$run$times$primary_t)
  the_data <- number(the_data)
  the_data <- flat(the_data)
  if(the_data[1, "total"] < 2) {
    stop("Game has too few submissions to be included!")
  }
  df <<- rbind(df, the_data)
  #return(the_data) #commented out so it doesn't return when I'm doing it to all the games
}
tout <- function(link, expected_n, variance = 5) {
  the_data <- NA
  the_data <- con(link, expected_n, variance)
  if(the_data$run$game[1] %in% df2$game) {
    stop("You already imported this game, silly!")
  }
  the_data$speed <- new_var(the_data$run$times$primary_t)
  the_data <- number(the_data)
  the_data <- flat(the_data)
  if(the_data[1, "total"] < 2) {
    stop("Game has too few submissions to be included!")
  }
  df2 <<- rbind(df2, the_data)
  #return(the_data) #commented out so it doesn't return when I'm doing it to all the games
}
tous <- function(link, expected_n, variance = 5) {
  the_data <- NA
  the_data <- con(link, expected_n, variance)
  if(the_data$run$game[1] %in% df3$game) {
    stop("You already imported this game, silly!")
  }
  the_data$speed <- new_var(the_data$run$times$primary_t)
  the_data <- number(the_data)
  the_data <- flat(the_data)
  if(the_data[1, "total"] < 2) {
    stop("Game has too few submissions to be included!")
  }
  df3 <<- rbind(df3, the_data)
  #return(the_data) #commented out so it doesn't return when I'm doing it to all the games
}

#make lower-case view because i keep forgetting to capitalize it
view <- function(x) {
  View(x)
}

#create a function to import data
#comment it out because it doesn't work for JAVA websites
#import <- function(link) {
#  wiki <- read_html(link)
#  tables <- html_nodes(wiki, "table")
#  html_table(tables[[1]], fill = TRUE)
#}
#I tried to do this with rvest, but apparently Speedrun.com is JAVA, which requires jsonlite / dplyr
#I used claude to troubleshoot errors and help me import with these since the textbooks only covered rvest

#have claude create an import function that works on JAVA sites
import <- function(game_id, category_id = NULL) {
  # Get game info to find categories
  base_url <- "https://www.speedrun.com/api/v1" #this is the base api link that we concat the rest onto
  
  # If no category specified, get the first one
  if (is.null(category_id)) {
    categories <- fromJSON(paste0(base_url, "/games/", game_id, "/categories"))
    category_id <- categories$data$id[1]
  } #i tried specifying categories and it didn't work, so I went with the first one / the default for all the games
  
  # Get scores
  leaderboard_url <- paste0(base_url, "/leaderboards/", game_id, 
                            "/category/", category_id, "?embed=players") #this is making the rest of the url
  leaderboard <- fromJSON(leaderboard_url) #this is making the object from the scores it just pulled up
  return(leaderboard$data$runs) #this is returning the df, so we can assign it to something
}

#fix import function so it imports all categories
import_all <- function(game_id) {
  
  base_url <- "https://www.speedrun.com/api/v1"
  
  # Get all categories for the game
  tryCatch({
    categories <- fromJSON(paste0(base_url, "/games/", game_id, "/categories"))
    category_ids <- categories$data$id
    
    # Create an empty list to store all runs
    all_runs <- list()
    
    # Loop through each category
    for (i in seq_along(category_ids)) {
      cat("Fetching category", i, "of", length(category_ids), "\n")
      
      leaderboard_url <- paste0(base_url, "/leaderboards/", game_id, 
                                "/category/", category_ids[i], 
                                "?embed=players")
      
      tryCatch({
        leaderboard <- fromJSON(leaderboard_url)
        runs <- leaderboard$data$runs
        
        # Add category name to the data
        runs$category_name <- categories$data$name[i]
        runs$category_id <- category_ids[i]
        
        all_runs[[i]] <- runs
      }, error = function(e) {
        cat("Error with category", category_ids[i], ":", e$message, "\n")
      })
      
      # Small delay to avoid rate limiting
      Sys.sleep(0.5)
    }
    
    # Use bind_rows instead of rbind - it handles different columns
    combined <- bind_rows(all_runs)
    return(combined)
    
  }, error = function(e) {
    stop("Error fetching game data. Check game_id: ", game_id, "\nError: ", e$message)
  })
}

#now import some data
uno_wii <- import("uno_wii")
#View(uno_wii)
#commented out so it doesn't pop open every time I re-run this


#Chloe said: Just throw them out!
#get game id, if doesn't match for all instances of the game, remove them when "flat"
#just defensive programming
#consistency calls the import function, so run import function to have it loaded
#but don't run it in "all" function or it messes up
#con as in consistency
con <- function(game_id, expected_n, variance = 5) {
  leaderboard <- import(game_id)
  jeux_id <- leaderboard$run$game[1]
  cat_id <- leaderboard$run$category[1]
  leaderboard <- leaderboard[leaderboard$run$category == cat_id &
                               leaderboard$run$game == jeux_id,]
  if(nrow(leaderboard) > expected_n + variance | nrow(leaderboard) < expected_n - variance) {
    stop("Unexpected import number! The API may be messed up for this game :(")
  }
  return(leaderboard)
}
#so now these give the same thing:
#uno_wii <- import("uno_wii")
#uno_wii_c <- con("uno_wii")
#but it's corrected when "game_id" is messed up:
#gm <- import("gm")
#gm_c <- consistancy("gm")
#gm_a <- all("gm")
#nrow(gm)
#nrow(gm_c)
#table(gm$run$category)
#table(gm_c$run$category)
#ok, game id was same for everyone, what about category_id?
#it's the same for all of them! Even though they're different categories!
#if we can't throw them out, invite them all in? So it's at least equal for all games?

#add 2 new arguments to "con" - the expected number in there, and how much variance allowed
#so if you're expecting 10 imports and it gives you 100, it stops the import
#and gives you an error instead

gm <- con("gm", 2424)
gm2 <- con("gm", 750)
#ca marche!!

uno_wii <- con("uno_wii", 10)

#Function to make variables, where:
#time is the variable for the runner's time
#speed is calculated by dividing it by the minimum, a ratio of actual_time:best_time
new_var <- function (time) {
  
  #average time as ratio of minimum, so that 1 = min, 2 = you took twice as long, etc.
  speed <- time / min(time)
  
  return(speed)
}

#now use it
uno_wii$speed <- new_var(uno_wii$run$times$primary_t)

#get total number of entries in each game
uno_wii$total <- nrow(uno_wii)
#View(uno_wii)
#it works!!

#make a function that sees how many submissions there were when they submitted
#including their own submission (the first submission is 1)
#I had a hard time doing this, and my first idea was to use a loop
#like for i in i:nrow(df) {df$number = i}
#which would've been sooooo slow!
#but i had issues when each line in the df was its own df (according to claude)
#and it wasn't reading the date as a date it could order
#and when I tried to get the AI to troubleshoot that part it wrote the function to do the whole thing
#and this solution works faster than a loop would
number <- function(df) {
  df <- df[order(as.Date(df$run$date)), ] #this orders the df by date
  df$submission_number <- 1:nrow(df) #this creates a new column and assigns a number to it, from 1 to the number of rows
  return(df) #this returns the df so that we can assign it to something!
}


#now use it
uno_wii <- number(uno_wii)
#View(uno_wii)

#now do it for another game
tales_of_kenzera_zau <- con("Tales_of_Kenzera_Zau", 2)
tales_of_kenzera_zau$speed <- new_var(tales_of_kenzera_zau$run$times$primary_t)
tales_of_kenzera_zau$total <- nrow(tales_of_kenzera_zau)
tales_of_kenzera_zau <- number(tales_of_kenzera_zau)
#View(tales_of_kenzera_zau) #commented out so it doesn't open every time i run this code

#test it on a game with hundreds of entries
#commented out since not random selection
#peak <- import("PEAK")
#peak$speed <- new_var(peak$run$times$primary_t)
#peak$total <- nrow(peak)
#peak$number <- nrow(peak)
#View(peak)
#still works!

#and then the dataframe we want to add them to
#need to first flatten them so that there isn't df's nested inside each df (using JSONlite flatten)
#uno_wii <- flatten_df(uno_wii, factor_ext_to_char = FALSE)
#tales_of_kenzera_zau <- flatten_df(tales_of_kenzera_zau, factor_ext_to_char = FALSE)
#it didn't work :(
#the different games have a different # of variables?

#or just make it one with variables we care about
#it looks like different games have a different number of columns, so works around that
#I would like to get the runner ID, but that's too far nested into the df for me to access with View
#View(tales_of_kenzera_zau$run$players) works but 
#View(tales_of_kenzera_zau$run$players$id) does not???
#and trying to get Claude to help breaks him #but ChatGPT was able to figure it out!
flat <- function(df) {
  nouveau <- data.frame(
    game = df$run$game,
    place = df$place,
    date = df$run$date,
    time = df$run$times$primary_t,
    speed = df$speed,
    total = nrow(df),
    submission_number = df$submission_number
  )
  ids <- sapply(df$run$players, function(x) x$id) #extracting the id's as a vector from within the sub-sub-sub-df
  nouveau$player_id <- ids #creating a new column in 'nouveau' to hold this info
  return(nouveau) #returning nouveau so that we can assign it to something if we want
}

# and run it:
f_tales_of_kenzera_zau <- flat(tales_of_kenzera_zau)
f_uno_wii <- flat(uno_wii)
#these two games I'm testing it on are coded into new df's for reality checks, but when I put this in the
#"All" function, it replaces the imported data #i don't need that many df's

#start assembling the df
df <- rbind(f_uno_wii, f_tales_of_kenzera_zau)
#the "all" function does this automatically
#and also returns the data so I can check each one in case something's off
#but I commented it out once I got it working so it wouldn't spam the enviorment / output

#maybe make a function to do all of this in one go?
#this function also checks to make sure the game isn't already in the df
#this would cause issues if we were looking at every category for games, but we're only looking at the first
#and checks to see that games has more than 2 submissions to each category / import!
all <- function(link, expected_n, variance = 5) {
  the_data <- NA
  the_data <- con(link, expected_n, variance)
  if(the_data$run$game[1] %in% df$game) {
    stop("You already imported this game, silly!")
  }
  the_data$speed <- new_var(the_data$run$times$primary_t)
  the_data <- number(the_data)
  the_data <- flat(the_data)
  if(the_data[1, "total"] < 2) {
    stop("Game has too few submissions to be included!")
  }
  df <<- rbind(df, the_data)
  #return(the_data) #commented out so it doesn't return when I'm doing it to all the games
}

#test with new game
doctor_who_the_edge_of_time <- all("doctor_who_the_edge_of_time", 2)
#it works!!

#now random sample 100 games!!
#it won't let you view them while it's still running code
#they also won't show up in the "data" panel until the code stops running #if reinstated that
#the ones commented out only return errors I don't understand
#and there is so much data that will work, so I don't care to understand
#I stopped having them double up as their own viewable df, to save some memory / my environment panel
#and that's how i didn't notice that it would sometime import stuff for the entire game until study ii
#the number of expected imports is the number of runs displayed on the leaderboard on 10/14
#all the code worked on 10/15. If it doesn't work in the future, check to see how many
#submissions are in the first category of the problematic game
all("madagascar_kartz", 5)
all("Who_Wants_To_Be_A_Millionaire_1999", 4)
#all("undertale", 378, 50) #API messed up
all("ane-san", 3)
#all("The_Last_of_Us_Part_II_No_Return", 1) #only 1 run in main category, shouldn't have been included
#all("winback_covert_operations") #this one gives me an error?
all("3rd_eye", 3)
all("miodesopsia_whispering_stories", 2) #10 working imports
all("family_trainer_series_4_jogging_race", 3)
all("themehotel", 9)
all("offtherails", 4)
all("depo", 11)
all("messiah", 2)
all("Rec_Paroxysm", 3)
all("mtpo", 122)
all("odell_lake", 2)
all("mgtt", 74)
all("ykm", 4) #20
all("terrain_tourney", 2)
all("the_rodinia_project", 3)
all("hw_brklss", 7)
all("balls", 5)
#all("thps4") #error
all("disneys_goofys_fun_house", 8)
all("hypixel_bw", 692)
all("kdl2", 71)
all("moto_x3m_construction_yard", 5)
all("unhack", 4)
all("fazelux", 3)  #30
all("sifu", 124)
all("the_last_of_us_part_i_category_extensions", 6)
all("The_Death", 3)
#all("beeny") #returns error
all("PBR_Out_of_the_Chute", 2)
all("Life_Vest_App", 14)
all("kirbys_rampage", 13)
all("Dimhaven_Enigmas", 4)
all("gravbo", 24)
all("bloody_wolf", 4)
all("bloodfs", 11)  #40
all("Gob_demo", 2)
all("post_void", 53)
#all("12_Bullets_to_Midnight") #error #:(
all("mto", 15)
all("fopss", 35)
all("dw2_gbc", 4)
all("the_dark_pictures_anthology_the_devil_in_me", 6)
all("sonic_heroes_pc_-_extended_levels_mod", 2)
all("ghost_of_tsushima", 8)
all("pkmnpark2", 28)
all("realm", 3)  #50 - 2 working imports!!!!!!
#all("hexic_hd") #error
all("finding_nemo_pc", 9)
all("utdisbelief", 10)
all("tube_tycoon_memes", 2)
all("sm_ffh_vr", 9)
all("cspeed", 4)
all("ballance", 12)
all("3d_monster_maze", 2)
all("stvef_meowmod", 2)
all("timecrisis", 6)
all("500_CALIBER_CONTRACTZ", 19)  #60
all("Care_Bears_To_The_Rescue_2024", 2)
all("mad_motor", 2)
all("recettear_an_item_shops_tale", 4)
all("uprising_join_or_die", 2)
all("BodyCamera_Shooter", 2)
#all("gun") #error
all("shadowgun", 2)
all("brutal_paws_of_fury", 2)
#all("codm", 23) #api error
all("snowboard_kids_plus", 2)
all("boomerang_woman", 2)  #70
all("Retro_Bowl_College", 5)
all("supercan", 2)
all("yandere_school", 4)
all("rise_and_shine", 9)
all("spyroar", 3)
all("renegade_racing", 9)
all("hwlegends", 3)
all("patrickstar", 8)
all("PEAK_CE", 15)
all("old_mans_journey", 7)  #80
all("nwcnes", 51)
all("hardboiled_chicken", 3)
all("Green_Heights", 5)
all("dreamworld_pogie", 31)
all("brief_karate_foolish", 2)
all("isat", 4)
#all("flatout_2") #error
all("batimce", 4)
all("iready", 18)
all("yugioh_5ds_tag_force_4", 8)
all("greenvideogame", 2) #90
all("the_hitchhikers_guide_to_the_galaxy", 2)
all("nightshade", 2)
all("Invertigo", 4)
all("Tomba_Special_Edition", 4)
all("dearesther", 5)
all("nsnd2", 3)
all("Easyland", 26)
all("spiritual_warfare", 4)
all("mademan", 10)
all("lotrk2", 4) #100 - 3 games successfully imported!
all("Rematch", 16)
all("Dont_Shoot_The_Puppy", 9)
#all("60sbr", 105, 20) #API error
all("Hazelnut_Hex", 2)

#ANALYSIS TIME!!!!
#To see how many submissions / how many unique participants
length(unique(df$player_id)) #number of accounts that submitted data
nrow(df) #total number of submissions

#to see how many games / game categories
games <- table(df$game)
length(games) #number of games
mean(games) #average of submissions per game
sd(games) #sd of submissions per game

#check for outliers in speed, not including everyone who holds the record
hist(df$speed[df$speed > 1])
#there's quite a few!
#outliers are people too #but not good enough speedrunners to be in the analyses
#or maybe they're too good for me!
#At least they tried! That's better than me

#get M and SD of average time (as a ratio)
mean(df$speed)
sd(df$speed)
#and not as a ratio (time, in seconds)
mean(df$time)
sd(df$time)
#and of place
mean(df$place)
sd(df$place)

#create a function for assumption checks via scatterplot
new_plot <- function(df, bind_x, bind_y) {
  picture <- ggplot(df, aes(x = .data[[bind_x]], y = .data[[bind_y]])) + 
    geom_point(alpha = .15) +
    labs(x = bind_x,
         y = bind_y) +
    stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth",
                color = "black")
  return(picture)
}

#and one that lets you change the color to a new variable
new_plot_colour <- function(df, bind_x, bind_y, colour) {
  picture <- ggplot(df, aes(x = .data[[bind_x]], y = .data[[bind_y]], colour = .data[[colour]])) + 
    geom_point(alpha = .15) +
    labs(x = bind_x,
         y = bind_y,
         color = colour) +
    stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth",
                color = "black")
  return(picture)
}


#create a function that combines assumption checks, regression model, and output
LM <- function(dv, iv, iv2 = NA, df) { #make a function
  #assumption checks
  if(is.na(iv2)) {
    plot <- new_plot(df, dv, iv)
    model <- lm(as.formula(paste(dv, "~", iv)), data = df)
  } else if (!is.na(iv2)) {
    plot <- new_plot_colour(df, dv, iv, iv2)
    if(is.numeric(df[[iv]])) {
      if(is.numeric(df[[iv2]])) {
        print(cor.test(df[[iv]], df[[iv2]]))
      }
    }
    model <- lm(as.formula(paste(dv, "~", iv, "+", iv2)), data = df)
    print(vif(model))
  }
  thing <- summary(model) #show regression and output, inc'l MSE
  print(thing)
  mean(thing$residuals^2)
  return(plot)
}
#ca marche!
#you have to scroll up a bit in the output to see the correlation, but it's in a different colour
#so it's fine #it works well enough

#hypothesis 1
LM("speed", "submission_number",, df[df$speed < mean(df$speed) + sd(df$speed) * 3 & 
                                       df$submission_number < mean(df$submission_number) + sd(df$submission_number) *3,])

#hypothesis 2
LM("time", "submission_number",, df[df$time < mean(df$time) + sd(df$time) * 3 &
                                      df$submission_number < mean(df$submission_number) + sd(df$submission_number) *3,])
#ok, so about 2.5 seconds faster per pre-existing datapoint, p < .00000000000000002

#hypothesis 3
LM("place", "submission_number", "total", df[df$place < mean(df$place) + sd(df$place) * 3 &
                                               df$submission_number < mean(df$submission_number) + sd(df$submission_number) *3,])
#ok, but are they more likely to win? with their position (first place, second place, etc) as DV
#i noticed some of the leaderboards had '0' in place (it didn't correspond with first place, sometimes multiple 0's per game)
#they weren't runs that were disqualified, these times were still in the leaderboard when you view it normally
#and I'm not sure what that's about, so I filtered them out
#correlation borderline concerning, I'll see what VIF says
#how is htat variance not inflated???

#make some pretty charts showing data divided by game, for scholar's week
#for h1 (speed)
picture1 <- ggplot(df[df$speed < mean(df$speed) + sd(df$speed) * 3 & 
                        df$submission_number < mean(df$submission_number) + sd(df$submission_number) *3,],
                   aes(x = .data[["speed"]], y = .data[["submission_number"]], colour = .data[["game"]])) + 
  geom_point(alpha = .15) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              color = "black")
picture1

#for h2 (time)
picture2 <- ggplot(df[df$time < mean(df$time) + sd(df$time) * 3 & 
                        df$submission_number < mean(df$submission_number) + sd(df$submission_number) *3,],
                   aes(x = .data[["time"]], y = .data[["submission_number"]], colour = .data[["game"]])) + 
  geom_point(alpha = .15) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              color = "black")
picture2

#for h3
picture3 <- ggplot(df[df$place > 0 & df$place < mean(df$place) + sd(df$place) * 3 & 
                        df$submission_number < mean(df$submission_number) + sd(df$submission_number) *3,],
                   aes(x = .data[["place"]], y = .data[["submission_number"]], colour = .data[["game"]])) + 
  geom_point(alpha = .15) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              color = "black")
picture3


#--------------------------------------------------------------------------------------------------------------------------------------------------


#study ii
#use the same code, but make a new df to rbind to #the first row is all NA
df2 <- data.frame (
  game = NA,
  place = NA,
  date = NA,
  time = NA,
  speed = NA,
  total = NA,
  submission_number = NA,
  player_id = "" #if I put NA here, it makes an id of the string "NA"
)

#this is the same as the "all" function, it's just french now
tout <- function(link, expected_n, variance = 5) {
  the_data <- NA
  the_data <- con(link, expected_n, variance)
  if(the_data$run$game[1] %in% df2$game) {
    stop("You already imported this game, silly!")
  }
  the_data$speed <- new_var(the_data$run$times$primary_t)
  the_data <- number(the_data)
  the_data <- flat(the_data)
  if(the_data[1, "total"] < 2) {
    stop("Game has too few submissions to be included!")
  }
  df2 <<- rbind(df2, the_data)
  #return(the_data) #commented out so it doesn't return when I'm doing it to all the games
}


#now just import data from games, and run the same analyses but on df2
#these are popular game, so I'll allow a variance of 20 submissions in case more
#people submit data
tout("smb", 750, 20)
tout("hypixel_bw", 692, 20) #included in sample for study 1
tout("smbce", 652, 20)
tout("lm", 712, 20)
tout("celestep8", 779, 20)
tout("MKWii", 662, 20)
tout("brawl_stars", 687, 20)
tout("minesweeper", 678, 20) #many share 1st place, 45th place
tout("pkmnredblue", 603, 20)
#tout("cclicker", 649, 20)
#tout("csgo", 631, 20)
#tout("rocketleague", 606, 20)
tout("choppy_orc", 693, 20)

#now remove that NA column #maybe not the best way to make the df, but it works
#if there's an NA in the first column, the regressions don't work for some reason?
if (is.na(df2[1,1])) {
  df2<- df2[2:nrow(df2),1:8]
} else {
  print("Error! You're trying to remove real data!")
}

#other  games that met inclusion criteria but gave errors / inaccurate n's:
#tout("superliminal") #743 runs #but it gives 940
#tout("gm") #636 runs #but it gives me 2424 runs??? #that's as many as my first df!
#tout("bhop_pro") #632 runs #and it gives 1287 runs?
#tout("cuphead") #612 runs #error
#tout("pou") #690 runs #but it gives me 1540 runs
#tout("portal") #731 runs #and an error
#tout("mk8dx") #722 runs #but it gives me 1964 runs
#tout("sm64") #704 runs #error
#tout("island_games", 660, 20) #660, it gives 769

#the rest is the same code as above, but with "df2" replacing "df"
#to make sure that's the case, run it after clearing your environment
#and only loading the functions back in
#so if you refer to "df", it says "what's that??"
#or you can see if something named 'df' is created in the enviorment (outside of a function)

#to see how many games / game categories
games2 <- table(df2$game)
print(games2)
length(games2) #number of games
mean(games2) #average of submissions per game
sd(games2) #sd of submissions per game

length(unique(df2$player_id)) #number of accounts that submitted data
nrow(df2) #total number of submissions

hist(df2$speed[df2$speed > 1])
#there's quite a few!
#outliers are people too #but not good enough speedrunners to be in the analyses
#At least they tried! That's better than me
max(df2$speed)
#that person was probably the person who took 9 hours to speedrun Minesweeper
#when the world record was .01 seconds

#get M and SD of speed (as a ratio)
mean(df2$speed)
sd(df2$speed)
#and not as a ratio (time, in seconds)
mean(df2$time)
sd(df2$time)
#and of place
mean(df2$place)
sd(df2$place)

#hypothesis 1: investigate speed again
LM("speed", "submission_number",, df2[df2$speed < mean(df2$speed) + sd(df2$speed) * 3 &
                                        df2$submission_number < mean(df2$submission_number) + sd(df2$submission_number) * 3,])
#scatterplot: it's linear in the wrong direction!!!!! the data should not be vertical!!!
#how is this model not significant? there's so much data
#more data than there was previously when it was significant
#i think this variable "speed" sucks #why did i make it?

#hypothesis 2
LM("time", "submission_number",, df2[df2$time < mean(df2$time) + sd(df2$time) * 3 &
                                       df2$submission_number < mean(df2$submission_number) + sd(df2$submission_number) * 3,])
#you can see clusters of games in the scatterplot
#regression

#hypothesis 3
LM("place", "submission_number", "total", df2[df2$place < mean(df2$place) + sd(df2$place) * 3 & df2$place > 0 &
                                                df2$submission_number < mean(df2$submission_number) + sd(df2$submission_number) * 3,])
#the scatterplot reminds me of Monet's "Water Lilies"     #why does R not know who Monet is?
#total number of submission still matters?
#the r squared went way down tho (as expected, but i would've like an r2 a little higher than that!)

#hypothesis 4: now looking at the top 50 in those datasets
LM("place", "submission_number", "total", df2[df2$place > 0 & df2$place <= 50 &
                                                df2$submission_number < mean(df2$submission_number) + sd(df2$submission_number) * 3,])
#why would the total number of submissions matter when we're looking at people who placed 1-50?
#but the r squared is still much closer to an expected value! #:(

#reality check the filter


#the spikes in data in first and 45th place were caused by so many people tying
#for first (n=44) and 45th in Minesweeper #since there were 44 first-place scores
#second-place times were all tied for 45th
mean(df2$place[df2$place <= 50 & df2$place > 0  &
                 df2$submission_number < mean(df2$submission_number) + sd(df2$submission_number) * 3])
sd(df2$place[df2$place > 0 & df2$place <= 50  &
               df2$submission_number < mean(df2$submission_number) + sd(df2$submission_number) * 3])

#demographics
nrow(df2[df2$place > 0 & df2$place <= 50  &
           df2$submission_number < mean(df2$submission_number) + sd(df2$submission_number) * 3,])
length(unique(df2$player_id[df2$place > 0 & df2$place <= 50 &
                              df2$submission_number < mean(df2$submission_number) + sd(df2$submission_number) * 3]))


#make some pretty charts showing data divided by game, for scholar's week
#for h1 (speed)
picture21 <- ggplot(df2[df2$speed < mean(df2$speed) + sd(df2$speed) * 3 & 
                          df2$submission_number < mean(df2$submission_number) + sd(df2$submission_number) * 3,],
                    aes(x = .data[["speed"]], y = .data[["submission_number"]], colour = .data[["game"]])) + 
  geom_point(alpha = .15) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              color = "black")
picture21

#for h2 (time)
picture22 <- ggplot(df2[df2$time < mean(df2$time) + sd(df2$time) * 3 & 
                          df2$submission_number < mean(df2$submission_number) + sd(df2$submission_number) *3,],
                    aes(x = .data[["time"]], y = .data[["submission_number"]], colour = .data[["game"]])) + 
  geom_point(alpha = .15) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              color = "black")
picture22

#for h3
picture23 <- ggplot(df2[df2$place > 0 & df2$place < mean(df2$place) + sd(df2$place) * 3 & 
                          df2$submission_number < mean(df2$submission_number) + sd(df2$submission_number) *3,],
                    aes(x = .data[["place"]], y = .data[["submission_number"]], colour = .data[["game"]])) + 
  geom_point(alpha = .15) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              color = "black")
picture23

#for h4
picture24 <- ggplot(df2[df2$place > 0 & df2$place <= 50 & 
                          df2$submission_number < mean(df2$submission_number) + sd(df2$submission_number) *3,],
                    aes(x = .data[["place"]], y = .data[["submission_number"]], colour = .data[["game"]])) + 
  geom_point(alpha = .15) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              color = "black")
picture24

#--------------------------------------------------------------------------------------------------------------------------------------------------





#study iii
#look at top 50 in true randomly sampled dataset
#scraping data is addictive, every time I have a follow up question or thing "future studies should..."
#i already have the import function written #and there's so much data out there to sample

#make version 3 of everything #again, all of this code is copied from studyii and updated
#again, clear environment and just run creation of functions in study 1 to make sure I didn't miss any "df2"s when updating
df3 <- data.frame (
  game = NA,
  place = NA,
  date = NA,
  time = NA,
  speed = NA,
  total = NA,
  submission_number = NA,
  player_id = "" #if I put NA here, it makes an id of the string "NA"
)

#good thing French has more than one word for "all"
tous <- function(link, expected_n, variance = 5) {
  the_data <- NA
  the_data <- con(link, expected_n, variance)
  if(the_data$run$game[1] %in% df3$game) {
    stop("You already imported this game, silly!")
  }
  the_data$speed <- new_var(the_data$run$times$primary_t)
  the_data <- number(the_data)
  the_data <- flat(the_data)
  if(the_data[1, "total"] < 2) {
    stop("Game has too few submissions to be included!")
  }
  df3 <<- rbind(df3, the_data)
  #return(the_data) #commented out so it doesn't return when I'm doing it to all the games
}

#import data
tous("tailsskypatrol", 5)
tous("maldita_castilla_cursed_castilla_ex", 12)
tous("secret_ties", 10)
#tous("pkmnbdsp", 33) #API error
#tous("dadish", 14) #API error
tous("httpswww.roblox.comgames732024737escape-construction-site-obby", 21)
tous("towerfallitchio", 6)
tous("jolly", 3)
tous("groundy_and_skyah", 5)
tous("archery_world_tour", 5)
tous("msp", 5)
tous("Pro_Evolution_Soccer_2018_Category_Extensions", 2)
tous("teddy", 3)
tous("nexomon_extinction", 2)
tous("qq", 6)
#tous("mcbebe", 6) #API error
tous("elf_bowling", 16)
tous("fairy_tail_gekitotsu_kardia_daiseidou", 4)
tous("httpwww.hardcoregaming101.netdark-seal", 3)
tous("strider2", 16)
tous("the_blackout_club", 9)
tous("Chess_2_Deadpool_Retaliation", 5)
tous("bishoujo_senshi_sailor_moon_r_gb", 22)
tous("just_dance_2019", 3)
tous("wwe_wrestlemania_xix", 2)
tous("yugioh_dark_duel_stories", 17)
tous("fernbus_simulator", 3)
tous("a_memoir_blue", 4)
tous("portal_category_extensions", 18)
tous("tlbtbigwater", 4)
tous("ghost_of_a_tale", 5)
tous("sscountdown", 36)
tous("lala_the_magical", 3)
tous("cho_aniki_kyuukyoku_muteki_ginga_saikyou_otoko", 2)
tous("qv", 2)
tous("wwe_2k16", 2)
tous("castlevania_lords_of_shadow_2", 4)
tous("the_demon_rush", 2)
tous("super_ghouls_n_ghosts_gba", 5)
tous("nsml", 6)
tous("cubic_castles", 4)
tous("mandagon", 15)
tous("faw3for", 2)
tous("tear_ring_saga_berwick_saga", 2)
tous("minivania_plus", 2)
#tous("spyrortpc", 126) #API error
tous("nm", 8)
tous("play_with_the_teletubbies", 11)
tous("de2", 12)
tous("geoid", 2)
tous("prison_break_the_conspiracy", 5)
tous("thsc", 523, 20)
tous("iwmtapocalypse", 4)
#tous("drmarion64", 23) #error
tous("breakers_revenge", 2)
tous("mr._chins_gourmet_paradise", 10)
tous("top_spin_4", 2)
tous("100_Waiting_Cats", 3)
tous("michigan_report_from_hell", 4)
tous("twow", 163, 20)
tous("dkcr3d", 5)
tous("moneymovers2", 4)
#tous("cannon_spike", 6) #API error
tous("balloon_kid", 4)
tous("gutwhale", 6)
tous("r_fom", 6)
tous("sm64_red_is_dead", 10)
tous("glow_hockey_2", 17) #is against a computer
tous("bionic_commando_arcade", 4)
tous("jelly_drift_category_extensions", 4)
tous("franklin_the_turtle_great_adventures", 2)
tous("flight", 8)
tous("rage_of_the_gladiator", 3)
tous("the_dig", 8)
tous("alpaca_stacka", 120)
tous("Construction_Simulator_2015", 2)
tous("am2", 3)
tous("hard_truck_apocalypse__ex_machina", 6)
tous("pokeypoke", 5)
tous("ddod", 4)
tous("gulman", 4)
tous("GI_Joe_Cobra_Strike", 2)
tous("dodgeem", 3)
tous("zgi", 4)
tous("siren_head_resurrection", 2)
#tous("cyberpunk_2077", 13) #API error
tous("Super_Karlson_Bro", 2)
tous("koopasrevenge1", 5)
tous("phozon", 2)
tous("super_minecraft_maker", 5)
tous("badgob", 23)
#tous("mkm2", 4) #connection error
#tous("moto_x3m_spookyland", 14) #API error
tous("sm64_new_star", 13)
tous("iwoneforall", 3)
tous("peters_house", 7)
tous("barbie_fashion_show", 3)
tous("cursor_10_2nd", 3)
tous("arkhamoriginsblackgate", 7)
tous("steamboat_mario", 4) #88 games!!!
tous("stfbw", 2)
tous("saint_seiya_ougon_densetsu", 2)
tous("rally_de_africa", 4)
tous("ESCAR", 2)
tous("ford_mustang_the_legend_lives", 3)
tous("ben_bonk_the_ball", 8)
tous("cho", 22)
tous("alcatraz_prison_escape", 2)
tous("smosm", 7)
tous("heathcliff_the_fast_and_the_furriest", 2)
tous("asteroids", 7)
tous("kaizo_marathon", 3)
tous("ab_mcdonalds", 5)

#now remove that NA column
if (is.na(df3[1,1])) {
  df3<- df3[2:nrow(df3),1:8]
} else {
  print("Error! You're trying to remove real data!")
}

games3 <- table(df3$game)
length(games3) #number of games #it's 101 (I sampled 1 extra, oops)
mean(games3) #average of submissions per game
sd(games3) #sd of submissions per game

length(unique(df3$player_id)) #number of accounts that submitted data
nrow(df3) #total number of submissions

hist(df3$speed[df3$speed > 1]) #check for outliers #there will always be outliers

#get M and SD of average time (as a ratio)
mean(df3$speed)
sd(df3$speed)
#and not as a ratio (time, in seconds)
mean(df3$time)
sd(df3$time)
#and of place
mean(df3$place)
sd(df3$place)

#run that regression
#hypothesis 1 #speed
LM("speed", "submission_number",,df3[df3$speed < mean(df3$speed) + sd(df3$speed) * 3 & 
                                       df3$submission_number < mean(df3$submission_number) + sd(df3$submission_number),])

#H2: time
LM("time", "submission_number",, df3[df3$time < mean(df3$time) + sd(df3$time) * 3 & 
                                       df3$submission_number < mean(df3$submission_number) + sd(df3$submission_number),])

#hypothesis 3: place
LM("place", "submission_number", "total", df3[df3$place < mean(df3$place) + sd(df3$place) * 3 & df3$place > 0 & 
                                                df3$submission_number < mean(df3$submission_number) + sd(df3$submission_number),])

#H4: top 50 in place
LM("place", "submission_number", "total", df3[df3$place <= 50 & df3$place > 0 & 
                                                df3$submission_number < mean(df3$submission_number) + sd(df3$submission_number),])


#demographics of the filter
mean(df3$place[df3$place <= 50 & df3$place > 0  &
                 df3$submission_number < mean(df3$submission_number) + sd(df3$submission_number) * 3])
sd(df3$place[df3$place > 0 & df3$place <= 50  &
               df3$submission_number < mean(df3$submission_number) + sd(df3$submission_number) * 3])
nrow(df3[df3$place > 0 & df3$place <= 50  &
           df3$submission_number < mean(df3$submission_number) + sd(df3$submission_number) * 3,])
length(unique(df3$player_id[df3$place > 0 & df3$place <= 50 &
                              df3$submission_number < mean(df3$submission_number) + sd(df3$submission_number) * 3]))
#number of submissions with these filters
#i could have just looked at my degrees of freedom #but I already had this written from study 2




#make some pretty charts showing data divided by game, for scholar's week
#for h1 (speed)
picture31 <- ggplot(df3[df3$speed < mean(df3$speed) + sd(df3$speed) * 3 & 
                          df3$submission_number < mean(df3$submission_number) + sd(df3$submission_number) * 3,],
                    aes(x = .data[["speed"]], y = .data[["submission_number"]], colour = .data[["game"]])) + 
  geom_point(alpha = .15) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              color = "black")
picture31

#for h2 (time)
picture32 <- ggplot(df3[df3$time < mean(df3$time) + sd(df3$time) * 3 & 
                          df3$submission_number < mean(df3$submission_number) + sd(df3$submission_number) *3,],
                    aes(x = .data[["time"]], y = .data[["submission_number"]], colour = .data[["game"]])) + 
  geom_point(alpha = .15) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              color = "black")
picture32

#for h3
picture33 <- ggplot(df3[df3$place > 0 & df3$place < mean(df3$place) + sd(df3$place) * 3 & 
                          df3$submission_number < mean(df3$submission_number) + sd(df3$submission_number) *3,],
                    aes(x = .data[["place"]], y = .data[["submission_number"]], colour = .data[["game"]])) + 
  geom_point(alpha = .15) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              color = "black")
picture33

#for h4
picture34 <- ggplot(df3[df3$place > 0 & df3$place <= 50 & 
                          df3$submission_number < mean(df3$submission_number) + sd(df3$submission_number) *3,],
                    aes(x = .data[["place"]], y = .data[["submission_number"]], colour = .data[["game"]])) + 
  geom_point(alpha = .15) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              color = "black")
picture34