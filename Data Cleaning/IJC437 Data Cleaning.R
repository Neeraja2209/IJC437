#Installing necessary packages
install.packages("dplyr")
install.packages("readr")
install.packages("stringr")
install.packages("tydyr")
install.packages("lubridate")

#Loading packages
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(lubridate)

# Reading artists csv file
artist<- read_delim("artists.csv", delim = "\t", col_names = FALSE)

# Assigning column names 
colnames(artist) <- c("Artist_ID", "Artist", "Followers", "Popularity", "Role", "Genre", "Genre_List")

# Renaming by index
colnames(artist)[8] <- "Image_url"

# Removing the first row as those are column headers
artist <- artist[-1, ]

# Cleaning text fields
artist <- artist %>%
  mutate(
    Artist = str_to_lower(str_trim(Artist)),
    Role   = str_to_lower(str_trim(Role)),
    Genre  = str_to_lower(str_trim(Genre)),
    Genre_List = str_replace_all(Genre_List, "\\[|\\]|'|\"", "") %>% str_trim()
  )

# Assigning datatype to numeric fields
artist <- artist %>%
  mutate(
    Followers  = as.numeric(Followers),
    Popularity = as.numeric(Popularity)
  )

# Handling duplicates
artist <- artist %>%
  distinct(Artist_ID, .keep_all = TRUE)

# Reading song chart csv file
song_chart<- read_delim("song_chart.csv", delim = "\t", col_names = FALSE)

# Assigning column names
colnames(song_chart) <- c("Song_ID", "Rank_Score", "Peak_Position", "Weeks_on_Chart", "Week")

# Removing first row as those are column headers
song_chart <- song_chart[-1, ]

# Assigning datatypes for numerical fields
song_chart <- song_chart %>%
  mutate(
    Rank_Score  = as.numeric(Rank_Score),
    Peak_Position = as.numeric(Peak_Position),
    Weeks_on_Chart = as.numeric(Weeks_on_Chart)
  )

song_chart <- song_chart %>%
  mutate(
    Week  = as.character(Week))

# Removing duplicates
song_chart <- song_chart %>%
  distinct(Song_ID, .keep_all = TRUE)


# Reading acoustic features csv file as audio_features
audio_features<- read_delim("acoustic_features.csv", delim = "\t", col_names = FALSE)

# Assigning column names
colnames(audio_features) <- c("Song_ID", "Duration_ms", "Key", "Mode", "Time_Signature", "Acousticness", "Danceability", "Energy", "Instrumentalness", "Liveness", "Loudness", "Speechiness", "Valence", "Tempo")

# Removing first row as those are column headers
audio_features <- audio_features[-1, ]

# Assigning datatypes
audio_features <- audio_features %>%
  mutate(
    Duration_ms= as.numeric(Duration_ms),
    Key = as.numeric(Key),
    Mode = as.numeric(Mode),
    Time_Signature = as.numeric(Time_Signature),
    Acousticness  = as.numeric(Acousticness),
    Danceability  = as.numeric(Danceability),
    Energy  = as.numeric(Energy),
    Instrumentalness  = as.numeric(Instrumentalness),
    Liveness  = as.numeric(Liveness),
    Loudness  = as.numeric(Loudness),
    Speechiness  = as.numeric(Speechiness),
    Valence  = as.numeric(Valence),
    Tempo  = as.numeric(Tempo)
  )

cols_to_fix <- c("Instrumentalness", "Acousticness", "Liveness", "Danceability", "Energy", "Loudness", "Speechiness", "Valence", "Tempo")

# To maintain unified representation and notation of the audio features
audio_features <- audio_features %>%
  mutate(across(all_of(cols_to_fix),
                ~ as.numeric(str_replace(., "^'", ""))))

audio_features %>% mutate(across(where(is.numeric), ~ format(., scientific = FALSE)))


# Reading releases csv file
releases<- read_delim("releases.csv", delim = "\t", col_names = FALSE)

# Assigning column names
colnames(releases) <- c("Artist_ID", "Album_ID", "Release_Date", "Release_Date_Precision")

# Removing first row as those are column headers
releases <- releases[-1, ]

#creating a variable mapping Portuguese month names to English names
month_map <- c(
  "jan" = "Jan", "fev" = "Feb", "mar" = "Mar", "abr" = "Apr",
  "mai" = "May", "jun" = "Jun", "jul" = "Jul", "ago" = "Aug",
  "set" = "Sep", "out" = "Oct", "nov" = "Nov", "dez" = "Dec"
)

#Replacing Portuguese Release Dates with English and making sure all dates follow the same format
releases <- releases %>%
  mutate(
    release_date_clean = Release_Date %>%
      # Making lowercase
      tolower() %>% 
      # Replacing Portuguese to English
      str_replace_all(month_map) %>% 
      # Removing extra spaces
      str_replace_all("\\s+", "") %>% 
      # Unifying separators
      str_replace_all("/", "-")               
  )

# Detecting patterns to recognize if the Release Date value is year only, month & year, or full date
releases <- releases %>%
  mutate(
    year_only  = str_detect(release_date_clean, "^\\d{4}$"),
    month_year = str_detect(release_date_clean, "^[A-Za-z]{3}-\\d{2}$"),
    full_date  = str_detect(release_date_clean, "^\\d{4}-\\d{1,2}-\\d{1,2}$")
  )

# Parsing
releases <- releases %>%
  mutate(
    parsed = case_when(
      full_date ~ ymd(release_date_clean),
      
      month_year ~ {
        mm_str <- str_sub(release_date_clean, 1, 3)
        yy     <- as.numeric(str_sub(release_date_clean, -2))
        
        yyyy <- if_else(yy >= 50, 1900 + yy, 2000 + yy)
        
        my(str_c(mm_str, "-", yyyy))
      },
      
      TRUE ~ NA_Date_
    )
  )

# Extracting components based on the parsed data
releases <- releases %>%
  mutate(
    year  = if_else(year_only, as.numeric(release_date_clean), year(parsed)),
    month = if_else(month_year | full_date, month(parsed), NA_real_),
    day   = if_else(full_date, day(parsed), NA_real_)
  )

# building ISO String
releases <- releases %>%
  mutate(
    release_date_iso = case_when(
      !is.na(day)   ~ sprintf("%04d-%02d-%02d", year, month, day),
      !is.na(month) ~ sprintf("%04d-%02d", year, month),
      TRUE          ~ sprintf("%04d", year)
    )
  )

#Setting release date precision (there were data discrepancies in the original column)
releases <- releases %>%
  mutate(
    release_date_precision = case_when(
      full_date  ~ "day",
      month_year ~ "month",
      year_only  ~ "year",
      TRUE       ~ NA_character_
    )
  )


# Removing unnecessary columns
releases <- releases %>%
  select(-Release_Date, - Release_Date_Precision, -year_only, -month_year, -full_date,-parsed, -year, -day, -month, -release_date_clean)

#Renaming columns
releases <- releases %>%
  rename(
    Release_Date = release_date_iso,
    Release_Date_Precision = release_date_precision
  )


# Reading tracks csv file
tracks<- read_delim("tracks.csv", delim = "\t", col_names = FALSE)

# Assigning column names
colnames(tracks) <- c("Song_ID", "Album_ID","Track_Number", "Release_Date", "Release_Date_Precision")

# Removing first row as those are column headers
tracks <- tracks[-1, ]

# Reading songs csv file
songs<- read_delim("songs.csv", delim = "\t", col_names = FALSE)

# Assigning column names
colnames(songs) <- c("Song_ID", "Song_Name","Billboard", "Artists", "Popularity", "Explicit", "Song_Type")

# Removing first row as those are column headers
songs <- songs[-1, ]

# Assigning datatypes
songs<- songs %>%
  mutate(
    Explicit  = as.logical(Explicit),
    Popularity = as.numeric(Popularity)
  )

#Merging tables
merged_dataset <-  audio_features%>%
  left_join(song_chart, by = "Song_ID")

merged_dataset <-  merged_dataset%>%
  left_join(songs, by = "Song_ID")

merged_dataset <-  merged_dataset%>%
  left_join(tracks, by = "Song_ID") 

merged_dataset <-  merged_dataset%>%
  left_join(releases, by = "Album_ID")

merged_dataset <-  merged_dataset%>% 
  left_join(artist, by = "Artist_ID")

#Reordering Columns(for better readability and understanding)
music_dataset <- merged_dataset [, c("Artist_ID", "Album_ID","Release_Date.y",
                                     "Release_Date_Precision.y","Artist","Followers",
                                     "Popularity.y", "Role","Genre", "Genre_List", 
                                     "Song_ID", "Track_Number", "Song_Name","Billboard",
                                     "Artists", "Popularity.x", "Explicit",
                                     "Song_Type", "Duration_ms","Key","Mode",
                                     "Time_Signature","Acousticness",
                                     "Danceability","Energy", "Instrumentalness",
                                     "Liveness","Loudness","Speechiness","Valence",
                                     "Tempo","Rank_Score","Peak_Position", 
                                     "Weeks_on_Chart")]

# Renaming columns(To make it easier to identify which column comes from which table)
music_dataset <- music_dataset %>% rename(
  artist_id = Artist_ID, album_id = Album_ID, 
  release_date = Release_Date.y,
  release_date_precision = Release_Date_Precision.y,
  artists.name = Artist, artist.followers = Followers, 
  artist.popularity = Popularity.y, artists.role = Role, 
  artist.genre = Genre, artist.genre_list = Genre_List, 
  song_id = Song_ID, tracks.track_number = Track_Number, 
  songs.songs_name = Song_Name, songs.billboard = Billboard,
  songs.artists = Artists, songs.popularity = Popularity.x,
  songs.explicit = Explicit, songs.song_type = Song_Type,
  audio_features.duration_ms = Duration_ms, 
  audio_features.key = Key, audio_features.mode = Mode, 
  audio_features.time_signature = Time_Signature, 
  audio_features.acousticness = Acousticness, 
  audio_features.danceability = Danceability, 
  audio_features.energy = Energy, 
  audio_features.instrumentalness = Instrumentalness,
  audio_features.liveness = Liveness,
  audio_features.loudness = Loudness, 
  audio_features.speechiness = Speechiness, 
  audio_features.valence = Valence, 
  audio_features.tempo = Tempo, 
  song_chart.rank_score = Rank_Score, 
  song_chart.peak_position = Peak_Position,
  song_chart.weeks_on_chart = Weeks_on_Chart
   )

#downloading the merged music dataset
write.csv(music_dataset, "Music Dataset.csv", row.names = FALSE)


########################################################################################

