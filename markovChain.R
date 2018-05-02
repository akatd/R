library(quantmod)
library(data.table)
library(caret)

# get the dataset

getSymbols(c("^GSPC"))

# data preprocessing

dates <- index(GSPC)

GSPC <- data.table(GSPC)

GSPC$Close.Date <- dates

# create sequences

new_set <- c() # to store the sequences

nm1 <- c("GSPC.Close", "GSPC.High", "GSPC.Low", "GSPC.Volume")
nm2 <- c("Close_Gap", "High_Gap", "Low_Gap", "Volume_Gap")
dropcols <- c("GSPC.Close", "GSPC.High", "GSPC.Low", "GSPC.Volume", "GSPC.Open", "GSPC.Adjusted", "Close.Date")

for(i in seq(100)){
  
  row_quant <- sample(10:30, 1) # sample length for each sequence (between 10 and 30)
  
  row_start <- sample(1:(nrow(GSPC) - row_quant), 1) # get beginning row for sequence
  
  dat_sub <- GSPC[row_start:(row_start + row_quant),] # create sequence subset of data
  
  dat_sub[, (nm2) := (.SD - shift(.SD, type = "lag"))/shift(.SD, type = "lag"), .SDcols = nm1]
  dat_sub[, Close_Date := max(Close.Date)]
  dat_sub[, Daily_Change := (GSPC.Close - GSPC.Open)/GSPC.Open]
  dat_sub[, Outcome_Next_Day_Direction := shift(GSPC.Volume, type = "lead") - GSPC.Volume]
  dat_sub[, which(names(dat_sub) %in% dropcols) := NULL]
  dat_sub <- na.omit(dat_sub)
  
  dat_sub$seq_id <- i
  
  new_set <- rbind(new_set, dat_sub)
  
  print(i)
  
}

# discretize features

new_set[, Close_Gap := cut(Close_Gap,
                          breaks = quantile(Close_Gap,
                                            probs = seq(0, 1, by = 1/3)),
                          labels = c("L", "M", "H"),
                          right = FALSE)]

new_set[, Volume_Gap := cut(Volume_Gap,
                            breaks = quantile(Volume_Gap,
                                              probs = seq(0, 1, by = 1/3)),
                            labels = c("L", "M", "H"),
                            right = FALSE)]

new_set[, Daily_Change := cut(Daily_Change,
                              breaks = quantile(Daily_Change,
                                                probs = seq(0, 1, by = 1/3)),
                              labels = c("L", "M", "H"),
                              right = FALSE)]

new_set[, Low_Gap := cut(Low_Gap,
                         breaks = quantile(Low_Gap,
                                           probs = seq(0, 1, by = 1/3)),
                              labels = c("L", "M", "H"),
                              right = FALSE)]

new_set[, High_Gap := cut(High_Gap,
                          breaks = quantile(High_Gap,
                                            probs = seq(0, 1, by = 1/3)),
                          labels = c("L", "M", "H"),
                          right = FALSE)]

# collapse features per day

new_set[, Event_Pattern := paste0(Close_Gap, Volume_Gap, Daily_Change, Low_Gap, High_Gap)]

# collapse features per sequence

new_set[, Event_Pattern := paste(Event_Pattern, collapse = ","), by = .(seq_id, Close_Date)]

# only select last entry of sequence

new_set <- new_set[, .SD[.N], seq_id][, .(seq_id, Close_Date, Event_Pattern, Outcome_Next_Day_Direction)]

# split data for validation

new_set_validation <- copy(new_set[Close_Date >= "2016-01-01"][,-2])

new_set <- new_set[Close_Date < "2016-01-01"][,-2]

# binary encoding for next day outcome

new_set[, Outcome_Next_Day_Direction := ifelse(Outcome_Next_Day_Direction > 0, 1, 0)]

new_set_validation[, Outcome_Next_Day_Direction := ifelse(Outcome_Next_Day_Direction > 0, 1, 0)]

# split dataset by target variable (in this case 1 or 0)

new_set_pos <- copy(new_set[Outcome_Next_Day_Direction == 1, -3])

new_set_neg <- copy(new_set[Outcome_Next_Day_Direction == 0, -3])

# build markov transition matrix

build_transition_matrix <- function(compressed_matrix, unique_patterns){
  
  grids <- c()
  
  for(from_event in unique_patterns){
    
    print(from_event)
  
    for(to_event in unique_patterns){
    
      pattern <- paste0(from_event, ",", to_event)
    
      ID_matches <- compressed_matrix[grep(pattern, compressed_matrix$Event_Pattern),]
    
      if(nrow(ID_matches) > 0){
      
        Event_Pattern <- paste0(ID_matches$Event_Pattern, collapse = ",", sep = "~~")
      
        found <- gregexpr(pattern = pattern, text = Event_Pattern)[[1]]
      
        grid <- c(pattern, length(found))
      
      } else {
      
        grid <- c(pattern, 0)
      
      }
    
      grids <- rbind(grids, grid)
    
    } # end: for
  
  } # end: for
  
  # create dt from grids matrix
  
  grid_dt <- data.table(pairs = grids[,1], counts = grids[, 2])
  
  # split pair string into seperate columns
  
  grid_dt[, c("x", "y") := tstrsplit(pairs, ",", fixed = TRUE)]
  
  # total number of unique counts
  
  all_events_count <- length(unique_patterns)
  
  # build transition matrix
  
  transition_matrix <- (matrix(as.numeric(as.character(grid_dt$counts)), ncol=all_events_count, nrow=all_events_count))
  
  # create dt from transition matrix
  
  transition_dt <- as.data.table(transition_matrix)
  
  names(transition_dt) <- unique_patterns
  
  row.names(transition_dt) <- unique_patterns
  
  transition_dt[is.na(transition_dt)] <- 0
  
  transition_dt <- transition_dt/rowSums(transition_dt)
  
  return(transition_dt)
  
} # end: build_transition_matrix

# get all unique patterns (states)

unique_patterns <- unique(strsplit(paste0(new_set$Event_Pattern, collapse = ","), split = ",")[[1]])

# create transition matrix for each set

grid_pos <- build_transition_matrix(new_set_pos, unique_patterns)
grid_neg <- build_transition_matrix(new_set_neg, unique_patterns)


# validation

actual <- c()
predicted <- c()

for(event_id in seq(nrow(new_set_validation))){
  
  patterns <- strsplit(x = paste0(new_set_validation$Event_Pattern[event_id], collapse = ","), split = ",")[[1]]
  
  pos <- c()
  neg <- c()
  log_odds <- c()
  
  for(id in seq(length(patterns) - 1)){
    
    log_value <- log(grid_pos[patterns[id],patterns[id + 1]] / grid_neg[patterns[id],patterns[id + 1]])
    
    if(is.na(log_value) ||
       (length(log_value)==0) ||
       (is.nan(log(grid_pos[patterns[id],patterns[id+1]] / grid_neg[patterns[id],patterns[id+1]])) == TRUE)){
      
      log_value <- 0.0
      
    } else if(log_value == -Inf){
      
      log_value <- log(0.00001 / grid_neg[patterns[id], patterns[id + 1]])
      
    } else if(log_value == Inf){
      
      log_value <- log(grid_pos[patterns[id], patterns[id + 1]] / 0.00001)
      
    }
    
    log_odds <- c(log_odds, log_value)
    
    pos <- c(pos, grid_pos[patterns[id], patterns[id + 1]])
    
    neg <- c(neg, grid_neg[patterns[id], patterns[id + 1]])
    
  } # end: for
  
  print(paste('outcome:', new_set_validation$Outcome_Next_Day_Direction[event_id]))
  print(sum(pos)/sum(neg))
  print(sum(log_odds))
  
  actual <- c(actual, new_set_validation$Outcome_Next_Day_Direction[event_id])
  predicted <- c(predicted, sum(log_odds))
  
} # end: for

result <- confusionMatrix(ifelse(predicted > 0, 1, 0), actual)


