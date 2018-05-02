library(data.table)

##### SIMULATE DATA #####

# define distinct values

page_vec <- paste0("page_", 1:5)
sess_vec <- paste0("sess_", 1:50)
cust_vec <- paste0("cust_", 1:30)

# create SessionId, CustId and Page vectors

sess_rep <- round(exp(rnorm(length(sess_vec), 1, 1)))
sess_rep <- ifelse(sess_rep == 0, 1, sess_rep)

SessionId <- rep(sess_vec, sess_rep)
CustId <- rep(sample(cust_vec, length(sess_vec), replace = TRUE), rle(SessionId)$length)
Page <- sample(page_vec, length(SessionId), replace = TRUE)

# create timestamp vector

begin_date <- as.integer(as.POSIXct("2018-07-07 00:00:00"))
end_date <- as.integer(as.POSIXct("2018-07-07 23:59:59"))
Timestamp <- vector()

for (i in seq(length(sess_vec))) {
  
  start <- sample(begin_date:end_date, 1)
  
  session_length <- rle(SessionId)$length[i]
  
  session_ts <- sample(start:(start + 1800), session_length, replace = TRUE)
  
  Timestamp <- c(Timestamp, session_ts)
  
}

# build clickstream dt

clickstream <- data.table(SessionId = SessionId,
                          CustId = CustId,
                          Page = Page,
                          Timestamp = as.POSIXct(Timestamp, origin = "1970-01-01"))

clickstream <- clickstream[order(CustId, SessionId, Timestamp)]

clickstream[, c("Path", "PageNo") := .(paste(Page, collapse = ","), .N), by = SessionId]

clickstream[, PageSeq := seq(.N), by = SessionId]

clickstream[, duration := Timestamp - shift(Timestamp, 1, type = "lag"), by = SessionId]


# build summary dt

summary <- clickstream[, .(Path = paste(Page, collapse = ","),
                           length = .N,
                           unique = length(unique(Page)),
                           duration = max(Timestamp) - min(Timestamp),
                           landingpage = first(Page),
                           lastpage = last(Page)), by = SessionId]




summary[, .(lapply(.SD, function(x) cut(x, breaks = quantile(x, probs = seq(0, 1, 1/3)), include.lowest = TRUE))), .SDcols = c("length", "unique")]

# clean ws

rm(list = setdiff(ls(), c("clickstream", "summary")))