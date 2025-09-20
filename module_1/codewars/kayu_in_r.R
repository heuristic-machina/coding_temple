#codewars

#The following string are the times for a group of runners in the format hh/mm/ss
#(hours/minutes/seconds). "01|15|59, 1|47|6, 01|17|20, 1|32|34, 2|3|17"
#Convert this into numbers to compare the results giving the 3 statistics: range, 
#average, and median. The result should be in this format: 
#"Range: 00|47|18 Average: 01|35|15 Median: 01|32|34" 
#additional notes: if a result in seconds is ab.xy... it will be given truncated
#as ab. if the given string is "" return ""

stat <- function(times_str) {
  if (times_str == "") return("")
  
  # Split and parse times
  times <- strsplit(times_str, ",\\s*")[[1]]
  hms <- lapply(times, function(t) {
    parts <- as.integer(strsplit(t, "\\|")[[1]])
    parts[1] * 3600 + parts[2] * 60 + parts[3]
  })
  
  secs <- unlist(hms)
  
  # Helper to format seconds into hh|mm|ss
  #c-style sprintf formatting multiple values
  #in one go with consistent padding
  format_hms <- function(s) {
    h <- s %/% 3600
    m <- (s %% 3600) %/% 60
    sec <- s %% 60
    sprintf("%02d|%02d|%02d", h, m, sec)
  }
  
  # Compute stats
  range_val <- max(secs) - min(secs)
  avg_val <- floor(mean(secs))
  median_val <- floor(median(secs))
  
  # Format output
  paste0(
    "Range: ", format_hms(range_val),
    " Average: ", format_hms(avg_val),
    " Median: ", format_hms(median_val)
  )
}

times <- "01|15|59, 1|47|6, 01|17|20, 1|32|34, 2|3|17"
stat(times)
#[1] "Range: 00|47|18 Average: 01|35|15 Median: 01|32|34"


#2 If we list all the natural numbers below 10 that are multiples of 3 or 5, we 
#get 3, 5, 6 and 9. The sum of these multiples is 23. Finish the solution so 
#that it returns the sum of all the multiples of 3 or 5 below the number passed
#in. Note: If a number is a multiple of both 3 and 5, only count it once.

solution <- function(n) {
  if (n <= 0) return(0)
  nums <- 1:(n - 1)
  sum(nums[nums %% 3 == 0 | nums %% 5 == 0])
}

solution(10)
#[1] 23

