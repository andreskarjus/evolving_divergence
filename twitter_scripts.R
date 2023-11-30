### packages ####
library(httr)	#	1.4.5
library(tidyverse)	#	2.0.0
library(data.table)	#	1.14.2
library(rtweet)	#	1.0.2
library(scales)	#	1.2.0
library(colorspace)	#	2.0.3
library(Matrix)	#	1.5.3
library(text2vec)	#	0.6.1
library(cds)  # procrustes	#	1.0.3
library(umap)	#	0.2.8.0
library(patchwork)	#	1.1.1
library(dbscan)	#	1.1.10
library(ggrepel)	#	0.9.3
library(shadowtext)	#	0.1.2
library(ggimage) # for tears laugh emoji	#	0.3.1
library(ggflags) # for us flag	#	0.0.2
library(quanteda)	#	3.2.1
library(quanteda.textstats)	#	0.95
library(zoo) # 1.8.11



#### functions ####

# patching for buggy rtweet function which doesn't respect check=F otherwise:
get_timeline_call_fixed = function (user, n = 200, max_id = NULL, home = FALSE, parse = TRUE, 
          check = F, # the fix
          token = NULL, ...) 
{
  check=F
  stopifnot(is_n(n), is.atomic(user), is.atomic(max_id), is.logical(home))
  if (home) {
    query <- "statuses/home_timeline"
  }
  else {
    query <- "statuses/user_timeline"
  }
  if (length(user) > 1) {
    stop("can only return tweets for one user at a time.", 
         call. = FALSE)
  }
  token <- check_token(token)
  if (check) {
    rl <- rate_limit(token, query)
    n.times <- rl[["remaining"]]
    if (length(n.times) == 0 || !is.numeric(n.times)) {
      n.times <- 0
    }
    n.times <- n.times[1]
    if (n%/%200 < n.times) {
      n.times <- ceiling(n/200L)
    }
  }
  else {
    rl <- NULL
    n.times <- ceiling(n/200L)
  }
  if (n.times == 0L) {
    if (!is.null(rl)) {
      reset <- round(as.numeric(rl[["reset"]], "mins"), 
                     2)
    }
    else {
      reset <- "An unknown number of"
    }
    warning("rate limit exceeded. ", round(reset, 2), 
            " mins until rate limit resets.", call. = FALSE)
    return(data.frame())
  }
  if (n < 200) {
    count <- n
  }
  else {
    count <- 200
  }
  params <- list(user_type = user, count = count, max_id = max_id, 
                 tweet_mode = "extended", ...)
  names(params)[1] <- .id_type(user)
  url <- make_url(query = query, param = params)
  tm <- scroller(url, n, n.times, type = "timeline", 
                 token)
  if (parse) {
    tm <- tweets_with_users(tm)
  }
  tm
}

#environment(get_timeline_call_fixed) <- asNamespace('rtweet')
#assignInNamespace("get_timeline_call", get_timeline_call_fixed, ns = "rtweet") # NO LONGER WORKS, would need to check if rtweet has changed internally


# stopwords
stopliterals = unique( c("i", "a", "be", "is","was", "will", "of", "to", "it", "so", "do", "does","did", "by", "go", "no", "if", "as", "at", "am", "an", "in", "on", "the", "but", "was", "are", "has", "have","had", "and", "for", "got", "get", "yes", "its","this", "that","these", "those", "with","can", "could","would", letters,  "'s", "'s", "’s", "n't","n't","'ll", "'d", '!', '"', '$', '%', '&', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '[', ']', '^', '_', '`', '{', '|', '}', '~', '.', "'", "'",'"',"’", "„","“", "#","…", "@", "", 
      "--", "–", "—", "..", "...", "....", ".....",  "......", ".......", "........", "//", 
      "\uFE0F", "amp","-pron-","\n", "\n\n" )
                         )



getthetweets = function(users, n, token){
  #x=rate_limit(token)
  #lim= x  %>% filter(query=="statuses/user_timeline") %>% pull(remaining) # assuming max at start
  # mins= x %>% filter(query=="statuses/user_timeline") %>% pull(reset)
  #userchunks = split(users$user_id, ceiling(seq_along(users$user_id)/ (lim/( ceiling(n/200)) )  ) )
  # sapply(userchunks, length)
  # userchunks = split(users$user_id, ceiling(seq_along(users$user_id)/ (900/(n/200)-100)  ) )
  userchunks = split(users$user_id, ceiling(seq_along(users$user_id)/ ((900*198)/n)  ) ) # fixed: every 200 tweets decrements the remain counter; a bit for buffer; should be able to do chunk by chunk without timing out.
  nchunks = length(userchunks)  # how many chunks needed?

  
  tweets = tibble()
  print(paste0(Sys.time(), " doing ", nrow(users), " users in ", nchunks, " chunks" ))
  for(i in 1:nchunks){
    tryCatch({
     # if(i > 1){
        x=rate_limit(token)
        lim= x %>% filter(query=="statuses/user_timeline") %>% pull(remaining)
        if(lim < (length(userchunks[[i]])*ceiling(n/200) )   ) {
          reset = x %>% filter(query=="statuses/user_timeline") %>% pull(reset)
          print(paste0(Sys.time(), " Waiting for ",  ceiling(reset), " minutes." ))
          Sys.sleep(as.numeric(reset)*60+1) # plus a little buffer just in case
        }
     # }
      print(paste(Sys.time(),i))
      
      # mining
      tw=NULL
      tw = get_timeline(
        #"foxnews",
        userchunks[[i]],
        n = n,
        token = token,
        check=FALSE,    # otherwise runs out of status check limit fast; now manual control
        include_rts="0" # direct api param, excludes retweets
      )
      
      # if successful, add
      if(!is.null(tw) && nrow(tw)>0){
        # tw = mutate(tw, group = 
        #               case_when(user_id %in% {users %>% filter(follow=="CNN") %>% pull(user_id)} ~ "CNN",
        #                         user_id %in% {users %>% filter(follow=="FoxNews") %>% pull(user_id)} ~ "FoxNews"
        #               ) 
        # )   # old
        
        tw = tw[, 
                c("user_id", "text", "status_id","status_url", "created_at",
                  "favorite_count","retweet_count", "lang" )
                ]
        
        tweets = rbind(tweets, tw)
        tweets2 <<- tweets  # debug, save to global
      } else {
        print("Something wrong, query empty")
      }
      try({rm(tw)})
    }, error = function(e){if(exists("ee")){try({ee<<-c(ee,e)})}else{ee<<-e};print(e) }
    ) # end trycatch
  }

  rate_limit(token) %>% filter(query %in% c("application/rate_limit_status", "statuses/user_timeline")) %>% print()
  return(tweets)
}





getusers = function(token, target, goal = 100000, batchsize = 25000, skip=25000, providedcursor=NA, 
                    minage="2021-02-01", lasttweetmin="2021-03-01", debug=F
                    ){
  print(paste(Sys.time(), "Starting"))
  users1 = tibble() 
  
  if(is.na(providedcursor)){
    cursor="-1"
    if(skip>0){
    # paginate through first n new users (who might be new to twitter too so no tweets)
    # "At this time, results are ordered with the most recent following first"
    # store cursor
    u1 = get_followers(
      user=target,
      n = skip,
      page = cursor,
      retryonratelimit = T, # but just in case
      parse = TRUE,
      verbose = F,
      token = token
    )
    cursor = next_cursor(u1)
    #  }
    cat("...\n")
    }
    # will start data collection from paginated cursor
    globalcursor<<-cursor # store just in case
  } else {
    cursor=providedcursor
  }
  
  total=0
  ii=1
  while(nrow(users1) < goal){
    tryCatch({
      print(paste(Sys.time(), ii, total))
      
      # Check if limits remaining
      # applicable limit: application/rate_limit_status, 180x
      x=rate_limit(token) %>% filter(query %in% c("followers/ids", "application/rate_limit_status", "users/lookup"))
      # cat(" ids left ", x$remaining[3])
      # if limits reached, wait required number of minutes:
      if(x$remaining[1] < 1 ){
        cat(" (waiting on status180,",x$remaining[1], ") ")
        Sys.sleep(x$reset[1])
      }

      # To return more than 75,000 user IDs in a single call (the rate limit maximum), set "retryonratelimit" to TRUE.
      # limit: followers/ids which is 15x5000
      u1 = get_followers(
        user=target,
        n = batchsize,
        page = cursor,
        retryonratelimit = T,
        parse = TRUE,
        verbose = F,
        token = token
      )
      if(exists("u1") && nrow(u1)>0 ){
        cat(paste0(" (",Sys.time(), " ", nrow(u1), " users " ))
        total=total+nrow(u1)
        cursor = next_cursor(u1)      # store cursor before filtering, needed to get next batch
        
        if(!debug){
        # Returns data on up to 90,000 Twitter users. To return data on more than 90,000 users, code must be written to iterate through user IDs whilst avoiding rate limits, which reset every 15 minutes
        # limit: users/lookup, 900 which seems to stand for 90k, as 100-batch reduced to 899
        
        x=rate_limit(token) %>% filter(query %in% c("followers/ids", "application/rate_limit_status", "users/lookup"))
        if(x$remaining[2] < (batchsize/100)){  # looks like needed for users/lookup (no retry param)
          cat(" (waiting on lookup900,",x$remaining[2], ") ")
          Sys.sleep(x$reset[2])
        }
        u1 = lookup_users(u1$user_id) # amend data (takes time, hopefully ids limit will replenish meanwhile?)
        cat(nrow(u1), "accessed ", Sys.time())
        # filtering:
        
        u1 = u1 %>% select(user_id, screen_name, country, location, statuses_count, friends_count, followers_count, account_created_at, created_at, protected) %>% 
          filter(country == "United States",  # just US
                 !protected,  # wouldn't be able to get tweets
                 account_created_at < as.POSIXct(paste0(minage, " 00:00:00")), # has had account at least few months
                 created_at  > as.POSIXct(paste0(lasttweetmin, " 00:00:00")) ,        # but has tweeted least once within last month
                 friends_count >= 20,   # has more than that they follow
                 followers_count >= 5,  # and that follows them  (filter more later?)
                 statuses_count >=20 # has tweets that can be actually mined
          ) %>% 
          select(-c(protected,created_at))
        u1$batch = ii
        cat(paste0(" ",nrow(u1), " left)\n" ))
        } 
        # else {
        #   u1 = u1 %>% select(user_id, screen_name, country, location, statuses_count, friends_count, followers_count, account_created_at, created_at, protected) %>% 
        #     filter(country == "United States",  # just US
        #            !protected,  # wouldn't be able to get tweets
        #            account_created_at < as.POSIXct(paste0(minage, " 00:00:00")), # has had account at least few months
        #            created_at  > as.POSIXct(paste0(lasttweetmin, " 00:00:00"))         # but has tweeted least once within last month
        #            #friends_count >= 50,   # has more than that they follow
        #            #followers_count >= 10,  # and that follows them  (filter more later?)
        #            #statuses_count >=50 # has tweets that can be actually mined
        #     ) 
        # }
        users1 = rbind(users1, u1) %>% filter(!duplicated(user_id)) # append eligible users
        test<<-users1  # debug
        rm(u1)
        ii=ii+1
        
      }
    }, error = function(e){if(exists("ee")){ee<<-c(ee,e)}else{ee<<-e};print(e) }
    ) # end trycatch
  } # end while
  globalcursor2<<-cursor # store cursor, could continue data collection if needed
  try({ print(paste(Sys.time(), "Done collecting", nrow(users1), "users; total users parsed:", total )) })
  return(users1)
}


stream_tweets_v2patch = function (q = "", timeout = 30, parse = TRUE, bearertoken = NULL, 
          file_name = NULL, verbose = TRUE, postbody, ...) 
{
  if ("append" %in% names(list(...))) {
    stop("append should only be used with stream_tweets2() (which is in development)", 
         call. = FALSE)
  }
  if (!identical(getOption("encoding"), "UTF-8")) {
    op <- getOption("encoding")
    options(encoding = "UTF-8")
    on.exit(options(encoding = op), add = TRUE)
  }
  token <- rtweet:::check_token(token)
  if (!timeout) {
    timeout <- Inf
  }
  if (missing(q) || is.null(q)) 
    q <- ""
  stopifnot(is.numeric(timeout), timeout > 0, any(is.atomic(q), 
                                                  inherits(q, "coords")), is.atomic(file_name))
  if (identical(q, "")) {
    query <- "statuses/sample"
    params <- NULL
  }
  else {
    query <- "statuses/filter"
    params <- rtweet:::stream_params(q, ...)
  }
  #url <- make_url(restapi = FALSE, query, param = params)
  if (is.null(file_name)) {
    tmp <- TRUE
    file_name <- rtweet:::tmp_json()
  }
  else {
    tmp <- FALSE
  }
  if (is.infinite(timeout)) 
    tmp <- FALSE
  if (!grepl("\\.json$", file_name)) {
    file_name <- paste0(file_name, ".json")
  }
  if (!file.exists(file_name)) 
    file.create(file_name)
  if (verbose) {
    message(paste0("Streaming tweets for ", timeout, " seconds..."))
  }
  r <- NULL
  con <- file(file_name, "wt", encoding = "UTF-8")
  on.exit({
    sh <- tryCatch(close(con), error = function(e) return(NULL), 
                   warning = function(w) return(NULL))
  }, add = TRUE)
  start_time <- Sys.time()
  stop_time <- Sys.time() + timeout
  ctr <- 0
  
  # check stream rules
  getrules = function(){
    httr::GET(url = "https://api.twitter.com/2/tweets/search/stream/rules",
                           httr::add_headers(.headers = c(`Content-Type` = 'application/json',
                                                          Authorization = paste0("Bearer ", bearertoken)
                           ))) %>% content() %>%  .$data %>% return()
  }
  presentrules = getrules()
  # Step three: Add your rule to the stream
  if(length(presentrules) > 1){ warning("more than two rules somehow, check it") }
  if(length(presentrules) < 1){
    message("0 rules, adding")
    httr::POST(url = "https://api.twitter.com/2/tweets/search/stream/rules",
               body = paste0('{"add": [ ', postbody, ' ]}'),
               # '{"add": [ {"value": "cat has:images", "tag":"x"} ] }' 
               encode="json",
               httr::add_headers(.headers = c(`Content-Type` = 'application/json',
                                              Authorization = paste0("Bearer ", bearertoken)
               ))
               #httr::write_stream(write_fun(con)), 
               #,verbose()
    )  %>%  http_status()
    if(length(getrules()) < 1){ stop("no rules but adding failed") }
  }
  
  # debug rule
  # httr::POST(url = "https://api.twitter.com/2/tweets/search/stream/rules",
  #            body = paste0('{"add": [ ', '{ "value": "(year OR work OR this OR that OR happy) lang:en" }', ' ]}'),
  #            # '{"add": [ {"value": "cat has:images", "tag":"x"} ] }' 
  #            encode="json",
  #            httr::add_headers(.headers = c(`Content-Type` = 'application/json',
  #                                           Authorization = paste0("Bearer ", bearertoken)
  #            ))
  #            #httr::write_stream(write_fun(con)), 
  #            #,verbose()
  # )  %>%  http_status()
  
  # debug deleter
  # httr::POST(url = "https://api.twitter.com/2/tweets/search/stream/rules",
  #            body = '{"delete": { "ids": [ "1373699228359352321" ]}}',
  #            encode="json",
  #            httr::add_headers(.headers = c(`Content-Type` = 'application/json',
  #                                           Authorization = paste0("Bearer ", bearertoken)
  #            ))
  # ) %>%  http_status()
  
  write_fun <- function(con) {
    function(x) {
      writeLines(rawToChar(x), con)
    }
  }
  
  while (timeout > 0) {
    r <- tryCatch(
    
    httr::GET(url = "https://api.twitter.com/2/tweets/search/stream?tweet.fields=created_at,author_id",
              httr::config(timeout=timeout),
               httr::add_headers(.headers = c(Authorization = paste0("Bearer ", bearertoken))),
               httr::write_stream(write_fun(con)),
               timeout = timeout
               #,verbose()
    )  %>% content() -> x #  http_status()
    
      , error = function(e) return(e))
    timeout <- as.numeric(difftime(stop_time, Sys.time(), 
                                   units = "secs"))
    if (timeout > 0) {
      ctr <- ctr + 1
      if (ctr == 1 && verbose) 
        message("The stream disconnected prematurely. Reconnecting...")
      if (ctr == 2 && verbose) 
        message("Reconnecting again...")
      if (ctr == 5) 
        break
    }
    else if (verbose) {
      message("Finished streaming tweets!")
    }
  }
  # while (timeout > 0) {
  #   r <- tryCatch(httr::POST(url = url, httr::config(token = token, 
  #                                                    timeout = timeout), httr::write_stream(write_fun(con)), 
  #                            httr::add_headers(Accept = "application/json"), 
  #                            httr::add_headers(`Accept-Encoding` = "gzip, deflate")), 
  #                 error = function(e) return(e))
  #   timeout <- as.numeric(difftime(stop_time, Sys.time(), 
  #                                  units = "secs"))
  #   if (timeout > 0) {
  #     ctr <- ctr + 1
  #     if (ctr == 1 && verbose) 
  #       message("The stream disconnected prematurely. Reconnecting...")
  #     if (ctr == 2 && verbose) 
  #       message("Reconnecting again...")
  #     if (ctr == 5) 
  #       break
  #   }
  #   else if (verbose) {
  #     message("Finished streaming tweets!")
  #   }
  # }
  close(con)
  if (parse) {
    #out <- parse_stream(file_name, verbose = verbose)
    out = readLines(file_name, encoding = "UTF-8", warn=FALSE)
    if (tmp) {
      file.remove(file_name)
    }
    closeAllConnections()  # seems otherwise something stays open and next call fails
    return(out)
  }
  if (verbose) 
    message("streaming data saved as ", file_name)
  invisible(r)
}




filter_users = function(users, batchsize=89999, goal=Inf, followerfilter, debug=F, token){
  require("rtweet")
  ff = followerfilter
  
  httr::set_config(httr::config(http_version = 0)) # on server curl will crash otherwise
  
  
  print(paste(Sys.time(), "Starting"))
  users1 = tibble() 
  
  ulist=split(users, ceiling(seq_along(users)/batchsize))
  
  ii=1
  for(i in seq_along(ulist)){
    tryCatch({
      print(paste(Sys.time(), ii))
      
      
      # Returns data on up to 90,000 Twitter users. To return data on more than 90,000 users, code must be written to iterate through user IDs whilst avoiding rate limits, which reset every 15 minutes
      # limit: users/lookup, 900 which seems to stand for 90k, as 100-batch reduced to 899
      
      # note: some previously fetched ids don't get lookup match, must be deleted accounts
      x=rate_limit(token) %>% 
        filter(query %in% c("followers/ids", "application/rate_limit_status", "users/lookup"))
      if(x$remaining[1] < 2 ){
        cat(" (waiting on status180,",x$remaining[1], ") ")
        Sys.sleep(as.numeric(x$reset[1] )*60 )
      }
      if(!debug){
        if(x$remaining[2] < 900){  # looks like needed for users/lookup (no retry param)
          cat(" (waiting on lookup900,",x$remaining[2], ") ")
          Sys.sleep(as.numeric(x$reset[2] )*60 )
        }
      }
      
      # look up users
      u1 = lookup_users(ulist[[i]]) # amend data (takes time)
      cat(nrow(u1), "accessed out of ", length(ulist[[i]]), as.character(Sys.time()), "|")
      # filtering:
      
      u1 = u1 %>% select(user_id, screen_name, country, location, statuses_count, friends_count, followers_count, account_created_at, created_at, protected) %>% 
        
        filter(
               !protected,  # wouldn't be able to get tweets
               country         == ff$country,
               account_created_at < ff$account_created_at,
               created_at       > ff$created_at ,       
               friends_count   >= ff$friends_count,   # has more than that they follow
               followers_count >= ff$followers_count,  # and that follows them  (filter more later?)
               statuses_count  >= ff$statuses_count # has tweets that can be actually mined
        ) %>% 
        select(-c(protected, created_at))
      u1$batch = ii
      cat(paste0(" ",nrow(u1), " left)\n" ))
      
      
      users1 = rbind(users1, u1) # %>% filter(!duplicated(user_id)) # append eligible users
      test<<-users1  # debug
      rm(u1)
      ii=ii+1
    }, error = function(e){if(exists("ee")){ee<<-c(ee,e)}else{ee<<-e};print(e) }
    ) # end trycatch
    
    if(nrow(users1) >= goal){
      print(paste(Sys.time(), "Goal n reached, stopping early"))
      break   # optional: get only specified number of users
    }
  } # end for
  try({ print(paste(Sys.time(), "Done collecting", nrow(users1), "users" )) })
  return(users1)
}

sleepnow=function(){
  print(Sys.time())
  if(Sys.info()["sysname"] == "Windows") {
    installr::os.sleep(first_turn_hibernate_off = T)
  } else {
    system("pmset sleepnow")
  }
}


#### prep mined data ####



followers_listparser = function(
  accounts,
  corpusfolder ="C:/Users/Andres/korpused/twitter",
  files = "followerminer", 
  fixed = "followerminer/fixed"
){
  
  ff = file.path(corpusfolder, files)
  tf = file.path(corpusfolder,fixed)
  l = list.files(ff, pattern = ".RDS$")
  l2 = gsub("[_0-9]*\\.RDS$", "", l) %>% tolower()
  l = list.files(ff, pattern = ".RDS$", full.names = T)
  
  accounts$collected = accounts$unique = accounts$nfiles = NA
  for(i in seq_along(accounts$acc)) {
    cat(accounts$acc[i], " ")
    a = which(l2 %in% accounts$acc[i])
    accounts$nfiles[i] = length(a)
    x1 = lapply(l[a], function(x) readRDS(x) %>% pull(1) ) %>% unlist(F,F)
    accounts$collected[i] = length(x1)
    x2 = x1 %>% unique() 
    accounts$unique[i] = length(x2)
    x2 %>% writeLines(file.path(tf, paste0(accounts$acc[i], ".txt")), useBytes = T)
  }
  accounts$side = factor(accounts$side,levels = c("left", "leanleft", "center", "leanright", "right"))
  accounts$path = file.path(tf, paste0(accounts$acc, ".txt") )
  return(accounts)
}


tweetfilter = function(x, side){
  x = x %>% filter(created_at>=as.POSIXct(paste0("2021-02-01", " 00:00:00"))) %>% 
    filter(!lang %in% c("ja", "zh", "ko", "ar", "he", "ru", "th", "uk", "ur", "el", "fa" )) 
  if(side != "news"){
    x = x %>% 
      group_by(user_id) %>% 
      filter((sum(favorite_count)/n()) > 0.03 ) %>% 
      arrange(desc(retweet_count+favorite_count), desc(nchar(text)), .by_group=T) %>% 
      slice_head(n=700 ) %>% group_by(user_id) %>% 
      filter(!(min(created_at)>=as.POSIXct(paste0("2021-09-05", " 00:00:00")) & n() == 700) )
  }
  return(x  %>% mutate(side=side))
}



# still in there:
# -
# –
# —

tweetlemmatizer = function(x, news=""){
  # x = data.frame(text="hi #guys https://url.com/s https://url.com/s &amp; $cash ”&#8216; !! mad-dogs @man  b/c and w/ this") # debug
  
  # ----> to fix: ’ still somehow stays in there --> now ok?
  print(paste(Sys.time(), "doing cleaning"))
  ids = names(x)
  x = x = iconv(x, "UTF-8", "UTF-8",sub=' ')  # fixes some broken utf, so it doesn't throw warning
  x = x %>% 
    gsub("[0-9:]+(am|pm)([^[:alpha:]])", "\\1 \\2",.,ignore.case = T) %>%  # remove time digits
    gsub("[“”'‘’«‹»›„‟〝〞〟＂‚‛]", " ",.) %>%  # try to remove the smart quotes once more
    str_replace_all("[\U1F3FB\U1F3FC\U1F3FD\U1F3FE\U1F3FF\U200D\UFE0F\U203C\U0001F3FC\U0001F3FD\U0001F3FE\U0001F3FB♀♂\U2640\U2642]"," ") %>% # skin tone modifiers, gender modifiers, their joiners, "invisible" chars, too much variation and messes with lemmatizer; but also separated gender symbol counts as punctuation for regex
    gsub("&[#a-z0-9]{0,5};", " ", ., ignore.case = T) %>%   #various html entities that mess with lemmatizer (for some reason str_replace_all failed here?)
    # try to rescue classic punctuation-emoticons from the gsubs and later lemmatizer:
    gsub("([[:space:]]|^)([:;][-]{0,1}[()/]|:[-]{0,1}[ops]|\\(:)([[:space:]]|$)", " |\\2| ",., ignore.case = T) %>% 
    gsub("([^|]|^)([\"'!?,:;.=()—%~*+•-])+([^|]|$)", '\\1\\2\\3', .) %>%  # repeated punct (also removes remaining handtyped emoticons but I guess not many of those
    
    # flag emoji - uses multiple chars which then get split ---> doesn't work, spacy messes up
    #str_replace_all(. , "([\U1F1E6\U1F1E7\U1F1E8\U1F1E9\U1F1EA\U1F1EB\U1F1EC\U1F1ED\U1F1EE\U1F1EF\U1F1F0\U1F1F1\U1F1F2\U1F1F3\U1F1F4\U1F1F5\U1F1F6\U1F1F7\U1F1F8\U1F1F9\U1F1FA\U1F1FB\U1F1FC\U1F1FD\U1F1FE\U1F1FF]{2,2})"," |\\1| "  ) %>% 
    # instead hand-code the more frequent flags, discard others, and restore after spacy
    str_replace_all(., c("\U1F1FA\U1F1F8" = " |-US-| ",
                         "\U1F1FA\U1F1F2" = " |-US-| ", # variant, same as US flag
                         "\U1F1F2\U1F1FD" = " |-MX-| ",
                         "\U1F1E8\U1F1E6" = " |-CA-| ",
                         "\U1F1EC\U1F1E7" = " |-GB-| ",
                         "\U1F1EE\U1F1F9" = " |-IT-| ",
                         "\U1F1EE\U1F1F1" = " |-IL-| "
                         )) %>% 
    str_replace_all(. , "([\U1F1E6\U1F1E7\U1F1E8\U1F1E9\U1F1EA\U1F1EB\U1F1EC\U1F1ED\U1F1EE\U1F1EF\U1F1F0\U1F1F1\U1F1F2\U1F1F3\U1F1F4\U1F1F5\U1F1F6\U1F1F7\U1F1F8\U1F1F9\U1F1FA\U1F1FB\U1F1FC\U1F1FD\U1F1FE\U1F1FF]{2,2})"," "  ) %>%  # discard other less frequent ones
    
    # hashtags, but need twice to fix if multiple without space:
    gsub("[#]([0-9]*[a-z]+[0-9]*)($|[[:space:][:punct:]]|#)", " |\\1| \\2", .,ignore.case = T) %>%  
    gsub("[#]([0-9]*[a-z]+[0-9]*)($|[[:space:][:punct:]]|#)", " |\\1| \\2", .,ignore.case = T) %>%  
    gsub("[|]{2,2}", "| |",.) %>% 
    gsub("[#…]+", " ",.) %>%  # apparently the above somehow misses some, so a hardcoded fix. also the dots symbols which the punct flag misses.
    # same for cashtags, but leave their symbol
    gsub( "([$][a-z]+)([$]|[[:space:][:punct:]]|$)", " |\\1| \\2", ., ignore.case = T) %>% 
    gsub(" ([$][a-z]+)([[:space:][:punct:]]|$)", " |\\1|\\2", ., ignore.case = T) %>% 
    # protect some terms of interest from being lemmatized/broken apart:
    gsub("([[:space:][:punct:]]|^)(fake|can't|january) (news|even|6th|6)", " |\\2_\\3|",., ignore.case = T) %>% 
    gsub("([[:space:]#]|^)(wokeness|woke|gonna|45th|45|adulting|blessed|lit|vaxed|anti-vaxxer|anti-vax|anti-vaccine|anti-vaccination|anti-masker|maga|acab)[s]*[[:space:][:punct:]]", " |\\1\\2| ",.,ignore.case = T) %>% 
    # homogenize varying spellings of haha and such:
    gsub("([[:space:][:punct:]]|^)(ha){2,}([[:space:][:punct:]]|$)|( ha ha){1,}", " haha ",.,ignore.case = T) %>%  
    gsub("a{2,}nd", "aand",., ignore.case = T) %>% 
    gsub("(zzz|soo|boo|yess|ohh|ooh|aww|hmm|umm)[zoshwm]+", "\\1", ., ignore.case = T) %>%
    #
    # in-word dash to underscore so it won't split (but only alphabet i.e. not numeric ranges)
    gsub("([[:alpha:]])-([[:alpha:]])","\\1_\\2",.,ignore.case = T ) %>% 
    gsub("([[:space:][:punct:]]|^)([acb]{1,2})/([dc]{1,2})", " |\\2\\3|",., ignore.case = T) %>% # this splits b/c and ac/dc though
    gsub("[[:space:]]/s", "|_s|",.) %>%   # sarcasm
    gsub("[/]([^|]|$)", " ", .) %>% # remove all other slashes /, this also gets rids of arbitrary fractions by extension (but keep the smiley one)
    gsub("(^|[[:space:]])([0-9]+[.,)]|•)", " ", .) %>% # remove list dots (spacy won't split if no space)
    gsub("([^|]|^)([,.%!?&:;)-])+([^ |])","\\2 \\3", .) %>% # put space after punctuation (spacy can fail; dashes already fixed where needed)
    str_replace_all("\\s+", " ") # remove newlines and multi-spaces (that above may also generate), incl the unicode ones which gsub would miss
  gc(verbose=F)
  
  
  # Lemmatize
  names(x)=ids # reattach names; these get passed to spacy object, and will allow re-building after chunking just fine.
  rm(ids)
  # spacy seems to choke and crash on full dataset, so will chunk
  chunks = (1:length(x)) %>%  {split(., ceiling(seq_along(.)/200000))} # 100000
  clist = vector("list", length(chunks)) 
  for(i in seq_along(chunks)){
    print(paste(Sys.time(), "doing chunk", i, "of", length(chunks)))
    clist[[i]]  = spacy_parse(x[ chunks[[i]] ] , pos=F,tag=F,lemma=T,entity=F,multithread=T, dependency = F, nounphrase = F ) # lemmatize
  }
  x = do.call(rbind, clist) 
  rm(clist); gc(verbose=F)
  
  # post-lemmatization restoration steps:
  x$lemma = x$lemma %>% 
    str_replace_all(., "^\\s+$|^[0-9\"'!?,:;.=()—%~*+•-]+$|@.*|http.*", " ") %>%  # matches also the weird unicode spaces that gsub won't  #gsub("^[[:space:]]+$|^[0-9\"'!?,:;.=()—%~*+•-]+$|@.*", " ",.) %>% 
    # empties to make sure collocations not misleading, will remove later; but protect |...| ones
    # remove clinging punctuation, for some reason not properly all tokenized:
    gsub("^[\"'!?,:;.=()—%~*+•-]+([[:alpha:]0-9]+)", "\\1",.) %>% 
    gsub("([[:alpha:]0-9]+)[\"'!?,:;.=()—%~*+•-]+$", "\\1",.) %>% 
    tolower() %>% 
    # restore flags:
    str_replace_all(., c("\\|-us-\\|" = "\U1F1FA\U1F1F8",
                         "\\|-mx-\\|" = "\U1F1F2\U1F1FD",
                         "\\|-ca-\\|" = "\U1F1E8\U1F1E6",
                         "\\|-gb-\\|" = "\U1F1EC\U1F1E7",
                         "\\|-it-\\|" = "\U1F1EE\U1F1F9",
                         "\\|-il-\\|" = "\U1F1EE\U1F1F1"
    )) 
    
  # but keep extra spaces, needed for multiword to work properly

  
  # Multiword
  print(paste(Sys.time(), "Calculating collocations"))
  x = x %>% as.tokens(use_lemma=T, include_pos="none") # switch to quanteda format
  multiword = rbind(
    textstat_collocations(x, min_count = 100, size=2 ) %>% filter(nchar(collocation)>=5),
    textstat_collocations(x, min_count = 100, size=3 ) %>% filter(nchar(collocation)>=8),
    textstat_collocations(x, min_count = 100, size=4 ) %>% filter(nchar(collocation)>=10)
    ) %>% 
    filter(grepl("^[a-z 0-9]+$", collocation) & !grepl("^[ ]|[ ]$|[ ]{2,}", collocation) ) %>%  
    arrange(desc(lambda))
  # note, this seems to crash when testing with very small samples (probably the collocation parameters, if less data), but fine on corpus.
  
  phr = multiword %>% 
    filter(lambda>=5 & length==2 & !grepl("^my |^your |^his |^her |^our |^their |^a |^an |^the |^of | a$| an$| the$| of$", collocation)) %>% 
    pull(collocation) %>% 
    c(.,   # add some of the 3 and 4 ones manually to be safe, as sometimes the lambda gives weird results for longer ones; and add the media station names if multiword.
      c("able_to", "united states of america", "president joe biden","of course", "law abiding", "law and order", "small business", "cancel culture", "political correctness", "politically correct", "president donald trump", "president donald j trump", "donald j trump", "former president", "supreme court", "black lives matter", "all lives matter", "blue lives matter", "press secretary","now be the time","my god", "be i suppose to","at the same time", "day at a time","all over the place","anything to do with","how many","in the first place","ask for a friend", "george floyd", "police brutality","school shooting", "school shoot", "4th of july", "post a photo", "look forward to", "in front of", "take care of", "sorry to hear", "in order to", "link in bio", "in a row", "hell of a", "might as well", "out of control", "hang in there", "secretary of state", "give a fuck", "behind the scene", "make i sick", "in other word", "people of color","person of color", "the wake of", "get away with", "at some point", "i be sorry", "what the hell", "shout out to", "piece of shit", "not to mention", "love to see", "shame on you", "not at all", "great to see", "you be welcome", "check it out", "flat out", "sum up", "walk dead", "your loss", "next door", "live stream", "pretty much", "federal law", "so far", "great job", "a lot", "local government","national guard", "same thing", "gon na", "nuff said" , "call of duty",    "mother jones", "daily beast", "new yorker", "break news", "new york times", "the economist", "washington post", "yahoo news", "associated press", "christian science monitor", "the hill", "usa today", "wall street journal", "american conservative", "fox news", "fox news alert", "epoch times", "new york post", "washington examiner", "washington times", "fox friends", "one america news", "daily caller", "daily mail", "daily wire", "national review", "american spectator", "key be",  "key to", "be key", "stop the steal", "fake news", "democrat party", "conservative party", "nasty woman", "alternative facts", "alternative fact", "drain the swamp", "make america great again", "believe woman", "no wonder", "rent free", "the us", "to say the least", "anti vaxxer", "anti vax", "anti vaccine", "anti vaccination", "anti masker", "anti mask", "rolling stone", "roger stone", "the squad", "critical_race_theory")
    ) %>% setdiff(.,c("be able", "just post", "president donald", "president joe", "donald j", "j trump")) %>% 
    unique()
  # flags = c("🇦 🇩", "🇦 🇪", "🇦 🇫", "🇦 🇬", "🇦 🇮", "🇦 🇱", "🇦 🇲", "🇦 🇴", "🇦 🇶", "🇦 🇷", "🇦 🇸", "🇦 🇹", "🇦 🇺", "🇦 🇼", "🇦 🇽", "🇦 🇿", "🇧 🇦", "🇧 🇧", "🇧 🇩", "🇧 🇪", "🇧 🇫", "🇧 🇬", "🇧 🇭", "🇧 🇮", "🇧 🇯", "🇧 🇱", "🇧 🇲", "🇧 🇳", "🇧 🇴", "🇧 🇶", "🇧 🇷", "🇧 🇸", "🇧 🇹", "🇧 🇻", "🇧 🇼", "🇧 🇾", "🇧 🇿", "🇨 🇦", "🇨 🇨", "🇨 🇩", "🇨 🇫", "🇨 🇬", "🇨 🇭", "🇨 🇮", "🇨 🇰", "🇨 🇱", "🇨 🇲", "🇨 🇳", "🇨 🇴", "🇨 🇵", "🇨 🇷", "🇨 🇺", "🇨 🇻", "🇨 🇼", "🇨 🇽", "🇨 🇾", "🇨 🇿", "🇩 🇪", "🇩 🇬", "🇩 🇯", "🇩 🇰", "🇩 🇲", "🇩 🇴", "🇩 🇿", "🇪 🇦", "🇪 🇨", "🇪 🇪", "🇪 🇬", "🇪 🇭", "🇪 🇷", "🇪 🇸", "🇪 🇹", "🇪 🇺", "🇫 🇮", "🇫 🇯", "🇫 🇰", "🇫 🇲", "🇫 🇴", "🇫 🇷", "🇬 🇦", "🇬 🇧", "🇬 🇩", "🇬 🇪", "🇬 🇫", "🇬 🇬", "🇬 🇭", "🇬 🇮", "🇬 🇱", "🇬 🇲", "🇬 🇳", "🇬 🇵", "🇬 🇶", "🇬 🇷", "🇬 🇸", "🇬 🇹", "🇬 🇺", "🇬 🇼", "🇬 🇾", "🇭 🇰", "🇭 🇲", "🇭 🇳", "🇭 🇷", "🇭 🇹", "🇭 🇺", "🇮 🇨", "🇮 🇩", "🇮 🇪", "🇮 🇱", "🇮 🇲", "🇮 🇳", "🇮 🇴", "🇮 🇶", "🇮 🇷", "🇮 🇸", "🇮 🇹", "🇯 🇪", "🇯 🇲", "🇯 🇴", "🇯 🇵", "🇰 🇪", "🇰 🇬", "🇰 🇭", "🇰 🇮", "🇰 🇲", "🇰 🇳", "🇰 🇵", "🇰 🇷", "🇰 🇼", "🇰 🇾", "🇰 🇿", "🇱 🇦", "🇱 🇧", "🇱 🇨", "🇱 🇮", "🇱 🇰", "🇱 🇷", "🇱 🇸", "🇱 🇹", "🇱 🇺", "🇱 🇻", 
  #           "🇲 🇶", "🇲 🇷", "🇲 🇸", "🇲 🇹", "🇲 🇺", "🇲 🇻", "🇲 🇼", "🇲 🇽", "🇲 🇾", "🇲 🇿", "🇳 🇦", "🇳 🇨", "🇳 🇪", "🇳 🇫", "🇳 🇬", "🇳 🇮", "🇳 🇱", "🇳 🇴", "🇳 🇵", "🇳 🇷", "🇳 🇺", "🇳 🇿", "🇴 🇲", "🇵 🇦", "🇵 🇪", "🇵 🇫", "🇵 🇬", "🇵 🇭", "🇵 🇰", "🇵 🇱", "🇵 🇲", "🇵 🇳", "🇵 🇷", "🇵 🇸", "🇵 🇹", "🇵 🇼", 
  #           "🇵 🇾", "🇶 🇦", "🇷 🇪", "🇷 🇴", "🇷 🇸", "🇷 🇺", "🇷 🇼", "🇸 🇦", "🇸 🇧", "🇸 🇨", "🇸 🇩", "🇸 🇪", "🇸 🇬", "🇸 🇭", "🇸 🇮", "🇸 🇯", "🇸 🇰", "🇸 🇱", "🇸 🇲", "🇸 🇳", "🇸 🇴", "🇸 🇷", "🇸 🇸", "🇸 🇹", "🇸 🇻", "🇸 🇽", "🇸 🇾", "🇸 🇿", "🇹 🇦", "🇹 🇨", "🇹 🇩", "🇹 🇫", "🇹 🇬", "🇹 🇭", "🇹 🇯", "🇹 🇰", "🇹 🇱", "🇹 🇲", "🇹 🇳", "🇹 🇴", "🇹 🇷", "🇹 🇹", "🇹 🇻", "🇹 🇼", "🇹 🇿", "🇺 🇦", "🇺 🇬", "🇺 🇲", "🇺 🇳", "🇺 🇸", "🇺 🇾", "🇺 🇿", "🇻 🇦", "🇻 🇨", "🇻 🇪", "🇻 🇬", "🇻 🇮", "🇻 🇳", "🇻 🇺", "🇼 🇫", "🇼 🇸", "🇽 🇰", "🇾 🇪", "🇾 🇹", "🇿 🇦", "🇿 🇲", "🇿 🇼")
  lem2 = x %>% 
    tokens_compound( phrase(phr), valuetype="fixed", join = F) %>% # phrases
    #tokens_compound( phrase(flags), valuetype="fixed", join = F, concatenator = "") %>%  # special: flags to be rejoined after spacy murdered them. hopefully works. ---> didn't work, fixed above.
    tokens_remove(
      c('"', "'", "-", "+", "!", "?", ",", ":", ";", ".", "=", "(", ")", "[", "]", "—", "%", "~", "*", "•", "–", "_", "|", " ", ""),
      valuetype="fixed") %>% # clean punctuation
    # (even if would keep, not comparable, as preparsing necessarily kills off some already)
    as.list() %>% 
    lapply(function(x) gsub("[| ]","",x)) %>%  # !  remove hashtag and phrase protectors
    .[sapply(., length)>0] %>%   # remove final empty tweets if any after cleaning
    as.tokens()
  
  extras = lem2 %>% .[(names(.) %in% news) ]  # separate extra-corpus of news&other
  lem2 =   lem2 %>% .[!(names(.) %in% news) ] # and remove from main
  
  try({  # doesnt work
    attr(lem2, "phrases") = phr
    attr(lem2, "multiword") = multiword
    attr(lem2, "extras") = extras  # send via attr, extract later
  })
  return(lem2)
}




tweetfilter_spam = function(tweets){
  tweets = tweets %>% 
    filter(!(user_id %in% 
               c("30245355", "1315724329", "1262841084935311360", "1931184464", "200381071","353368454","1931184464",
                 "1457376757313687555","1157720426166018051","2914629843","1088222072944377861","1066469679542071296","993590061894529024","21604764","964223300154150915","928283661677539328","2861842401","1267873218","864532106336235521","842095599100997636","741677886248226816","13792562","730670109824417792","18284543","53543144","4874838393","14941201","4855573727","119217218","3369547257","976496018895310848","606776750","829186945267834880","824654940882825218","794674561069420545","3256880023","555129716","66336739","1430344376266407941","1370469884379926531","1358912438117220354","1349823652645261312","1288226864457084928","1284136324790878209","900908184486854656","1100754444508151808","1052306397247889408","1901546563","1473699590","948061059998416896","895814938995957760","1067347077959729152","3772896857","3303517698","122264472","1909219404","2611870982","1705052335","2647139192","10729632","2216299843","2508960524","1170208442","2212096494","2883048011","2883171442","1407064677734039553","2918046094","1406007181","1021450103058501632","39537361","16372127","12750972","370094706","2422679658","780208672827842560","165568433","598443842","1125887570","595609536","585167005","1028578680187052032","237768112","20528077","20528163","20041451","580191183","94235855","1265400483247767552","2183148738","2727033394","1300929143773724675","599283881","1399489769646694401","1409397742837764098","1469590486541213697","1309775766901518336","769398218","769341457","21294385","1003333397912195072","181542270","15269964","852564048541503489","1424083397442424839","17862527","20527935","2870953361","20527857","223451848","21110041","223454029","28635395","29211557","21021326","14364384","15309804","20589462","12349612","300781487","300776500","1088156485","20589321","300790855","21100130","437009630","13918492","300798024","20589232","805511952076259329","726414503420780544","589561485","861481999","834563918","1096517138041454592","99559129","91287877","20705412","946595851","53868363","28390125","16259594","222490156","21793505","851342335224852480","423235137","21287753","135450080","966127496738504705","1254922537227730945","726208034683244545","11714012","22742382","457112019","3597959885","20596281","4275102862","21702265","499359329","74565896","1485119407684685825","104213417","2254990015","811298705278193664","880456537474519040","2361881604","607875269","1174212689964539904","136093603","97334488","88498598","99365098","994438040763461632","121639467","36327065","123891232","920184279748960256","114968299","105007356","1165731066","785344759","107315355","146319328","81471317","90540771","94694970","90542413","90303826","80994311","139910208","98913977","90336986","110200567","113210853","109125981","112315040","82533442","108796629","233370941","81427689","109126744","95549240","170498144","90117873","90779112","91253534","114052881","3897288074","81701489","145460790","94902670","94460755","97363770","115843693","135596944","106522404","15304100","93294649","739702460","889905956020252676","1105208411853975553","863291717352542208","94186834","3158157865","819687603045707776","387794556","822778748","740942321470971906","19076243","2440823659","1007597875877089282","994660666706481152","599298339","21072306","844969843195568128","970815571041415168","68667716","34383589","293325995","965789418786381824","1414878481503211521","1047957382905487360","550369381","280404413","2231959346","1707098202","878668911729881088","269754683","715081724","18391051","26003862","82960432","599632006","1478673436134166533","596687292","2366043722","596964216","3300792155","376207434","720863642","1713505176","882759681667588096","702278455","2468225130","70778108"
                 ))) %>%  # remove some obvious spam accounts, and weather bots
    filter(nchar(text)<=90 | (nchar(text)>90 & !duplicated(paste(user_id, text) ))) %>% 
    filter(!grepl("followed me // automatically checked by|^Thank God For Another Day|^Hey yo, just letting you know that I went live on|I am Available for the next 6 Hours on https://t.co/7DTstdjx3S|tracked by https://t.co/t9FW1xD29H|^Just posted a photo (@|http)|^Just posted a video (@|http)|https://t.co/UCdK5pQJVa|@ThreadReaderApp|@RemindMe_OfThis|@everycolorbot|@tinycarebot|@MakeitAQuote|@quotedreplies| pero | para | una |  por | que | porqué | qué | está | esta | eso | aqui | como | vamos | muy | estoy | tengo | quiere | acá |MyTwitterAnniversary", text, ignore.case = T)) # remove bot-like messages, likely automated; also spanish
  x = str_extract(tweets$text, "https://([^[:space:]])+") 
  tweets = tweets[!(x %in% {x%>% table() %>% sort() %>% tail(30) %>% names()}), ] # remove spammy posts
  # also remove links and @names (lots of tweets are just @reply and another username)
  tweets$text = tweets$text %>% 
    gsub("http[s]*://[^[:space:]]+", " ", ., ignore.case = T) %>%  # remove links
    gsub("(@[^@^[:space:]]{3,})+", " ", .)  %>%  # remove usernames
    gsub("[[:space:]]+", " ", .) %>%  # homogenize whitespaces
    gsub("^ $", "", .)      # if just space left, make 0;  
  tweets = tweets %>% filter(nchar(text)>0)
  names(tweets$text)=tweets$status_id  # important
  #tweets %>% group_by(side) %>% count() %>% print()
  return(tweets)
}



dofreqs = function(lem2, tw, okusers, wprop, wusers){
  # sides=c("left", "right")
  v = list()
  lv = lem2 %>% as.list() %>% .[tw$user_id %in% okusers & tw$side == "left"]
  rv = lem2 %>% as.list() %>% .[tw$user_id %in% okusers & tw$side == "right"]
  v[[1]] = lv  %>% itoken( progressbar = FALSE) %>% create_vocabulary()
  v[[2]] = rv  %>% itoken( progressbar = FALSE) %>% create_vocabulary()
  
  s = sapply(v, function(x) sum(x$term_count) ) 
  freqs = full_join(
    v[[1]] %>% select(term, term_count, doc_count),
    v[[2]] %>% select(term, term_count, doc_count),
    by="term", suffix=c("_l", "_r") ) %>% 
    mutate(term_count_l=replace_na(term_count_l, 0), 
           term_count_r=replace_na(term_count_r, 0),
           doc_count_l=replace_na(doc_count_l, 0),
           doc_count_r=replace_na(doc_count_r, 0)
    ) %>% 
    filter( !(nchar(term)==1 & grepl("[[:alpha:]]", term))) %>%  # single letters/numbers
    left_join(data.frame(prop=wprop, term=names(wprop)), by="term") %>% 
    left_join(data.frame(nusers=wusers, term=names(wusers)), by="term")  
  attr(freqs,"s")=s
  
  freqs = freqs %>% 
    mutate(fln = term_count_l/attr(freqs,"s")[1]*1000000, # per mil
           frn = term_count_r/attr(freqs,"s")[2]*1000000) %>% 
    mutate(fnlm = (log(frn)+log(fln))/2) %>%  # mean log freq
    mutate(fnla = abs(log(frn)-log(fln))) %>% # abs of log dif
    mutate(log2dif = log2(frn/fln) ) %>%          # log2 difference (same as n times)
    mutate(
      doc_count_l_n=doc_count_l/length(lv)*1000000,
      doc_count_r_n=doc_count_r/length(rv)*1000000
    ) %>% 
    mutate(log2dif_doc = log2(doc_count_r_n/doc_count_l_n) ) # log2 difference (kind of like n times, 1=2x, 2=4x, basically 2^x to get times)
  rm(lv, rv)
  
  #colnames(freqs)[2:3] = c("fl", "fr")
  bigvoc = freqs %>% filter(term_count_r>=5 | term_count_l>=5) %>% 
    pull(term) # larger voc for doc2vec pre-embedding
  freqs = freqs %>% 
    mutate(ok=
             case_when(
               ((prop>0.75 | term_count_r>1000 | term_count_l > 1000) &   # if low freq require decent user-freq ratio 
                  (term_count_r>5 & term_count_l>5) & # for freq comp, require at least some n
                  (doc_count_l_n > 5 & doc_count_r_n > 5) &   # same with tweet freq
                  (term_count_r>=100 | term_count_l>=100) & # for sem comp, one needs to be ok (filtered below)
                  (nusers >= 100) &   # require usage by min n users
                  # spanish and single-letter flag components (should be fine now though)
                  !(term %in% c(
                    "pero", "para", "una", "por", "que", "los", "se", "el", "lo", "de", "es", "en", 
                    "🇦","🇧","🇨", "🇪", "🇫", "🇬", "🇭", "🇮", "🇲", "🇳", "🇵", "🇷", "🇸", "🇹", "🇺", "🇽",
                    "[", "]", "-", "–", "_")) # should be gone now though
               ) ~ T, T~F )) 
  
  freqs=as.data.frame(freqs)
  rownames(freqs)=freqs$term
  attr(freqs, "bigvoc") = bigvoc
  return(freqs)
}




doglove = function(lem2, side, wprop, tw, okusers){
  it = lem2 %>% as.list() %>% .[tw$user_id %in% okusers & tw$side == side]  %>% 
    itoken( progressbar = FALSE)
  vl = create_vocabulary(it)
  tcm = create_tcm(it, vocab_vectorizer(vl %>% prune_vocabulary(term_count_min=5)), 
                   skip_grams_window = 5) # weights = rep(1,6)
  tcm = names(wprop[wprop>0]) %>% {tcm[rownames(tcm)%in% . , rownames(tcm)%in% .]} 
  diag(tcm)=0
  glove = GlobalVectors$new( rank = 300, x_max = 100, lambda = 1e-5, learning_rate = 0.12)
  vecs = glove$fit_transform(tcm %>% drop0(), n_iter = 1000, convergence_tol=0.001, n_threads = 16)
  vecs = vecs + t(glove$components) # the recommended word+context vector summing
  attr(vecs, "v") = vl
  return(vecs)
}

compsims = function(w, vecr, vecl, n){
  sim2(vecr, vecr[w,,drop=F])[,1] %>% sort(decreasing = T) %>% head(n) %>% .[-1] %>% names() %>% cat()
  cat("\n")
  sim2(vecl, vecl[w,,drop=F])[,1] %>% sort(decreasing = T) %>% head(n) %>% .[-1] %>% names() %>% cat()
}



prepare_umap = function(um,tf, minp, lda_model, tops){
  # generates keywords using lda topics and tfidf on dbscan clusters (seems better)
  db0=dbscan(um[, c("V1", "V2")], eps=0.1, minPts=minp )
  um$db = db0$cluster %>% as.factor()
  print(paste(db0$cluster %>% unique %>% length(), "clusters", sum(db0$cluster==0), "noise" ))
  keyw = lda_model$get_top_words(n = 100, lambda = 0.5)
  bwords = tibble()
  tf2=as.matrix(tf) %>% .[,!(colnames(.)%in% c(stopwords(),"don_t", "won_t", "doesn_t") )]
  rownames(tf2) = tf@Dimnames[[1]]
  for(i in setdiff(levels(um$db), "0")){
    x = um %>% dplyr::filter(db==i)
    ti = 
      tops[x %>% pull(user_id),] %>% 
      colSums() %>% 
      order(decreasing = T) %>% 
      #setdiff(., bwords$tops) %>% 
      .[1]
    kw = keyw[,ti] %>% 
      #setdiff(.,bwords$keyw) %>% 
      .[nchar(.)>3] %>% .[1:2]
    kw2 = tf2[intersect(x %>% pull(user_id), rownames(tf2)),] %>% colSums() %>% sort(decreasing = T) %>%   # the intersect is a bit hacky, some users missing?
      names() %>%  .[nchar(.)>3] %>% 
      head(2)
    bwords = rbind(bwords,
                   tibble(keyw=kw,keyw2=kw2, V1=median(x$V1), V2=median(x$V2), 
                          tops=ti, clus=i, n=nrow(x), 
                          #side=names(sort(table(x$side),decreasing = T))[1] 
                          col= table(x$side) %>% {./sum(.)} %>% .["left"] %>% ifelse(is.na(.),0,.)
                   ) 
    )
  }
  return(bwords)
}



#### plotters ####

comparisonplot = function(freqs1, xvar="log2dif_doc", ymx=31){
  freqs1 = freqs1 %>% rename(xvar=xvar) 
  g = g=ggplot(freqs1 %>% filter(big) ,
               aes( xvar, nusersp, label=term, color=xvar, size=sizemult))
  if(xvar=="log2dif_doc"){
    g=g+geom_vline(xintercept=c(-4,-3,-2,-1,0,1,2,3,4), size=0.1, color="gray70")
  } 
  g=g+
    geom_hline(yintercept=c(5,10,30), size=0.1, color="gray70")+
    #geom_vline(xintercept=c(-4,-3,-2,-1,0,1,2,3,4), size=0.1, color="gray70")+
    geom_point(data=freqs1, size=0.01, alpha=0.5)+
    geom_text(data=freqs1 %>% filter(!big, term!="🇺🇸") %>% sample_n(1000), size=1.6 , lineheight=0.45,hjust=0.5) +
    geom_text_repel(alpha=0.9,  hjust=0, vjust=0.5,
                    box.padding = unit(0, "lines"), point.padding=unit(0, "lines"),
                    min.segment.length = 0.2, max.time = 5, max.overlaps = 10, direction = "both", segment.size=unit(0.1,"mm"), segment.alpha=0.5, lineheight=0.45, force=0.11
    )+
    ggflags::geom_flag(aes(xvar, nusersp, country="us"), data=freqs1 %>% filter(term=="🇺🇸"), size=3, inherit.aes = F)+
    
    #,limits = c(-max(abs(freqs1$log2dif)),max(abs(freqs1$log2dif))))+
    scale_y_continuous( breaks=c(2,3,4,5,10,20,30),
                        #breaks=c(500,1000,5000),
                        #breaks=c(seq(200,1000,100),seq(2000,5000, 1000)), 
                        trans="log10", expand=c(0,0), 
                        limits=c(min(freqs1$nusersp)-0.06, ymx)
    ) +
    annotation_logticks(sides="l", size=0.3, 
                        short = unit(.5,"mm"),
                        mid = unit(1,"mm"),
                        long = unit(1.5,"mm")) +
    
    
    #diverging_hcl(11, palette = "Blue-Red")  %>% {.[6]="gray50";.}, limits = c(-4.9,4.9))+
    #scale_color_viridis_c(option = "B", end = 0.9,direction = -1 )+
    #ylim(c(-10, 10))+
    labs( 
      y=expression(Percentage~of~users~~"(note "*log[10]*" scale)"))+
    theme_bw()+
    theme(panel.grid = element_blank(),
          #panel.border = element_blank(),
          axis.line = element_line(size=0.2, color="gray50"),
          legend.position = "none",
          axis.ticks.y = element_blank(),
          axis.ticks.length.y = unit(0,"pt"),
          plot.title = element_text(size=8, margin=margin(1,0,1,0, unit="pt")),
          axis.title = element_text(size=8, margin=margin(0,0,0.1,0, unit="pt")),
          axis.text=element_text(size=8),
          plot.margin = margin(0.1,0.1,0.1,1, unit="pt")
    )
  if(xvar=="log2dif_doc"){
    xl= c(-max(abs(freqs1$xvar)), max(abs(freqs1$xvar)))
    g=g+scale_x_continuous(
          limits=c(-4, max(abs(freqs1$xvar))+0.01 ), expand=c(0,0),
          breaks=c(-4,-3,-2,-1,0,1,2,3,4), 
          labels=c("16x","8x", "4x left", "2x more in left", "=", "2x more in right", "4x right","8x", "16x" ), 
          name = expression(Log[2]~~tweet~frequency~difference)
          )+
      scale_color_gradientn(
        colors=c(rep("#023FA5",2), "#6A76B2", "gray60", "#B16273", rep("#8E063B", 2)),
        limits =xl)+
      scale_size(range = c(1.5,2.4),  guide = "none")
    
  }
  if(xvar=="semdif"){
    g=g+scale_x_continuous(limits=c(0, max(freqs1$xvar)+0.001), expand=c(0,0), 
                           name="Cosine distance (higher = more divergent)")+
      scale_color_gradientn(colors=c(rep("gray70",3), "gray40", rep("gray10",2)))+
      scale_size(range = c(1.9, 1.9),  guide = "none")+
      labs(title="(a) Embeddings-based divergence")
  }
  
  g
  
}
