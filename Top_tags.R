
#--------Functions Description
# Get_Tags(url) -  function returns top 30 common tags for items from the url + tags visualisation (using wordcloud package)

# Get_Items_Url(url, page_number_top) - return tbl with columns: name_id - name with unique number from item page url
# if url contains more than one page of items, then function return items name_id of the first 3 pages ( page_number_top ). 
# page_number_top = 3 by default - most popular items are on first 3 pages
#----------

Get_Items_Url <- function(url, page_number_top = 3){
 item_data_url <- "https://audiojungle.net/item/"
 page_number_max <- 60 # maximum number of pages
 items_on_page <- 30 # maximum number of items on page
 page <- "Royalty Free Music Tracks from AudioJungle.html"
 
 # Step 1: check if url contains 2 or more pages then get all urls of each page
 # split url into 2 parts by sepr: url_begin and url_end 
 if (grepl("date", url, fixed = TRUE)) {
 	sepr_page <- "&page="
 	if (grepl("date=this-year", url, fixed = TRUE)) sepr <- "date=this-year"
 	if (grepl("date=this-month", url, fixed = TRUE)) sepr <- "date=this-month" 
 	if (grepl("date=this-week", url, fixed = TRUE)) sepr <- "date=this-week" 
 	if (grepl("date=this-day", url, fixed = TRUE)) sepr <- "date=this-day" 
 } else { 
 	if ( grepl("page=", url,) || grepl("sort=", url) || grepl("view=", url) || grepl("vocals_in_audio", url) || grepl("rank", url) ||grepl("tempo", url) || grepl("include_pro_affiliated", url) ||grepl("tags", url) || grepl("price", url) )  {
 		sepr <- "?"
 		sepr_page <- "page="
 		sepr_and <- "&"
 	}
 	else sepr_page <- "one_page" 
 } 
 if (!sepr_page == "one_page") {
  url_begin_end <- strsplit(url, sepr, fixed = TRUE) # split url
  url_begin <- lapply(url_begin_end, `[[`, 1)
  url_end <- lapply(url_begin_end, `[[`, 2)
  if (sepr_and == "&") url_end <- paste0(sepr_and, url_end)
  webpage <- read_html(url)  
  total_items <- html_nodes(webpage, "._3HYTF") # total number of items 
  total_items <- as.numeric(gsub(',', '', html_text(total_items))) 
  page_number <- ceiling(total_items / items_on_page) # total number of pages
  if (page_number > page_number_max) page_number <- page_number_max
  url <- sapply(seq(1:page_number), function(m){
   		list_page <- paste0(url_begin, sepr, sepr_page, m, url_end) 
   }) #get url for each page	
 } else  page_number <- 1 # if url contains 1 page	
 
 # Step 2: download pages from url and create df_current_trend with name_id (name with unique number from item page url)
 if (page_number > page_number_top) page_number <- page_number_top # take not more pages than page_number_top
 print("page downloading:")
 for (n in seq(1:page_number))
 { 
  if (n < 2)  page <- "Royalty Free Music Tracks from AudioJungle.html" else page <- paste("Royalty Free Music Tracks from AudioJungle (Page ", n, ").html", sep = "")
  print(page) 
  download.file(url[n], destfile=page, quiet=TRUE)
  webpage_d <- read_html(page) 
  item_names <- html_nodes(webpage_d, "._2Pk9X") # item names 
  item_href <- item_names%>% html_attr("href") # item href
  item_name_id <- gsub(item_data_url, "", item_href)   
  tmp <- data.frame(name_id=item_name_id, dt=Sys.Date(), stringsAsFactors=FALSE)
  if (exists("df_current_trend")) df_current_trend <- rbind(df_current_trend, tmp) else df_current_trend <- tmp	 
 } 
 print("End of page downloading.")
  
 df_current_trend <- df_current_trend[order(df_current_trend$name_id), ] 
 # Delete double rows in df_current_trend:
 i <- 1
 k <- nrow(df_current_trend)
 while (i < k) {
	if (df_current_trend[i, "name_id"] == df_current_trend[i + 1, "name_id"]) {
		df_current_trend <- df_current_trend[-i, ]
		k <- nrow(df_current_trend)
	} else i <- i + 1	
 }   
 # Delete items like "-french-jazz-routine/3929379" in df_current_trend:
 i <- 1
 while (i <= k) {
	if (substring(df_current_trend[i, "name_id"], 1, 1) == "-") {
		df_current_trend <- df_current_trend[-i, ]
		k <- nrow(df_current_trend)
	} else i <- i + 1	
  }   
 cat("Number of items = ", k)
 return(df_current_trend)
}

#----------
Get_Tags<-function(url){
 df_current_trend <- Get_Items_Url(url)
 item_data_url <- "https://audiojungle.net/item/"
 page <- "Royalty Free Music Tracks from AudioJungle.html"	
 
# Create tags_count with columns: name_id - name with unique number from item page url
 tags_count <- data.frame(tag = " ", count = 0)
 k <- nrow(df_current_trend)
 url <- sapply(seq(1:k), function(n){list_page <- paste0(item_data_url, df_current_trend[n, "name_id"]) }) #take url for each trend item
 
#Start loop - scrap tags
print("Reading tags from:")
for (n in seq(1:k))
{
 try_download <- try(download.file(url[n], destfile = page, quiet=TRUE), silent = TRUE)
 if(is(try_download,"try-error")){
      print(paste0("ERROR: ", url[n]))
  } else { # start of try else  
    cat("n=", n, ",", url[n], " ") 
    webpage_item <- read_html(page)
 	item_page_tags <- html_nodes(webpage_item, ".meta-attributes__attr-tags")
	item_page_tags <- gsub("\\s", "", gsub('\n','', html_text(item_page_tags)))	  
    if (!(length(item_page_tags) == 0))  {   #check if tags exist
        	tgs <- strsplit(item_page_tags, split=',', fixed=TRUE) 
        	df_tmp <- data.frame(matrix(unlist(tgs), ncol=1, byrow=F))	
            m <- nrow(df_tmp)
  		  	for (i in seq(1:m)) #check each tag in tags_count   
  		  		
        	# Var1: create tags_count with unique column tags, count (of repeated tags)
  			{
  			   	 j <- match(df_tmp[i, ], tags_count[, "tag"]) #find the tag in tags_count 
  			   	 if (!is.na(j)) tags_count[j, "count"] <- tags_count[j, "count"] + 1 else { 
  			   	 	tags_count <- rbind(tags_count, data.frame(tag = df_tmp[i, ],count = 1)) 
  			   	  }	   	  
  			}
  			tags_count <- tags_count[order(tags_count$count, decreasing=TRUE), ]  
  						
  			#Var2: create tags_count with NOT unique column tags, count = 1 
  			# {
  				# tags_count <- rbind(tags_count, data.frame(tag = df_tmp[i, ],count = 1))
  			# }
  			# tags_count <- tags_count[order(tags_count$tag, decreasing=TRUE), ]			
     } 
  } # end of try else    
 }
#End loop - scrap tags 
 return(tags_count)
}
#-------------

 # Install 
library(XML) 
library(xml2)
library(rvest)
library(selectr)
library(xml2)
library(jsonlite)
library(tidyverse)
library(curl)
library("wordcloud")
library("RColorBrewer")
library(sqldf)  
 
#examples of url:
#url <- "https://audiojungle.net/category/music/jazz?page=2&sort=trending&view=list"
#url <- "https://audiojungle.net/popular_item/by_category?category=music/jazz"
#url <- "https://audiojungle.net/top-sellers"
#url <- "https://audiojungle.net/feature"
#url <- "https://audiojungle.net/category/music/ambient/new-age?date=this-year&price_max=11&sort=date#content"
#url <- "https://audiojungle.net/category/music/ambient/new-age?sales=rank-1&vocals_in_audio=background%20vocals/harmonies#content"

 start.time <- Sys.time()
 df <- Get_Tags(url)
 end.time <- Sys.time()
 time.taken <- end.time - start.time

# for Var1       
 print("Top 30 tags: ")
 print(df[1:30, ]) 
 
# # for Var2 - 1
 # df%>%group_by(tag)%>%summarise(count=n())%>%filter(count>1) # using tidyr/dplyr
# # for Var2 - 2 
 # df <- sqldf("SELECT tag, COUNT(*) AS count FROM df GROUP BY tag HAVING COUNT(*) > 1 ORDER BY count DESC")

#Find the most popular tags from the url - Visualization:
 set.seed(1234)
 wordcloud(words = df$tag, freq = df$count, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2")) 

 # barplot(df[1:30,]$count, las = 2, names.arg = t[1:30, ]$tag,
        # col ="lightblue", main ="Most frequent tags for trending items",
        # ylab = "Tags frequencies")




