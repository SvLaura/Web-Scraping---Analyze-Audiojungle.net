# Web-Scraping - Analyze-Audiojungle.net

 Data analysis of website audiojungle.net using R. As a result to make reccomendations for authors and answer questions:
1. What tags to write during uploading a new track? 
2. When should upload tracks? What day of the week (month) has the most sales?
3. Is there any correlation between sales and count and type of author badges,  comments,  rates ?


1.
Top_tags.R - scrap data from audiojungle.com for understanding what tags are most demanded (from the passing url ). If url contain more than one page - only first 3 pages will be taken.
Examples of the urls:
 https://audiojungle.net/popular_item/by_category?category=music/jazz - 2020 Best Selling Royalty Free Jazz Music Tracks - updated weekly.
 https://audiojungle.net/category/music/jazz?date=this-year&sort=trending#content - Trending items in Jazz Added in the last year
