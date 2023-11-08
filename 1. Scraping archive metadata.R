require(rvest)
require(dplyr)
require(tidyverse)
require(RSelenium)
require(glue)

#set date of scraping to save scraping record in dated folder
date <- format(Sys.time(), "%Y_%m")

#define username and password for your account with the decisions archive
username <- "username"
password <- "password"
  
#This is the code for running 
#docker run -d -p 4445:4444 -p 5901:5900 selenium/standalone-firefox-debug:2.53.0
#screen sharing 0.0.0.0:5901

#initiate server, navigate to login page ####
remDr <- remoteDriver(remoteServerAddr = "0.0.0.0",port=4445L)

#Works on some different setups of 
#remDr <- remoteDriver(remoteServerAddr = "localhost",port=4445L)
#remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",port=4445L)

remDr$open(silent = TRUE)
remDr$navigate("https://decisions.refappeal.ie")

#fill in login info
webElem <- remDr$findElement(using='id',value="Username")
webElem$sendKeysToElement(list(username))

webElem<-remDr$findElement(using='id',value="Password")
webElem$sendKeysToElement(list(password))

webElem<-remDr$findElement(using='css selector',value="input#Action")
webElem$clickElement()

#click login
webElem<-remDr$findElement(using='id',value="submit")
webElem$clickElement()

#go to search page
webElem <- remDr$findElement(using='class name',value="boldbuttons")
webElem$clickElement()

webElem <- remDr$findElement(using='class name',value="SearchButton")
webElem$clickElement()

#function for later
extract2 <- function(x,li) x[[li]]

#webElem <- remDr$findElement(using='xpath',value='//tbody/tr/td[4]/select/option[3]')
# this function will get the tables from years, ranging 2001 - 2020 ####

#Get list lengths for year and country
#find list of years
#webElem <- remDr$findElements(using='xpath',value="//tbody/tr/td[4]/select/option")


for (j in 25:3) {
  #25:3 scrapes yearse 2023-2001
  
  #enter year
  c <-'//tbody/tr/td[4]/select/option['
  d <-']'  
  yrgetter <- paste0(c,j,d)
  webElem <- remDr$findElement(using='xpath',value=yrgetter)
  webElem$clickElement()
  year <- webElem$getElementText()
  
  for (i in 3:154) {  #Double check!! to see if number of countries has changed
    #3:154
    
    #enter country i
    a <- '//tbody/tr/td/select/option['
    b <- ']'
    ctygetter <- paste0(a,i, b)
    webElem <- remDr$findElement(using='xpath',value=ctygetter)
    cntry <- webElem$getElementText()
    webElem$clickElement()
    #select year
    webElem <- remDr$findElement(using='xpath',value=yrgetter)
    webElem$clickElement()
    #show all
    webElem <- remDr$findElement(using='xpath',value="//tbody/tr[3]/td[4]/select/option[4]")
    webElem$clickElement()
    
    #perform search
    webElem <- remDr$findElement(using='xpath',value="//tfoot/tr/td[2]/input")
    webElem$clickElement()
    
    #get results
    webElem <- remDr$findElement(using='xpath',value="//h1/span[2]")
    webElem$getElementText() -> result #this is the text of number of results
    
    #write number of results to year results.txt"
      #results <- paste('results.txt')
      #paste(i,year,cntry,result, sep = ",") %>% 
      #  write.table(file=glue(date,"/",results), 
      #              append=TRUE, col.names = FALSE)
    
    results <- paste('results.txt')
    paste0('"',i,'","',
           year,'","',
           cntry,'","',
           result, '"') %>% 
    write(file=glue(date,"/",results), 
          append=TRUE)
     
    #write the html table to "year decisions.txt"
    remDr$getPageSource()[[1]] %>% read_html %>% html_table(fill=TRUE) %>% extract2(3) %>% 
      write.table(file=glue(date,"/decisions.txt"), append=TRUE, col.names = FALSE)
    Sys.sleep(.05)
    print(paste0(cntry," ",i," ",year, " ", j," ", result))
  }
  #after one year, this logs out and logs back in again before starting the next year
  webElem <- remDr$findElement(using='xpath',value="//a[contains(@href,'/website/romda/romda.nsf?LogOut')]")
  webElem$clickElement()
  
  Sys.sleep(1)
  
  remDr$getCurrentUrl() %>% print()
  print('logged out')
  print(i)
  print(year)
  
  webElem<-remDr$findElement(using='id',value="Username")
  webElem$sendKeysToElement(list("sasha.brown.2016@mumail.ie"))
  
  webElem<-remDr$findElement(using='id',value="Password")
  webElem$sendKeysToElement(list(password))
  
  webElem<-remDr$findElement(using='css selector',value="input#Action")
  webElem$clickElement()
  
  webElem<-remDr$findElement(using='id',value="submit")
  webElem$clickElement()
  
  Sys.sleep(1)
  
  webElem <- remDr$findElement(using = 'xpath', value = "//div[@id='nav01']/ul[1]/li[1]/a[1]")
  webElem$clickElement()
  
  print('about to click new search')
  
  Sys.sleep(5)
  
  webElem <- remDr$findElement(using='class name',value="boldbuttons")
  webElem$clickElement()
  
  
  webElem <- remDr$findElement(using='class name',value="SearchButton")
  webElem$clickElement()
}

#2nd scrape - Time to identify over 250 country/year combinations ####
read_csv(glue(date,"/results.txt"), col_names = F) -> results_read_in

colnames(results_read_in) <- c("cntry_code", "year", "country", "displaying")

results_read_in %>% 
filter(displaying == "Displaying: 1 to 250 of 250") %>% 
mutate(year_code = year - 1998)  -> results_over250


#Make a search that uses the above variable to make searches with each decision/outcome and extract all


#2nd scrape for the countries/year with more than 250 for years 2006 to 2011 ####

#use rows from the results_over250 object
for (j in 3:nrow(results_over250)) {
  
#i is for decision/outcomes  3:14
for(i in 3:14){
  #enter Country
  a <- '//tbody/tr/td/select/option['
  b <- ']'
  country <- paste(results_over250$cntry_code[j]) 
  ctygetter <- paste0(a,country, b)
  webElem <- remDr$findElement(using='xpath',value=ctygetter)
  cntry <- webElem$getElementText()
  webElem$clickElement()
  
  #enter year
  c <-'//tbody/tr/td[4]/select/option['
  d <-']'  
  year_code <- paste(results_over250$year_code[j])
  yrgetter <- paste0(c,year_code,d)
  webElem <- remDr$findElement(using='xpath',value=yrgetter)
  webElem$clickElement()
  year <- webElem$getElementText()
  
  #decision/outcome from i: in 3:14
  c <-'//tbody/tr[2]/td[4]/select/option['
  d <- ']'
  decgetter <- paste0(c,i,d)
  webElem <- remDr$findElement(using='xpath',value=decgetter)
  webElem$clickElement()
  decision <- webElem$getElementText()
  
  #show all
  webElem <- remDr$findElement(using='xpath',value="//tbody/tr[3]/td[4]/select/option[4]")
  webElem$clickElement()
  
  #perform search
  webElem <- remDr$findElement(using='xpath',value="//tfoot/tr/td[2]/input")
  webElem$clickElement()
  
  #get results
  webElem <- remDr$findElement(using='xpath',value="//h1/span[2]")
  webElem$getElementText() -> result #this is the text of number of results
  
  #write number of results to year results.txt"
  results <- paste0('results_second_scrape.txt')
  paste0('"',country,'","',
         year,'","',
         cntry,'","',
         result,'","',
         decision,'","',
         i,'"') %>% 
    write(file=glue(date,"/",results), 
          append=TRUE)
  
  name <- glue(date,"/decisions2.txt")

  remDr$getPageSource()[[1]] %>% read_html %>% 
    html_table(fill=TRUE) %>% extract2(3) %>% 
    write.table(file=name, append=TRUE, col.names = FALSE)
  
  print(paste0(cntry," ",year, " ", decision, " ", decision, " ", i))
  
}
}


#3rd scrape - identify over 250 country/year/decision combinations ####
read_csv(glue(date,"/results_second_scrape.txt"), col_names = F) -> results2_read_in

colnames(results2_read_in) <- c("cntry_code", "year", "country", "displaying", "outcome","outcome_code")

results2_read_in %>% 
  filter(displaying == "Displaying: 1 to 250 of 250") %>% 
  mutate(year_code = year - 1998)  -> results2_over250



#3rd scrape ####

#use rows from teh results2_over250 object
for (j in 1:nrow(results2_over250)) {
  
  #i is for appeal type 3:21
  for(i in 3:21){
    #enter Country
    a <- '//tbody/tr/td/select/option['
    b <- ']'
    country <- paste(results2_over250$cntry_code[j]) 
    ctygetter <- paste0(a,country, b)
    webElem <- remDr$findElement(using='xpath',value=ctygetter)
    cntry <- webElem$getElementText()
    webElem$clickElement()
    
    #enter year
    c <-'//tbody/tr/td[4]/select/option['
    d <-']'  
    year_code <- paste(results2_over250$year_code[j])
    yrgetter <- paste0(c,year_code,d)
    webElem <- remDr$findElement(using='xpath',value=yrgetter)
    webElem$clickElement()
    year <- webElem$getElementText()
    
    #decision/outcome from
    c <-'//tbody/tr[2]/td[4]/select/option['
    d <- ']'
    dec_outcome <- results2_over250$outcome_code[j]
    decgetter <- paste0(c,dec_outcome,d)
    webElem <- remDr$findElement(using='xpath',value=decgetter)
    webElem$clickElement()
    decision <- webElem$getElementText()
    
    #appeal type 3:21
    c <-'//tbody/tr[2]/td[2]/select/option['
    d <- ']'
    appealtype <- i
    appealgetter <- paste0(c,appealtype,d)
    webElem <- remDr$findElement(using='xpath',value=appealgetter)
    webElem$clickElement()
    type <- webElem$getElementText()
    
    #show all
    webElem <- remDr$findElement(using='xpath',value="//tbody/tr[3]/td[4]/select/option[4]")
    webElem$clickElement()
    
    #perform search
    webElem <- remDr$findElement(using='xpath',value="//tfoot/tr/td[2]/input")
    webElem$clickElement()
    
    #get results
    webElem <- remDr$findElement(using='xpath',value="//h1/span[2]")
    webElem$getElementText() -> result #this is the text of number of results
    
    #write number of results to year results.txt"
    results <- paste0('results_third_scrape.txt')
    paste0('"',country,'","',
           year,'","',
           cntry,'","',
           result,'","',
           decision,'","',
           type,'","',
           i,'"') %>% 
      write(file=glue(date,"/",results), 
            append=TRUE)
    
    name <- glue(date,"/decisions3.txt")
    
    remDr$getPageSource()[[1]] %>% read_html %>% 
      html_table(fill=TRUE) %>% extract2(3) %>% 
      write.table(file=name, append=TRUE, col.names = FALSE)
    
    print(paste0(cntry," ",year, " ", decision, " ", type, " ", i))
    
  }
}

#read in third scrape ####
read_csv(glue(date,"/results_third_scrape.txt"), col_names = F) -> results3_read_in

colnames(results3_read_in) <- c("cntry_code", "year", "country", "displaying", "outcome","appeal_type", "appeal_code")

results3_read_in %>% 
  filter(displaying == "Displaying: 1 to 250 of 250") %>% 
  mutate(year_code = year - 1998)  -> results3_over250



name <- glue(date,"/decisions_final.txt")

remDr$getPageSource()[[1]] %>% read_html %>% 
  html_table(fill=TRUE) %>% extract2(3) %>% 
  write.table(file=name, append=TRUE, col.names = FALSE)


