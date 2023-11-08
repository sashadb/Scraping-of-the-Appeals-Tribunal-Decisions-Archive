library(tidyverse)
library(glue)
require(rvest)
require(dplyr)
require(RSelenium)

#import cleaned list of decisions from step 1
#requires some cleaning of document from step 1, including deletion of duplicates and NA rows
#change 'file' below to filepath
decisions_in_archive <- read_csv("file")

#convert reference to be comparable to filename
decisions_in_archive <- decisions_in_archive %>%
  mutate(filename = Reference) %>%
  mutate(
    filename = str_replace_all(filename, "/", "-"),
    filename = str_to_lower(filename),
    filename = str_trunc(filename , 10)
  )

date <- format(Sys.time(), "%Y_%m")

#define username and password for your account with the decisions archive
username <- "username"
password <- "password"



#makes the downloads work and sends them to documents/atapdfs
#run this command below when running docker
#docker run -d -p 4445:4444 -p 5901:5900 -v ~/Documents/atapdfs/home/seluser/Downloads selenium/standalone-firefox-debug:2.53.1

#initiate server, navigate to login page ####
remDr <- remoteDriver(remoteServerAddr = "0.0.0.0",port=4445L)

#Works on other computers
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

login <- function(){
  #fill in login info
  webElem <- remDr$findElement(using='id',value="Username")
  webElem$sendKeysToElement(list("sasha.brown.2016@mumail.ie"))
  
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
}

relog <- function(){
  
  #logout and log back in
  webElem <- remDr$findElement(using='xpath',value="//a[contains(@href,'/website/romda/romda.nsf?LogOut')]")
  webElem$clickElement()
  
  Sys.sleep(2)
  
  #fill in login info
  webElem <- remDr$findElement(using='id',value="Username")
  webElem$sendKeysToElement(list("sasha.brown.2016@mumail.ie"))
  
  webElem<-remDr$findElement(using='id',value="Password")
  webElem$sendKeysToElement(list(password))
  
  webElem<-remDr$findElement(using='css selector',value="input#Action")
  webElem$clickElement()
  
  #click login
  webElem<-remDr$findElement(using='id',value="submit")
  webElem$clickElement()
  
  Sys.sleep(2)
  
  #go to search page
  webElem <- remDr$findElement(using='class name',value="boldbuttons")
  webElem$clickElement()
  
  Sys.sleep(1)
  
  webElem <- remDr$findElement(using='class name',value="SearchButton")
  webElem$clickElement()
}

logout <- function(){
  webElem <- remDr$findElement(using='xpath',value="//a[contains(@href,'/website/romda/romda.nsf?LogOut')]")
  webElem$clickElement()
}

# PDF Scraping ####
for (i in 1:nrow(missing)
     ){
#make search by reference number
  
  #textbox
  webElem <- remDr$findElement(using='xpath',value="//tbody/tr[4]/td[2]/input")
  webElem$sendKeysToElement(list(
    key = 'backspace',key = 'backspace',key = 'backspace',key = 'backspace',key = 'backspace',key = 'backspace',key = 'backspace',key = 'backspace',key = 'backspace',key = 'backspace',key = 'backspace',key = 'backspace',key = 'backspace',key = 'backspace',key = 'backspace',key = 'backspace',key = 'backspace',key = 'backspace',key = 'backspace',key = 'backspace'
  ))
  webElem$sendKeysToElement(list(missing$Reference[i]))
  
  #perform search
  webElem <- remDr$findElement(using='xpath',value="//tfoot/tr/td[2]/input")
  webElem$clickElement()
  
#if pdf, click link
  1 -> k
  a <- "//tr[(((count(preceding-sibling::*) + 1) ="
  b <-") and parent::*)]//a"
  pdfgetter <- paste (a,k,b)
  #find next decision link
  webElem <- remDr$findElements(using='xpath', value = pdfgetter)
  #if there is another one, click and get it. Otherwise, don't
  
  if(length(webElem) > 0){
    webElem <- remDr$findElement(using='xpath', value = pdfgetter)
    webElem$clickElement()
  
  
#click on pdf link
  print(i)
  Sys.sleep(1.7)
  
#record case details
  newEmptyObject() -> case
  #get country, year, type, decision, reference, pdf info
  webElem <- remDr$findElement(using='xpath', value = "//div/div/div/div/div/div/table/tbody/tr/td[2]")
  case$country <- webElem$getElementText()
  webElem <- remDr$findElement(using='xpath', value = "//div/div/div/div/div/div/table/tbody/tr[2]/td[2]")
  case$year <- webElem$getElementText()
  webElem <- remDr$findElement(using='xpath', value = "//div/div/div/div/div/div/table/tbody/tr[3]/td[2]")
  case$type <- webElem$getElementText()
  webElem <- remDr$findElement(using='xpath', value = "//div/div/div/div/div/div/table/tbody/tr[4]/td[2]")
  case$decision <- webElem$getElementText()
  webElem <- remDr$findElement(using='xpath', value = "//div/div/div/div/div/div/table/tbody/tr[5]/td[2]")
  case$reference <- webElem$getElementText()
  webElem <- remDr$findElement(using='xpath', value = "//div/div/div/div/div/div/table/tbody/tr[6]/td[2]")
  case$pdf_info <- webElem$getElementText()
  
  #write to file
  paste(c(case[[1]],case[[2]],case[[3]],case[[4]],case[[5]],case[[6]]), collapse = "    ") %>% 
    write.table(file="pdfs/decisions_info.txt",append=TRUE, col.names=FALSE)
  print(case$pdf_info)
  
  #locate pdf link
  webElem <- remDr$findElements(using='partial link text', value = 'PDF')
  
  #if it is a link click it
  if(length(webElem) ==1) {
    webElem[[1]]$clickElement()
    Sys.sleep(.4)}

#go back
  remDr$goBack()
  
  } else{
    print("no result")
  }

print(i)
print("done")
#wait
  Sys.sleep(.2)

  if (i%%50==49){
    print("about to relog")
    relog()
    print("relogged in")
  }
}



