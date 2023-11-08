library(pdftools)
library(purrr)
library(tidyverse)
library(tidytext)
library(dplyr)

library(stringr)
library(magick)
library(tesseract)
library(glue)



#First pass OCR function ####
#function
trib_member <- function(file) {
 # cat(glue('file {file} | '))
  print({file})
  #specify file location of files
  pages <- pdf_info(glue("pdfs/{file}"))
  input <- image_read_pdf(glue("pdfs/{file}"),density=150, pages = pages$pages) %>% 
  tesseract::ocr() %>% 
  strsplit('\n') %>% 
  flatten_chr()
  
  member <- input %>% 
    str_detect("Member of .*Tribunal*")
  
  #get name of TM from line above string detect, if blank go up one extra line. 
  if(any(member)) {
    member_name <- input %>%
      str_detect( "Member of.*Tribunal*") %>%
      which() %>% #if this value is 1 then either the tm is on the previous page or it is censored
      -1 %>% max(1) %>%  {input[.]}
    
    if (member_name == "")   member_name <- input %>%
        str_detect("Member of.*Tribunal? *") %>%
        which() %>% -2 %>% max(1) %>% {input[.]}
  } else{#For NA case, do process over again in previous page
    pages <- pdf_info(glue("pdfs/{file}"))
    input <- image_read_pdf(glue("pdfs/{file}"),density=150, pages = (pages$pages-1)) %>% 
      tesseract::ocr() %>% 
      strsplit('\n') %>% 
      flatten_chr()
  
    member <- input %>% 
      str_detect("Member of.*Tribunal*")
    
    #get name of TM from line above string detect, if blank go up one extra line. 
    if(any(member)) {
      member_name <- input %>%
        str_detect("Member of.*Tribunal*") %>%
        which() %>% -1 %>% max(1) %>%  {input[.]}
      
      if (member_name == "")   member_name <- input %>%
          str_detect("Member of.*Tribunal*") %>%
          which() %>% -2 %>% max(1) %>% {input[.]}
      #For NA case, do process over again in previous page
    } else{NA -> member_name}
    
    
  }
  return(member_name)
}

#get files name
files <- dir('pdfs')

#run the function ####
timestart <- Sys.time()
tmems <- map_chr(files,trib_member)
elapsed <- Sys.time() - timestart
elapsed

summary(tmems)

members <- data_frame(document=files,tribunal_member=tmems)
save(members, file="tms.rdata")

tm_ocr <- members

#convert to count of entries and view
tm_count <- tbl_df(count(tm_ocr, tribunal_member))
View(tm_count)
variable.names(tm_ocr)

write.csv(tm_count, file="tm_key.csv")

#go through the key and identify correctly identified tribunal members

read.csv("tm_key.csv")-> tm_key

left_join(tm_ocr, tm_key) -> tm_ocr


#Find decisions missing tm ####
#find na
missing <- tm_ocr[is.na(tm_ocr$id),]


# number not done yet
NROW(tm_ocr[tm_ocr$tm_present == FALSE,])

table(tm_ocr$tm_present)

not_present <- tm_ocr[tm_ocr$tm_present == FALSE,]
missing_count <- tbl_df(count(not_present, id))
View(missing_count)

tm_count <- tbl_df(count(tm_ocr, id))
View(tm_count)

#copier
#copy pdfs from the pdfs folder that match the missing list to folder title "missing"
copier <- function(file){
  file.copy(from=glue("pdfs/{file}"), to="missing/", 
            overwrite = TRUE, recursive = FALSE, 
            copy.mode = TRUE)
}

mapply(copier, missing$document)

#OCR again to get past the dead one letter line ####

trib_member_skipline <- function(file) {
  # cat(glue('file {file} | '))
  print({file})
  pages <- pdf_info(glue("missing/{file}"))
  input <- image_read_pdf(glue("missing/{file}"),density=150, pages = pages$pages) %>% 
    tesseract::ocr() %>% 
    strsplit('\n') %>% 
    flatten_chr()
  
  member <- input %>% 
    str_detect("Member of .*Tribunal*")
  
  #get name of TM from line above string detect, if blank go up one extra line. 
  if(any(member)) {
    member_name <- input %>%
      str_detect( "Member of.*Tribunal*") %>%
      which() %>% #if this value is 1 then either the tm is on the previous page or it is censored
      -2 %>% max(1) %>%  {input[.]}
    
    if (member_name == "")   member_name <- input %>%
        str_detect("Member of.*Tribunal? *") %>%
        which() %>% -2 %>% max(1) %>% {input[.]}
  } else{#For NA case, do process over again in previous page
    pages <- pdf_info(glue("missing/{file}"))
    input <- image_read_pdf(glue("missing/{file}"),density=150, pages = (pages$pages-1)) %>% 
      tesseract::ocr() %>% 
      strsplit('\n') %>% 
      flatten_chr()
    
    member <- input %>% 
      str_detect("Member of.*Tribunal*")
    
    #get name of TM from line above string detect, if blank go up one extra line. 
    if(any(member)) {
      member_name <- input %>%
        str_detect("Member of.*Tribunal*") %>%
        which() %>% -2 %>% max(1) %>%  {input[.]}
      
      if (member_name == "")   member_name <- input %>%
          str_detect("Member of.*Tribunal*") %>%
          which() %>% -2 %>% max(1) %>% {input[.]}
      #For NA case, do process over again in previous page
    } else{NA -> member_name}
    
    
  }
  
  return(member_name)
}

files <- dir('missing')

#run the function ####
timestart <- Sys.time()
tmems <- map_chr(files,trib_member_skipline)
elapsed <- Sys.time() - timestart
elapsed

tmems

members <- data_frame(document=files,tribunal_member=tmems)
save(members, file="tms.rdata")

View(members)

#convert to count of entries and view
tm_count <- tbl_df(count(members, tribunal_member))
View(tm_count)
variable.names(tm_ocr)

write.csv(tm_count, file = "tm_key.csv")

#Second pass OCR function ####
#editing the function to find more missing tribunal members
#this one checks to see if the signature is below the 'member of' instead of above
trib_member_below <- function(file) {
 # cat(glue('file {file} | '))
  #get the number of pages in the pdf
  pages <- pdf_info(glue("missing/{file}"))
  
  #read the last page
  input <- image_read_pdf(glue("missing/{file}"),density=150, pages = pages$pages) %>% 
  tesseract::ocr() %>% 
  strsplit('\n') %>% 
  flatten_chr()
  
  #detect word 'member of tribunal'
  member <- input %>% 
    str_detect("Member of .*Tribunal*")
  
  #get name of TM from line above string detect, if blank go up one extra line. 
  if(any(member)) {
    member_name <- input %>%
      str_detect( "Member of.*Tribunal*") %>%
      which() %>% #if this value is 1 then either the tm is on the previous page or it is censored
      +1 %>% min(length(input)) %>%  {input[.]}
    #if blank go up one line
    if (member_name == "")   member_name <- input %>%
        str_detect("Member of.*Tribunal? *") %>%
        which() %>% +2 %>% min(length(input)) %>% {input[.]}
    #For NA case, do process over again in previous page
  } else{
    pages <- pdf_info(glue("missing/{file}"))
    input <- image_read_pdf(glue("missing/{file}"),density=150, pages = (pages$pages-1)) %>% 
      tesseract::ocr() %>% 
      strsplit('\n') %>% 
      flatten_chr()
    member <- input %>% 
      str_detect("Member of.*Tribunal*")
    
    #get name of TM from line above string detect, if blank go up one extra line. 
    if(any(member)) {
      member_name <- input %>%
        str_detect("Member of.*Tribunal*") %>%
        which() %>% +1 %>% min(length(input)) %>%  {input[.]}
      
      if (member_name == "")   member_name <- input %>%
          str_detect("Member of.*Tribunal*") %>%
          which() %>% +2 %>% min(length(input)) %>% {input[.]}
      #For NA case, do process over again in previous page
    } else{NA -> member_name}
    
    
  }
  print({file})
  return(member_name)
}

files <- dir('missing')

#1725096<96>d3ap-16.pdf is weird, so get rid of it
missing <- missing[missing$document != "1725096<96>d3ap-16.pdf",]

#run the function ####
timestart <- Sys.time()
tmems <- map_chr(files,trib_member_below)
elapsed <- Sys.time() - timestart
elapsed

members <- data_frame(document=missing$document,tribunal_member=tmems)

#convert to count of entries and view
tm_count <- tbl_df(count(members, tribunal_member))
View(tm_count)
variable.names(tm_ocr)

#get rid of 'dated' ones
members <- filter(members, tribunal_member != ".*day.*")
members


save(members, file="missing_tms2.rdata")



#Third pass OCR ####
#editing the function to catch up to the fifth page and with different "member of" lines from the missing folder
#13 feb 2019

trib_member_longer <- function(file) {
  # cat(glue('file {file} | '))
  print({file})
  pages <- pdf_info(glue("missing/{file}"))
  input <- image_read_pdf(glue("missing/{file}"),density=150, pages = max(1,pages$pages - 4) : pages$pages) %>% 
    tesseract::ocr() %>% 
    strsplit('\n') %>% 
    flatten_chr()
  
  #get rid of blank lines
  input <- input[! grepl('^\\s*$', input)]
  
  member <- input %>% 
    str_detect("Member of.*Tribunal*|.*Member of.*International Protection.*| .*Member of.*Refugee.*")
  
  #get name of TM from line above string detect, if blank go up one extra line. 
  if(any(member)) {
    member_name <- input %>%
      str_detect("Member of.*Tribunal*|.*Member of.*International Protection.*| .*Member of.*Refugee.*") %>%
      which() %>% #if this value is 1 then either the tm is on the previous page or it is censored
      -1 %>% max(1) %>%  {input[.]}
    
    if (member_name == "")   member_name <- input %>%
        str_detect("Member of.*Tribunal? *") %>%
        which() %>% -2 %>% max(1) %>% {input[.]}
  } else{#For NA case, do process over again in previous page
    pages <- pdf_info(glue("missing/{file}"))
    input <- image_read_pdf(glue("missing/{file}"),density=150, pages = max(1,pages$pages-5)) %>% 
      tesseract::ocr() %>% 
      strsplit('\n') %>% 
      flatten_chr()
    
    input <- input[! grepl('^\\s*$', input)]
    
    member <- input %>% 
      str_detect("Member of.*Tribunal*|.*Member of.*International Protection.*| .*Member of.*Refugee.*")
    
    #get name of TM from line above string detect, if blank go up one extra line. 
    if(any(member)) {
      member_name <- input %>%
        str_detect("Member of.*Tribunal*") %>%
        which() %>% -1 %>% max(1) %>%  {input[.]}
      
      if (member_name == "")   member_name <- input %>%
          str_detect("Member of.*Tribunal*") %>%
          which() %>% -2 %>% max(1) %>% {input[.]}
      #For NA case, mark NA
    } else{NA -> member_name}
    
    
  }
  
  return(member_name)
}

files <- dir("missing")
View(files)

#run the function
timestart <- Sys.time()
tmems <- map_chr(files,trib_member_longer)
elapsed <- Sys.time() - timestart
elapsed

members <- data_frame(document=files,tribunal_member=tmems)

#convert to count of entries and view
tm_count <- tbl_df(count(members, tribunal_member))
View(tm_count)
variable.names(tm_ocr)

save(members, file="na_tms.rdata")


#DEBUGGING ####

#find cases with particular text
textones <- subset(members, members$id == "Memberoft")
textones


#find na
missing <- tm_ocr[is.na(tm_ocr$id),]
missing$document

#copy pdfs from the ratpdf folder that match the missing list
copier <- function(file){
  file.copy(from=glue("pdfs/{file}"), to="missing/", 
            overwrite = TRUE, recursive = FALSE, 
            copy.mode = TRUE)
}
missing$document
mapply(copier, missing$document)


#run the ocr function on all the files with NA
timestart <- Sys.time()
tmems <- map_chr(missing$document,trib_member)
elapsed <- Sys.time() - timestart
elapsed
#skrrrahh()


#test on ocr function
file <- dir("missing")

files[40]
pages <- pdf_info(glue("missing/",files[40]))
pages$pages
input <- image_read_pdf(glue("missing/",files[40]),density=150, pages = pages$pages-1)%>% 
  tesseract::ocr() %>% 
  strsplit('\n') %>% 
  flatten_chr()
input

member <- input %>% 
    str_detect("Member of .*Tribunal*")
  member
  #get name of TM from line above string detect, if blank go up one extra line. 
  if(any(member)) {
    member_name <- input %>% print(nrow(input))%>%
      str_detect( "Member of.*Tribunal*") %>%
      which() %>% #if this value is 1 then either the tm is on the previous page or it is censored
      +1 %>% min(length(input)) %>% print()
      {input[.]}
    #if blank go up one line
    if (member_name == "")   member_name <- input %>%
        str_detect("Member of.*Tribunal? *") %>%
        which() %>% +2 %>% min(nrow(input)) %>% {input[.]}
    #For NA case, do process over again in previous page
  }
