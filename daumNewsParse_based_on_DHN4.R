# daum news crawling (2018.05.22)
# suengjae Hong
# using the packages of DNH4

install.packages("selectr")
install.packages("data.table")

library(selectr)
source("https://install-github.me/forkonlp/DNH4") # using DNH4 packages
library(DNH4)
library(curl)
library(rvest)

wd <- c("D:/pro",
        "D:/pro/dta")

# [Function 1], which is modified source code from DNH4 packages
getContent2 <- function(turl = url) {
  if (!identical(url, character(0))) {
    tem <-
      httr::GET(turl,
                httr::user_agent("DNH4 by chanyub.park <mrchypark@gmail.com>"))
    if (tem$status_code == 200) {
      if (grepl("^http://v.media.daum.net/v", tem$url)) {
        hobj <- xml2::read_html(tem)
        hobj_nodes <-
          rvest::html_nodes(hobj, "div.head_view h3.tit_view")
        title <- rvest::html_text(hobj_nodes)
        Encoding(title) <- "UTF-8"
        
        hobj_nodes <-
          rvest::html_nodes(hobj, "span.info_view span.txt_info")
        datetime <- rvest::html_text(hobj_nodes)
        Encoding(datetime) <- "UTF-8"
        
        if(length(datetime) !=1){
          datetime <- datetime[c(-1)]
          # originally, the source code was 
          # datetime <- datetime[nchar(datetime) == 20] 
          # from this code, if the first atomic vector has the same length with 20, then it returns TRUE
          # therefore, it must change to be only second atomic vector remains
          
          datetime <- gsub("[^0-9.: ]","",datetime)
          datetime <- trimws(datetime)
          datetime <-
            gsub("([0-9]{4})\\.([0-9]{2})\\.([0-9]{2})\\.",
                 "\\1-\\2-\\3",
                 datetime)
          datetime <- as.POSIXlt(datetime)
          
          if (length(datetime) == 1) {
            edittime <- datetime[1]
          }
          if (length(datetime) == 2) {
            edittime <- datetime[2]
            datetime <- datetime[1]
          }
          
          hobj_nodes <-
            rvest::html_nodes(hobj, "div.head_view em.info_cp a.link_cp img")
          press <- rvest::html_attr(hobj_nodes, "alt")
          Encoding(press) <- "UTF-8"
          
          hobj_nodes <-
            rvest::html_nodes(hobj, "div.article_view section p")
          content <- rvest::html_text(hobj_nodes)
          Encoding(content) <- "UTF-8"
          content <- trimws(content)
          content <- gsub("\r?\n|\r", " ", content)
          content <- paste0("<p>", content, "<p>")
          content <- paste0(content, collapse = " ")
          content <- gsub("<p> <p>", " ", content)
          content <- gsub("<p>", "", content)
          
          newsInfo <-
            data.frame(
              url = turl,
              datetime = datetime,
              edittime = edittime,
              press = press,
              title = title,
              content = content,
              stringsAsFactors = F
            )
        } else {

# print(rURL[m]) # this error indicates that the length of datetime is 1 & returns what link has a problem
# the above code is for checking which URL returns error
          
          datetime <- gsub("[^0-9.: ]","",datetime)
          datetime <- trimws(datetime)
          datetime <-
            gsub("([0-9]{4})\\.([0-9]{2})\\.([0-9]{2})\\.",
                 "\\1-\\2-\\3",
                 datetime) 
          datetime <- as.POSIXlt(datetime)
          
          if (length(datetime) == 1) {
            edittime <- datetime[1]
          }
          if (length(datetime) == 2) {
            edittime <- datetime[2]
            datetime <- datetime[1]
          }
          
          hobj_nodes <-
            rvest::html_nodes(hobj, "div.head_view em.info_cp a.link_cp img")
          press <- rvest::html_attr(hobj_nodes, "alt")
          Encoding(press) <- "UTF-8"
          
          hobj_nodes <-
            rvest::html_nodes(hobj, "div.article_view section p")
          content <- rvest::html_text(hobj_nodes)
          Encoding(content) <- "UTF-8"
          content <- trimws(content)
          content <- gsub("\r?\n|\r", " ", content)
          content <- paste0("<p>", content, "<p>")
          content <- paste0(content, collapse = " ")
          content <- gsub("<p> <p>", " ", content)
          content <- gsub("<p>", "", content)
          
          newsInfo <-
            data.frame(
              url = turl,
              datetime = datetime,
              edittime = edittime,
              press = press,
              title = title,
              content = content,
              stringsAsFactors = F
            )
          }
      } else {
          
          print("NA & length(date) error FYI: see line 37") # added to identify error code
          print(rURL[m])
          newsInfo <-
          data.frame(
            url = "no news links",
            datetime = "no news links",
            edittime = "no news links",
            press = "no news links",
            title = "no news links",
            content = "no news links",
            stringsAsFactors = F
          )
        return(newsInfo)
      }
      
    } else {
      
      print("NA & status code error (!=200)") # added to identify error code
      newsInfo <-
        data.frame(
          url = url,
          datetime = "page is moved.",
          edittime = "page is moved.",
          press = "page is moved.",
          title = "page is moved.",
          content = "page is moved.",
          stringsAsFactors = F
        )
      
    }
    return(newsInfo)
  } 
    else {
    print("no news links")
    
    newsInfo <-
      data.frame(
        url = "no news links",
        datetime = "no news links",
        edittime = "no news links",
        press = "no news links",
        title = "no news links",
        content = "no news links",
        stringsAsFactors = F
      )
    return(newsInfo)
  }
}


# [code 2: main] # get all categories from Daum News

cate<-getMainCategory() #get main category. In our project, we specified Politics & Economics

subcate<-lapply(cate[,2], getSubCategory) # users can change getsubCategory options (by sid)

scate<-c() # detailed category

for(i in 1:length(subcate)-1){
  if(nrow(as.data.frame(subcate[i]))==0){
  }
  else {
    scate<-rbind(scate, 
                 data.frame(sid1=cate[i,2],subcate[[i]]))
  }
}

scate <- scate[,-c(2)] # we don't need category written in Korean

#############################
###### SETTING AREA  ########
#############################

# setting for crawling date
strDate<-as.Date("2016-07-01")
endDate<-as.Date("2016-12-31")

strTime<-Sys.time()
midTime<-Sys.time()

# setting for detailed scate area

# society (1) / politics (2) / economics (3) / foreign (4) 
# culture (5) / digital (6) / editorial (7) 

# in this code, we do not consider entertainment and sports

# ONLY
# society politics economic foreign culture digital editorial

###########################


# [Code 3] - get true url list and parse news Contents

for (date in strDate:endDate){
  date<-gsub("-","",as.character(as.Date(date,origin = "1970-01-01")))
  
  trueURL <- c()
  setwd(wd[1])
  
  # the "j" indicates sector (therefore, to select specific sectors, j must be changed)
  for (j in 1:nrow(scate)){
      
      # it needs to automate the code
      if (scate[j,1] == "editorial"){
      
      # check for the crawling round
      print(paste0(j,"th round - ",date," / ",scate[j,1]," - ",scate[j,2],
                   " / start Time: ",strTime," / spent Time: ",Sys.time()-midTime,
                   " / spent Time at first: ",Sys.time()-strTime))
      
      midTime<-Sys.time()
      
      
      # set turl and find max page
      turl <- paste0("http://media.daum.net",scate[j,2],"?regDate=",date)
      
      trym<-0
      
      max<-try(getMaxPageNum(turl), silent = T) ## find maximum pages of the news in that day
      
      while(trym<=5&&class(max)=="try-error"){
        max<-try(getMaxPageNum(turl), silent = T)
        Sys.sleep(abs(rnorm(1))) ## stop running code during randomized seconds
        trym<-trym+1
        print(paste0("try again max num: ",turl))
      }
      
      # after count max pages, find the real URL
      # length(max) == 0 indicates that there is no more page (only page 1 exists)
      if(length(max)==0){
        
        tryn <- 0
        
        turl_list <- try(getUrlListByCategory(turl),silent=T)
        
        while(tryn<=5&&class(turl_list)=="try-error"){
          turl_list <- try(getUrlListByCategory(turl),silent=T)
          Sys.sleep(abs(rnorm(1))) 
          tryn<-tryn+1
          print(paste0("tray again:",turl2))
          
        }
        
        turl_list <- turl_list[,2]
        turl_list <- cbind(turl_list,scate[j,1],scate[j,2],date)
        
        trueURL <- rbind(trueURL,
                         as.data.frame(turl_list))
      }
      else{
        
        # the below loop indicates that there are more than 2 pages in the turl
        for (k in 1:max){
          
          #set the final url in the first layer
          tryn <- 0
          
          turl2 <- paste0(turl,"&page=",k)
          
          turl_list <- try(getUrlListByCategory(turl2),silent=T)
          
          while(tryn<=5&&class(turl_list)=="try-error"){
            turl_list <- try(getUrlListByCategory(turl2),silent=T)
            Sys.sleep(abs(rnorm(1))) 
            tryn<-tryn+1
            print(paste0("tray again:",turl2))
            
          }
          
          turl_list <- turl_list[,2]
          turl_list <- cbind(turl_list,scate[j,1],scate[j,2],date)
          
          trueURL <- rbind(trueURL,
                           as.data.frame(turl_list))
          
        }
      }  # this bracket indicates main part for crawling true URL


# the below code is to identify whether the real news URL link is parsed (NOT USED ANYMORE)

#      setwd(wd[2])
#      write.csv(trueURL,paste0(date,'_trueURL_',scate[j,1],'.csv'))


      #################################
      # PART 4: 
      # now crawling the main content 
      #################################
      
      rURL <- as.character(trueURL$turl_list)
      
      # from below, write the code to parse the news content and comments      
      # get content from URL
      
      content <- c()
      tryp <- 0
      
      for (m in 1:nrow(as.data.frame(rURL))){
        newsCont <- try(getContent2(rURL[m]),silent=T)

        while(tryp<=5 && class(newsCont)=="try-error"){
          Sys.sleep(abs(rnorm(1)))
          tryp <- tryp +1
          print("try again")
        }
        
        if(class(newsCont$datetime)[1]=="POSIXct"){
          content <- try(rbind(content,newsCont),silent=T)
        }
      }
      
      setwd(wd[2])
      
     write.csv(content,paste0(date,'_content_',scate[j,1],'.csv'))

    Sys.sleep(abs(rnorm(1))) 
     
    }
    else{
    } # this bracket indicates the loop in first sub category part
    
  }  # this bracket indicates that the sub-category option will affect until this code
}  # this bracket indicates that date option will affect until this code


# end of file