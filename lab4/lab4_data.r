

library(XML)
library(xml2)
library(tools)
library(dplyr)

# init
host_name <- 'http://www.gks.ru'

# all stat years and path to it db
years_v <-    c(2003, 2004, 2005, 2006,
                2007, 2008, 2009, 2010,
                2011, 2012, 2013, 2014,
                2015)
db_names <- c('B03_14', 'B04_14', 'B05_14p', 'B06_14p',
              'B07_14p', 'B08_14p', 'B09_14p', 'B10_14p',
              'B11_14p', 'B12_14p', 'B13_14p', 'B14_14p',
              'B15_14p')
years <- data.frame(years_v, db_names)

#load doc with stat data and convert containing table into dataframe
loadGKSData <- function(ref){
  ext <- file_ext(ref)
  if(ext == "doc"){
    print("Неподдерживаемый тип источника")
    #getTableFromDoc(ref)
  }
  if(ext == "docx"){
    getTableFromDoc(ref)
  }else if(ext == "htm"){
    getTableFromHtm(ref)
  }else
    print("Неподдерживаемый тип источника")
  
}

toLocalEncoding <- function(x, sep=",", quote=TRUE, encoding="utf-8"){
  rawcsv <- tempfile()
  write.csv(x, file = rawcsv)
  result <- read.csv(rawcsv, encoding = "UTF-8")
  unlink(rawcsv)
  result
}

#dialog with user and return reference to needed stat doc
getGKSDataRef <- function(){
  path <- '/bgd/regl/'
  params <- '/?List&Id='
  id <- -1
  year <- readline(prompt = paste("Введите год от", years$years_v[1],
                                  "до", tail(years$years_v, n = 1), " "))
  db_name <- years$db_names[years_v == year]
  # go through tree until we got a link to doc instead of id in stat db
  while(TRUE){
    url <- paste(host_name, path, db_name, params, id, sep = '')
    xml <- xmlTreeParse(url, useInternalNodes = T)
    names <- xpathSApply(xml, "//name", xmlValue)
    Encoding(names) <- "UTF-8" 
    refs <- xpathSApply(xml, "//ref", xmlValue)
    for(i in 1:length(names))
      print(paste(i, names[i]))
    num <- readline(prompt = "Введите номер ")
    ref <- refs[as.numeric(num)]
    if(substr(ref, 1, 1) != "?")
      return(ref)
    id <- substr(ref, 2, nchar(ref))
  }
}

getTableFromHtm <- function(ref) {
  url <- paste(host_name, ref, sep = "")
  doc <- htmlParse(url, encoding = "Windows-1251")
  if(length(xpathSApply(doc,"//table", xmlValue)) == 0){
    print("Нет таблицы в источнике")
    return()
  }
  
  assign("dataGKS", readHTMLTable(doc, trim = TRUE, which = 1,
                                  stringsAsFactors = FALSE,
                                  as.data.frame = TRUE), .GlobalEnv)
  names(dataGKS) <<- gsub("[\r\n]", "", names(dataGKS))
  dataGKS[, 1] <<- gsub("[\r\n]", "", dataGKS[, 1])
  dataGKS <<- toLocalEncoding(dataGKS)
}

##get tables from doc
getTableFromDoc <- function(word_doc) {
  
  tmpd <- tempdir()
  tmpf <- tempfile(tmpdir=tmpd, fileext=".zip")
  
  file.copy(word_doc, tmpf)
  unzip(tmpf, exdir=sprintf("%s/docdata", tmpd))
  
  doc <- read_xml(sprintf("%s/docdata/word/document.xml", tmpd))
  
  unlink(tmpf)
  unlink(sprintf("%s/docdata", tmpd), recursive=TRUE)
  
  ns <- xml_ns(doc)
  
  tbls <- xml_find_all(doc, ".//w:tbl", ns=ns)
  
  lapply(tbls, function(tbl) {
    
    cells <- xml_find_all(tbl, "./w:tr/w:tc", ns=ns)
    rows <- xml_find_all(tbl, "./w:tr", ns=ns)
    dat <- data.frame(matrix(xml_text(cells), 
                             ncol=(length(cells)/length(rows)), 
                             byrow=TRUE), 
                      stringsAsFactors=FALSE)
    colnames(dat) <- dat[1,]
    dat <- dat[-1,]
    rownames(dat) <- NULL
    assign("dataGKS", dat, .GlobalEnv)
  })
}


# получим ссылку на данные, которые нас интересуют 2010, 15, 43 Место занимаемое субъектом России по производству пива для ggplot2 графика 
url <- getGKSDataRef()
# выгрузим данные по ссылке
DF <- loadGKSData(url)
# проверим целостность типы переменных и целостность данных
glimpse(DF)
#переименуем столбцы
names(DF) <- c("Idx", "RegName", "BeerPlace")
#извлечем данные Центрального Федерального округа они находятся со 2 индекса по 19
df.central.2010 <- DF[2:19, ]
# изменим тип переменной BeerPlace на numeric и избавимся от поля Idx
df.central.2010 <- df.central.2010 %>% 
  mutate_at(vars(BeerPlace), funs(as.numeric)) %>% 
  select(RegName, BeerPlace)
#после всех преобразований
glimpse(df.central.2010)
df.central.2010



# получим ссылку на данные, которые нас интересуют 2011, 15, 49 Место занимаемое субъектом России по производству пива для ggplot2 графика 
url <- getGKSDataRef()
# выгрузим данные по ссылке
DF <- loadGKSData(url)
# проверим целостность типы переменных и целостность данных
glimpse(DF)
#переименуем столбцы
names(DF) <- c("Idx", "RegName", "BeerPlace")
#извлечем данные Центрального Федерального округа они находятся со 2 индекса по 19
df.central.2011 <- DF[2:19, ]
# изменим тип переменной BeerPlace на numeric и избавимся от поля Idx
df.central.2011 <- df.central.2011 %>% 
  mutate_at(vars(BeerPlace), funs(as.numeric)) %>% 
  select(RegName, BeerPlace)
#после всех преобразований
glimpse(df.central.2011)
df.central.2011


#сохраним полученные данные в формате csv
write.csv2(x = df.central.2010, file = "./data/df.central.beer.2010.csv")
write.csv2(x = df.central.2011, file = "./data/df.central.beer.2011.csv")


