library("XML")
library("tm")
library("text2vec")
library("wordcloud")
library("ggplot2")
library("stringr")

a = list.files("C:/Users/zjkgf/Desktop/nyt_corpus/samples_500/")
dir = paste("C:/Users/zjkgf/Desktop/nyt_corpus/samples_500/",a,sep="")
n = length(dir)

alltext = c()
alltime = c()
allcategories = c()

for (i in 1:n){
  result = xmlParse(file=dir[i])
  rootnode = xmlRoot(result)
  datanode = rootnode[["body"]][["body.content"]][["block"]]
  if (!is.null(datanode)){
    fulltext = xpathApply(datanode,"//block[@class='full_text']",xmlValue)[[1]]
  } else
    fulltext = "NA"
  alltext = append(alltext,fulltext)
  headnode = rootnode[["head"]]
  year=""
  month=""
  day=""
  for (j in 1:xmlSize(headnode)){
    if (xmlName(headnode[[j]])=="meta"){
      if (xmlGetAttr(headnode[[j]],"name")=="publication_year"){
        year=xmlGetAttr(headnode[[j]],"content")
      }
      if (xmlGetAttr(headnode[[j]],"name")=="publication_month"){
        month=xmlGetAttr(headnode[[j]],"content")
      }
      if (xmlGetAttr(headnode[[j]],"name")=="publication_day_of_month"){
        day=xmlGetAttr(headnode[[j]],"content")
      }
    }
  }
  if (year!="" && month!="" && day!=""){
    time=paste(year,month,day,sep="/")
  } else if (year!="" && month!=""){
    time=paste(year,month,sep="/")
  } else if (year!=""){
    time=paste(year,sep="/")
  } else time = "NA"
  alltime = append(alltime,time)
  classify = c()
  idc = headnode[["docdata"]][["identified-content"]]
  for (j in 1:xmlSize(idc)){
    if (xmlName(idc[[j]])=="classifier"){
      class = xmlValue(idc[[j]])
      allclass = strsplit(xmlValue(idc[[j]]),'/')[[1]]
      if (length(allclass) >= 3){
      if (allclass[[1]] == "Top" && (allclass[[2]] == "News" || allclass[[2]] == "Features")){
        if (!allclass[[3]] %in% classify){
          classify = append(classify, allclass[[3]])
        }
      }
      }
    }
  }
  if (length(classify) != 0){
    temp = classify[[1]]
    if (length(classify) >= 2){
    for (j in 2:length(classify)){
      temp = paste(temp,classify[[j]],sep=",")
    }}
    allcategories = append(allcategories, temp)
  } else
  {
    allcategories = append(allcategories,"NA")
  }
}
frame = data.frame(full_text=alltext,categories=allcategories,time=alltime)
reuters = Corpus(VectorSource(alltext))
reuters = tm_map(reuters, PlainTextDocument)
reuters = tm_map(reuters, tolower)
reuters = tm_map(reuters, removeWords, stopwords("english"))
reuters = tm_map(reuters, stemDocument)
reuters = tm_map(reuters, removePunctuation)
reuters = tm_map(reuters, removeNumbers)
reuters = tm_map(reuters, stripWhitespace)
dtm = DocumentTermMatrix(reuters)
totalterm = c()
nowfreq = 1
while (TRUE){
  res = findFreqTerms(dtm,nowfreq)
  res2 = findFreqTerms(dtm,nowfreq+1)
  if (length(res) == 0 && length(res2) == 0){
    break
  }
  totalterm[[nowfreq]] = setdiff(res,res2)
  nowfreq = nowfreq + 1
}
wordnum = 0
i = length(totalterm)
top100word = c()
top100wordcount = c()
while (wordnum < 100 && i>=1){
  words = totalterm[[i]]
  for (word in words){
    wordnum = wordnum + 1
    top100word[[wordnum]] = word
    top100wordcount[[wordnum]] = i
    if (wordnum == 100){
      break
    }
  }
  i = i - 1
}
top100wordframe = data.frame(words=top100word, freq=top100wordcount)
wordcloud(top100wordframe$words,top100wordframe$freq,col = brewer.pal(8, "Set1"), min.freq = min(top100wordframe$freq),, random.color = T, max.words = max(top100wordframe$freq), random.order = T,  scale = c(4, 1))

i = length(totalterm)
totalword=c()
for (j in 1:100){
  totalword[[j]] = 0
}
while (i>=1){
  words = totalterm[[i]]
  for (word in words){
    totalword[[nchar(word)]] = totalword[[nchar(word)]]+i
  }
  i = i - 1
}
totalwordlen=c()
totalwordlennum=c()
for (i in 1:length(totalword)){
  c = totalword[[i]]
  if (c>0){
    totalwordlen = append(totalwordlen, i)
    totalwordlennum = append(totalwordlennum,c)
  }
}
totallenframe = data.frame(length=totalwordlen,count=totalwordlennum)
p = ggplot(totallenframe, aes(x=factor(length), y=count)) + geom_bar(stat="identity")+geom_text(aes(label=count), vjust=-0.2)+aes(fill=factor(length))+labs(x='单词长度', y='出现次数')

totalclass=c()
totalclassnum=c()
for (i in 1:length(alltext)){
  str = paste(frame[i,2],"",sep="")
  list = unlist(strsplit(str,","))
  for (element in list){
    have = FALSE
    for (somehow in totalclass){
      if (somehow == element){
        have=TRUE
        break
      }
    }
    if (!have){
      totalclass = append(totalclass,element)
      totalclassnum = append(totalclassnum,0)
    }
  }
}
for (i in 1:length(alltext)){
  str = paste(frame[i,2],"",sep="")
  list = unlist(strsplit(str,","))
  for (element in list){
    for (j in 1:length(totalclass)){
      if (totalclass[[j]] == element){
        totalclassnum[[j]] = totalclassnum[[j]]+1
      }
    }
  }
}
totalclassframe = data.frame(type=totalclass,count=totalclassnum)
p1 = ggplot(totalclassframe, aes(x=type, y=count)) + geom_bar(stat="identity")+geom_text(aes(label=count), vjust=-0.2)+aes(fill=type)+labs(x='类别', y='文章数目')+
  scale_x_discrete(labels = function(type) str_wrap(type, width = 10))

totalmonth=c("1","2","3","4","5","6","7","8","9","10","11","12","NA")
totalmonthnum=c(0,0,0,0,0,0,0,0,0,0,0,0,0)
for (i in 1:length(alltext)){
  str = paste(frame[i,3],"",sep="")
  list = unlist(strsplit(str,"/"))
  if (str == "NA"){
    element = "NA"
  } else{
    element=list[[2]]
  }
    for (j in 1:length(totalmonth)){
      if (totalmonth[[j]] == element){
        totalmonthnum[[j]] = totalmonthnum[[j]]+1
      }
    }
}
totalmonthframe = data.frame(month=totalmonth,count=totalmonthnum)
p2 = ggplot(totalmonthframe, aes(x=month, y=count)) + geom_bar(stat="identity")+geom_text(aes(label=count), vjust=-0.2)+aes(fill=month)+labs(x='月份', y='文章数目')+
  scale_x_discrete(labels = function(month) str_wrap(month, width = 10))

totalvec = c()
dtmmatrix = as.matrix(dtm)
vecmatrix = matrix(nrow=length(alltext),ncol=length(alltext))
for (i in 1:length(alltext)){
  for (j in 1:length(alltext)){
    x = dtmmatrix[i,]
    y = dtmmatrix[j,]
    res = sum(x*y)/sqrt((sum(x^2)*sum(y^2)))
    if (is.nan(res)){
      vecmatrix[i,j] = -1
    } else{
      vecmatrix[i,j] = res
    }
  }
  print("article")
  print(i)
}

totalclasscontent = c()
for (i in 1:length(totalclass))
{
  totalclasscontent[[i]] = list()
}
for (i in 1:length(alltext)){
  str = paste(frame[i,2],"",sep="")
  list = unlist(strsplit(str,","))
  for (element in list){
    for (j in 1:length(totalclass)){
      if (totalclass[[j]] == element){
        totalclasscontent[[j]] = append(totalclasscontent[[j]],i)
      }
    }
  }
}

totaldegree = c()
for (i in 1:length(totalclass)){
  ts = totalclasscontent[[i]]
  count = 0.0
  total = 0.0
  for (j in 1:length(ts)){
    for (k in 1:length(ts)){
      if (j != k){
        x = dtmmatrix[ts[[j]],]
        y = dtmmatrix[ts[[k]],]
        count = count + 1
        res = sum(x*y)/sqrt((sum(x^2)*sum(y^2)))
        if (is.nan(res)){
          res = -1
        }
        total = total + res
      }
    }
  }
  totaldegree = append(totaldegree,total/count)
}
for (i in 1:length(totalclass)){
  print(paste("类别",totalclass[[i]],"的相似度为："))
  print(totaldegree[[i]])
}

class1 = 21
class2 = 5
count = 0.0
total = 0.0
ts1 = totalclasscontent[[class1]]
ts2 = totalclasscontent[[class2]]
for (j in 1:length(ts1)){
  for (k in 1:length(ts2)){
      x = dtmmatrix[ts1[[j]],]
      y = dtmmatrix[ts2[[k]],]
      count = count + 1
      res = sum(x*y)/sqrt((sum(x^2)*sum(y^2)))
      if (is.nan(res)){
        res = -1
      }
      total = total + res
  }
}
print(paste("类别",totalclass[[class1]],"与类别",as.character(totalclass[[class2]]),"的相似度为："))
print(total/count)