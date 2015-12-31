#数据读取
get.msg<-function(path){
  con<-file(path,open = "rt")
  msg<-readLines(con)
  n<-grep("各位代表",msg)
  msg<-msg[seq(n[1],length(msg),1)]
  close(con)
  return(paste(msg,collapse = ""))
}
path="E:/资料/docs"
docs<-dir(path)
docs<-docs[which(docs!="cmds")]
msg<-sapply(docs,function(p) get.msg(paste(path,p,sep="/")))
msg<-as.data.frame(cbind(names(msg),unlist(msg)),stringAsFactor=F)
colnames(msg)<-c("id","sentence")
#rownames(msg)<-null
#数据清洗
msg$sentence<-gsub(" ","",msg$sentence)
msg$sentence<-gsub("（鼓掌）","",msg$sentence)
sentence<-as.vector(msg$sentence)
id<-as.vector(msg$id)
sentence<-gsub("[[:digit:]]*","",sentence)
sentence<-gsub("[a-zA-Z]","",sentence)
sentence<-sentence[!is.na(sentence)]
id<-id[!is.na(sentence)]
msg<-as.data.frame(cbind(id,sentence),stringsAsFactors = F)
stopwords<-read.table("E:/资料/docs/stopword_CN.csv",header=T,sep=",")
library(rJava)
library(Rwordseg)
segment.options(isNameRecognition=T)
system.time(x<-lapply(sentence,function(x) segmentCN(x,nature = T)))
#segment.options(isNameRecognition = TRUE)，getOption("isNameRecognition")
msgWords<-as.data.frame(cbind(rep(msg[,1],unlist(lapply(x,length))),
                        unlist(x),names(unlist(x))),
                        stringAsFactors=F)
msgWords<-msgWords[!(msgWords$V2%in%stopwords$term),]
#msgWords <- msgWords[which(nchar(msgWords$V2) > 1),]
msgWords$Term <- gsub("[\r\n]", "", msgWords$V2)


library(RColorBrewer)
library(wordcloud)
result<-as.dataframe(table(msgWords$Term))
wordcloud(result$Var1,result$Freq,random.order=FALSE,rot.per=.45,col=brewer.pal(9,"Set1"))





Logic <- rep(1, length(msgWords[,1]))
msgWords <- as.data.frame(cbind(msgWords, Logic), stringsAsFactors = F)
msgTerm <- dcast(msgWords, Term ~ Docid, value.var = "Logic", fun.aggregate = length)#制作词频表
msgTerm <- melt(msgTerm, id = 1)
msgTerm <- msgTerm[which(msgTerm$value > 0),]
names(msgTerm) <- c("Term", "Docid", "Freq")
msgTerm <- join(msgTerm, msgWords[,1:3])
msgTerm <- unique(msgTerm)
