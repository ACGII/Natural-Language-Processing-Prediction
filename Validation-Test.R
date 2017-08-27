for(j in 4:4){
     x<-preprocess_data(testdata[j])
     ss <- strsplit(x, "\\s+")
     n<-length(ss[[1]])
     end<-ss[[1]][[n]]
     rest<-gsub("\\s*\\w*$", "", x)
     print(rest);print(end);print(n)
     
     Corp <- corpus(rest) 
     Qdfm <- dfm(Corp, ignoredFeatures =c( stopwords("english")), stem=TRUE,ngrams=4)
     #if(n > 5){
       print(n)
          quadgram.frequency <- colSums(Qdfm)
          quadgram.frequency <- sort(quadgram.frequency, decreasing=TRUE)
          qfp.df<-data.frame(quadgram.frequency)
          qfp.df["quadgram"] <- rownames(qfp.df)
          
         print(qfp.df)
    # }
}


          f<-paste0(name,"frequency")
          Qdfm <- dfm(cor, ignoredFeatures =c( stopwords("english")), stem=TRUE,verbose=FALSE,ngrams=n)
          freq <- colSums(Qdfm)
          freq <- sort(freq, decreasing=TRUE)
          qfp.df<-data.frame(freq)
          qfp.df[name] <- rownames(qfp.df)
          qfp.df

}
       
Testdata$filelist[1]
testdata<-read_input_file(Testdata$filelist[1])
head(testdata)

  for(j in 1:6){
  
     q<-data.frame()
     t<-data.frame()
     u<-data.frame()
     bb<-data.frame()
     x<-character()
  
     x<-preprocess_data(testdata[j])
     ss <- strsplit(x, "\\s+")
     n<-length(ss[[1]])
     end<-ss[[1]][[n]]
     rest<-gsub("\\s*\\w*$", "", x)
     print(rest);print(end);print(n);print(j);cat('\n')
     Corp <- corpus(rest) 
     
    if(n>4) q<-procDfm(Corp,4,"Qgram")
     if(nrow(q)==0 && n>3){
         t<-procDfm(Corp,3,"Tgram") 
         if(nrow(t)==0 && n>2){
              bb<-procDfm(Corp,2,"Bgram")
              if(nrow(bb)==0){
                 u<-procDfm(Corp,1,"Ugram")
                # print(u)
              } else {
                #print(bb)
              }
         }else {
           #print(t)
         }
     } else {
       #print(q)
     }
      
}



 procDataG<- function(corp,n) {
    j<-0
    q<-data.frame()
    if(n>4){
             q<-procDfm(corp,4,"Qgram")
             j<-4
            
    }

     if(nrow(q)==0 && n>3){
         q<-procDfm(corp,3,"Tgram")
          j<-3 
         if(nrow(q)==0 && n>2){
              q<-procDfm(corp,2,"Bgram")
              j<-2
              if(nrow(q)==0){
                 q<-procDfm(corp,1,"Ugram")
                 j<-1
                # print(u)
              } else {
                #print(bb)
              }
         }else {
           #print(t)
         }
     } else {
       #print(q)
     }
     ret<-c(q,j)
     ret
}



switch(val[[3]],
 
{
  # case '1' here...
  print('one-1')
},
{
  # case 'two' here...
  print('two-2')    
},
{
   print('three-3')
},
{
   print('four-4')
}

)


for(i in 1:10){
 nm<-paste0("ug",i)
 nm<-read.csv(UG$filelist[i])
 sm<-paste0("u",i)
 sm<-head(nm,30)
print(nm);print(sm)

}



#U<-rbind(tg1,tg2,tg3,tg4,tg5,tg6,tg7,tg8,tg9,tg10)
#U<-rbind(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10)
U<-rbind(ug1,ug2,ug3,ug4,ug5,ug6,ug7,ug8,ug9,ug10)

#U<-rbind(bg1,bg2,bg3,bg4,bg5,bg6,bg7,bg8,bg9,bg10) 
U<-U[-c(3,4,5)]
y<-unique(U[,1])
z<-matrix(0,nrow=length(y),ncol=2)
nm<-data.frame(z)

for(i in 282132:length(y)){

      zz<-paste0('^',y[i],'$')
      x<-grep(zz,U[,1])
      sm<- sum(as.numeric(U[x,2]))
      nm[i,]<-c(as.character(y[i]),sm)
      U<-U[-c(x),]
      if(i%%100 == 0) {print(i)}
      if(i%%1000 == 0) {write.csv(nm,"Full_pgB.csv")}


}

let<-character()
for(i in 1:26){
     x<-paste0('^',letters[i],letters[i],letters[i])
     y<-paste(letters[i],letters[i],letters[i],sep='_')
     z<-paste0('^',LETTERS[i],LETTERS[i],LETTERS[i])
     w<-paste(LETTERS[i],LETTERS[i],LETTERS[i],sep='_')
    # print(c(w,x,y,z))
     let<-rbind(let,w,x,y,z)
}


for(i in 1:104){

      y<-grep(let[i,1],x[,1])
      if(length(y) !=0){
           x<-x[-c(y),]
      }
}



  