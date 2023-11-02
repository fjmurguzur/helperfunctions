

################ Fixing norwegian characters in a vector

fixCharacters<-function(x){iconv(x,from="UTF-8",to="ASCII//TRANSLIT")}



#B is always a factor in this case
#precision= how many decimals

descriptive.table <- function(a,b,label.a = "First variable",label.b = "Second variable",numeric.a=T,precision=1,clipboard=TRUE,ttest=T,anova=T,kruskal=T){
  results.list<-list()
  temp.df <- cbind.data.frame(a,b=as.factor(b))
  #names(temp.df) <- c(label.a,label.b)
  cat(paste("Initial number of rows: ",nrow(temp.df),sep=""))
  cat(paste("\nNAs in ",label.a,table(is.na(a))[2],sep=" "))
  cat(paste("\nNAs in ",label.b,table(is.na(b))[2],sep=" "))
  cat(paste("\nComplete cases: ",table(complete.cases(temp.df))[2]))
  
temp.df<-temp.df[complete.cases(temp.df),]
if(numeric.a==T){
  
  temp.df$a<-as.numeric(temp.df$a)
result.table <- list()
  

for(i in 1:length(levels(temp.df$b))){
  
  grouped.df<-temp.df[temp.df$b==levels(temp.df$b)[i],]
  
  result.table[[i]] <- data.frame(Variable=levels(temp.df$b)[i],Total=nrow(grouped.df),Mean=round(mean(grouped.df$a),digits=precision),SD=round(sd(grouped.df$a),digits=precision),
                                  Min=round(min(grouped.df$a),digits=precision),Max=round(max(grouped.df$a),digits=precision))
  
}

final.table<-cbind.data.frame(" "=label.b,do.call(rbind.data.frame,result.table))

cat("\n")
cat("\n")


print(final.table)

cat("\n")
cat("\n")


if(ttest==T){
  if(length(levels(temp.df$b))==2){
  cat("\n ###########################################################################
      \nT-test with formula (a~b)")
 print(t.test(a~b,data=temp.df))
  }
  else cat("\n ###########################################################################
           \nOnly 2 levels are allowed for T-test. Check the input")
} 

if(anova==T){

  cat("\n ###########################################################################
      \nANOVA with formula aov(a~b)")
  print(summary(aov(a~b,data=temp.df)))
}

}

if(kruskal==T){
  cat("\n ###########################################################################
      \nKruskal Wallis with formula kruskal.test(a~b)")
  print(kruskal.test(a~b,data=temp.df))
  
}
if(clipboard==TRUE){

  
#Copy to clipboard
cat("\n\nTable copied to clipboard")
write.table(final.table,"clipboard",sep="\t",row.names=F)
}
return(final.table)
}




########################################################################################################################


likert.recode<-function(x=NULL,from=NULL,to=NULL){
  for(i in 1:length(from)){
    
    x[x==from[i]]<-to[i]
    
  }
  return(x)
}


########################################################################################################################

likert.summarize<-function(x){
  temp.ls<-list()
  x<-as.data.frame(x) #Coerce to data.frame to avoid errors
  for(i in 1:ncol(x)){
    
    cat(paste("\nNAs in ",colnames(as.data.frame(x))[i],table(is.na(x[,i]))[2],"\n",sep=" "))
    temp.df<-data.frame(table(x[,i]),stringsAsFactors = F)
    temp.df$Percentage<-round(temp.df$Freq*100/sum(temp.df$Freq),digits = 1)
    temp.df$Var1<-as.character(temp.df$Var1)
    colnames(temp.df)<-c(paste("Likert.score",colnames(as.data.frame(x))[i], sep= "-"),paste("Count",colnames(as.data.frame(x))[i], sep= "-"),paste("Percentage",colnames(as.data.frame(x))[i], sep= "-"))
    temp.ls[[i]]<-temp.df
     
  }

  likert.data<-do.call(cbind.data.frame,temp.ls)
  write.table(likert.data,"clipboard",sep="\t",row.names=F)
  return(likert.data)
  #Copy to clipboard

}

############################################################################

likert.plotdata<-function(x,column.names=NULL,question.names=NULL,question.short=TRUE,percentage=TRUE){
  temp.ls<-list()
  x<-as.data.frame(x) #Coerce to data.frame to avoid errors
  

  
  for(i in 1:ncol(x)){
    cat(paste("\nNAs in ",colnames(as.data.frame(x))[i],table(is.na(x[,i]))[2],"\n",sep=" "))
    
    temp.df<-cbind.data.frame(Question=colnames(as.data.frame(x))[i],t(as.numeric(as.data.frame(table(x[,i]),stringsAsFactors = F)[,2])))
    
    if(is.null(column.names)){
      mylevels<- paste(rep("Likert_",times=ncol(temp.df[,-1])),c(1:ncol(temp.df[,-1])),sep="")
     # warning("Automatically generated names. Consider using the column.names argument")
    } else {
      mylevels<-column.names
    }
    
   
    colnames(temp.df)<-c("Question",mylevels)
    temp.ls[[i]]<-temp.df
    
  }
  tab<-list()
   tab$Count <- do.call(rbind.data.frame,temp.ls)
   tab$Percentage<-cbind.data.frame(Question=tab$Count$Question,tab$Count[,-1]*100/rowSums(tab$Count[,-1]))
   shortquestions<-paste(rep("Q_",times=nrow(tab$Count)),c(1:nrow(tab$Count)),sep="")
   
   tab$Count$CustomName<-shortquestions
   tab$Percentage$CustomName<-shortquestions
   
  numlevels<-length(mylevels)
 # neutralpoint<- neutralpoint
  #tab$midvalues<-tab[,neutralpoint+1]/2
  #tab.temp<-list()
  #tab.temp$Count<-cbind(tab[,1],paste(rep("Q_",times=nrow(tab)),c(1:nrow(tab)),sep="")
  #            ,tab[,2:ceiling(numlevels/2)],
  #            tab$midvalues,tab$midvalues,tab[,(neutralpoint+1):numlevels])
  
  #colnames(tab.temp$Count)<-c("Question","CustomName",mylevels[1:floor(numlevels/2)],"Likert_3a",
  #                  "Likert_3b",mylevels[(neutralpoint+1):numlevels])
  
  #tab.temp$Percentage<-cbind.data.frame(tab.temp$Count[,c(1,2)],tab.temp$Count[,-c(1,2)]*100/rowSums(tab.temp$Count[,-c(1,2)]))
  
  #point1<-2
  #point2<-((numlevels)/2)+1
  #point3<-point2+1
  #point4<-numlevels+1
  #mymin<-(ceiling(max(rowSums(tab2[,point1:point2]))*4)/4)*-100
  #mymax<-(ceiling(max(rowSums(tab2[,point3:point4]))*4)/4)*100
  
 # temp.rows<-length(tab2[,1])
  pal<-brewer.pal((numlevels),"RdBu")
 
    
    pal<-c(pal[1:(ceiling(numlevels/2)-1)], "#DFDFDF", pal[(ceiling(numlevels/2)+1):(numlevels)])
    
    legend.pal<-pal
    
    if(percentage==TRUE){tab2<-tab$Percentage} else {tab2<-tab$Count}
    
    if (question.short==TRUE){tab2 <-subset(tab2,select=-Question);tab3<-melt(tab2,id="CustomName")} else{tab2 <- subset(tab2,select=-CustomName);tab3<-melt(tab2,id="Question")}
    colnames(tab3)<-c("Question","Variable","Value")
    
    tab3$col<-rep(pal,each=length(tab2[,1]))
   
    p<-ggplot() + geom_bar(data=tab3, aes(x = Question, y=Value, fill=Variable), position="stack", stat="identity") +
      scale_fill_manual(values=c("gray90","gray70","gray50","gray30","gray10")) +
      labs(title="", y="Percentage",x="") + #Since the axes are flipped, the x label has to be written in the y label
      theme(axis.text.y = element_text(hjust=0)) +
      theme(legend.position = "bottom") +
      theme_minimal() +
    guides(fill=guide_legend(""))  # add guide properties by aesthetic
    
    
    print(p)
    write.table(tab,"clipboard",sep="\t",row.names=F)
    cat("\n\n#############################\n Table copied to clipboard \n#############################\n\n")
    return(tab)
   
}




#CHECK SPATIAL OVERLAP BETWEEN PEOPLE WHO SAY ORIGIN/GENDER AND NA


#descriptive.table(questionnaire.clean$Age.3,questionnaire.clean$Gender.3,label.a="Age",label.b = "Gender",anova=T,ttest = T)
