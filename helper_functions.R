#Co-occurence analysis

coo_df<-
data.frame(var1=gl(3,3,labels = c(paste0("grp",1:3))),var2=c("A","B","C","A","D","B","B","C","U"))

coo<-function(df,x,y){v<-crossprod(table(df[,c(x,y)]));diag(v)<-0;v}


#find duplicates

dup<-function(x){duplicated(x)|duplicated(x,fromLast = T)}