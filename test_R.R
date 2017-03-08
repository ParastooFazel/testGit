ret <- list()
ret2 <- c()
i <- 1
for(var in variables){
  eval(parse(text=paste("b<-  lme(",var," ~ Day*group,random=~1|Subject/Day,data=data,correlation=corCAR1(form=~1|Subject/Day))")))
  
  # print(Anova(b,type="3"))
  
  #}
 
 temp <- Anova(b,type="III")
  ret[[i]] <- temp
  ret2 <- rbind(ret2,c(attr(temp,"heading")[2],temp[,3]))
  colnames(ret2) <- c("name",rownames(temp))
  i <- i +1
}








ret <- list()
i <- 1
for(var in variables){
  eval(parse(text=paste("b<- cor.test(",var," ,FAT.pre,data=data)"))) 
  temp <- b
  if(length(which(temp$Pr < 0.05)) >= 1)
  {
    ret[[i]] <- temp
    i <- i +1
  }
}

