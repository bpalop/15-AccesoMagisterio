####################
# bpalop, June 2015
####################
setwd("~/Documents/15-Pedro")
library("data.table", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("manipulate", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("lattice", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

alcala<-read.csv("UnivGrades.csv",header=TRUE,sep=",",row.names=1)
access<-read.csv("AccessGrades.csv",header=TRUE,sep=",",row.names=1)

prepareTables<-function(){
  # Find maximum Grade obtained at University by each Student
  maxUnivGrade=data.table(alcala)[,list(max=max(Grade)),by=alcala$Student]
  colnames(maxUnivGrade)<-c("StudentId","maxUnivGrade")
  
  # Find maximum Bachillerato Grade to enter University by each Student
  maxBachGrade<-data.table(access)[,list(max=max(GradeBach,na.rm=TRUE)),by=access$Student]
  colnames(maxBachGrade)<-c("StudentId","maxBachGrade")
  #hist(maxBachGrade$maxBachGrade)
  
  # Find maximum Access Grade to enter University by each Student (average with Bachillerato and SAT)
  maxAccessGrade<-data.table(access)[,list(max=max(GradeAccess,na.rm=TRUE)),by=access$Student]
  colnames(maxAccessGrade)<-c("StudentId","maxAccessGrade")
  #hist(maxAccessGrade$maxAccessGrade)
  
  # Find chronologically first grade obtained at university
  sortByDateUnivGrade=alcala[order(alcala$StudentId,alcala$Date),]
  firstGrade<-by(sortByDateUnivGrade,sortByDateUnivGrade$StudentId,head,n=1)
  rm(sortByDateUnivGrade)
  firstGrade<-do.call("rbind", as.list(firstGrade))
  firstGrade<-firstGrade[,c(1,3)]
  colnames(firstGrade)<-c("StudentId","firstUnivGrade")
  
  # Find type of Bachillerato (if more than one, choose first)
  sortByDateBachType=access[order(access$StudentId,access$Date),]
  firstType<-by(sortByDateBachType,sortByDateBachType$StudentId,head,n=1)
  firstType<-do.call("rbind", as.list(firstType))
  # bpalop--Right now, firstType has lots of interesting information. Come back here later!
  #firstType<-firstType[,c(1,3)]
  
  # Create table with all derivated data
  allStudents<-maxUnivGrade
  rm(maxUnivGrade)
  allStudents<-merge(allStudents,maxBachGrade,by="StudentId")
  rm(maxBachGrade)
  allStudents<-merge(allStudents,maxAccessGrade,by="StudentId")
  rm(maxAccessGrade)
  allStudents<-merge(allStudents,firstGrade,by="StudentId")
  rm(firstGrade)
  allStudents<-merge(allStudents,firstType,by="StudentId")
  rm(firstType)
  
  # Type of previous studies: GRAE, GRAP, GRCT, GRHC, S01 for others
  allStudents$TipoBach<-revalue(allStudents$TipoBach,c("F02"="S01"))
  allStudents$TipoBach<-revalue(allStudents$TipoBach,c("U01"="S01"))
  allStudents$TipoBach<-revalue(allStudents$TipoBach,c("A01"="S01"))
  allStudents$TipoBach<-revalue(allStudents$TipoBach,c("U04"="S01"))
  allStudents$TipoBach<-revalue(allStudents$TipoBach,c("GRAE"="S01"))
  allStudents$TipoBach<-revalue(allStudents$TipoBach,c("GRAP"="S01"))
  allStudents$TipoBach<-revalue(allStudents$TipoBach,c("K02"="S01"))
  
  
  return(allStudents)

}  

PDF_MaxAcess_VS_BestAtUni<-function(){
  # maximum grade they ever obtained at university vs.
  #                     maximum grade they ever obtained at access point
  pdf("MaxAcess_VS_BestAtUni.pdf")
  xyplot(allStudentsCleanAccess$maxUnivGrade~allStudentsCleanAccess$maxAccessGrade,type = c("p","r"),
       xlab="Nota Maxima de Acceso",ylab="Mejor nota conseguida en la Universidad")
  dev.off()
}

PDF_MaxAccess_VS_FirstAtUni<-function(){
  # first grade they ever obtained at university vs.
  #                     maximum grade they ever obtained at access point
  pdf("MaxAccess_VS_FirstAtUni.pdf")
  xyplot(allStudentsCleanAccess$firstUnivGrade~allStudentsCleanAccess$maxAccessGrade,type = c("p","r"),
       xlab="Nota Maxima de Acceso",ylab="Primera convocatoria en la Universidad")
  dev.off()
}

PDF_MaxBach_VS_FirstAtUni<-function(){
  # maximum grade they ever obtained at university vs.
  #                     maximum grade they ever obtained at Highschool
  pdf("MaxBach_VS_FirstAtUni.pdf")
  xyplot(allStudentsCleanBach$firstUnivGrade~allStudentsCleanBach$maxBachGrade,type = c("p","r"),
       xlab="Nota Maxima de Bachillerato",ylab="Primera convocatoria en la Universidad")
  dev.off()
}

PDF_BachType_VS_FirstAtUni<-function(){
  # In this plot we confirm that, independently of accessGrade,
  # CT Highschoolers perform better than HC in their first attempt
  d<-ggplot(data=as.data.frame(allStudentsCleanAccess),
            aes(x=maxAccessGrade,y=firstUnivGrade,
                shape=factor(TipoBach),color=factor(TipoBach)))
  d<-d+geom_point(size=3)
  ggsave("BachType.pdf")
}

PDF_MathAccess_VS_FirstAtUni<-function(){
  # We classify students according to Access: Scientific Highschool, Social with Math, Social without Math
  socialNoMathBefore<-which(allStudentsCleanAccess$TipoBach=="GRHC" & is.na(allStudentsCleanAccess$GradeSocMath))
  socialMathBefore<-which(allStudentsCleanAccess$TipoBach=="GRHC" & !is.na(allStudentsCleanAccess$GradeSocMath))
  scientNoMathBefore<-which(allStudentsCleanAccess$TipoBach=="GRCT" & is.na(allStudentsCleanAccess$GradeMath))
  scientMathBefore<-which(allStudentsCleanAccess$TipoBach=="GRCT" & !is.na(allStudentsCleanAccess$GradeMath))
  d<-data.frame(TipoBach="GRHC-NM",
                tmp=allStudentsCleanAccess$TipoBach[socialNoMathBefore], #bpr delete later
                firstUnivGrade=allStudentsCleanAccess$firstUnivGrade[socialNoMathBefore],
                maxAccessGrade=allStudentsCleanAccess$maxAccessGrade[socialNoMathBefore])

  d<-rbind(d,data.frame(TipoBach="GRHC-M",
                        tmp=allStudentsCleanAccess$TipoBach[socialMathBefore], #bpr delete later
                        firstUnivGrade=allStudentsCleanAccess$firstUnivGrade[socialMathBefore],
                        maxAccessGrade=allStudentsCleanAccess$maxAccessGrade[socialMathBefore]
                        ))
  d<-rbind(d,data.frame(TipoBach="GRCT-NM",
                        tmp=allStudentsCleanAccess$TipoBach[scientNoMathBefore], #bpr delete later
                        firstUnivGrade=allStudentsCleanAccess$firstUnivGrade[scientNoMathBefore],
                        maxAccessGrade=allStudentsCleanAccess$maxAccessGrade[scientNoMathBefore]
  ))
  d<-rbind(d,data.frame(TipoBach="GRCT-M",
                        tmp=allStudentsCleanAccess$TipoBach[scientMathBefore], #bpr delete later
                        firstUnivGrade=allStudentsCleanAccess$firstUnivGrade[scientMathBefore],
                        maxAccessGrade=allStudentsCleanAccess$maxAccessGrade[scientMathBefore]
  ))
  
  boxplot(d$firstUnivGrade~d$TipoBach)
    
  #graph<-ggplot(data=d,
  #              aes(x=maxAccessGrade,y=firstUnivGrade,
  #                  #shape=factor(TipoBach),
  #                  color=factor(TipoBach)))
  #graph<-graph+geom_point(size=3)
  #return(graph)
}

# MAIN
allStudents<-prepareTables()
# bpalop -> pedro Que hacemos con los que no tienen nota de acceso
allStudentsCleanAccess<-allStudents[is.finite(allStudents$maxAccessGrade),]
allStudentsCleanBach<-allStudents[is.finite(allStudents$maxBachGrade),]

#boxplot(allStudentsCleanAccess$firstUnivGrade~allStudentsCleanAccess$TipoBach)
ddply(allStudentsCleanAccess,~TipoBach,summarise,
      mean=mean(firstUnivGrade),
      q1=quantile(firstUnivGrade,0.25),
      q2=quantile(firstUnivGrade,0.5),
      q3=quantile(firstUnivGrade,0.75))


