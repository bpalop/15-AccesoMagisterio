####################
# bpalop, June 2015
####################
setwd("~/Documents/15-Pedro")
alcala<-read.csv("CalificacionesSIalcalaCabecerasRAW.csv",dec=",",header=TRUE,sep=";",row.names=NULL)
acceso<-read.csv("datosaccesoSIalcala-limpiosCabecerasRAW.csv",dec=",",header=TRUE,sep=";",row.names=NULL)

#### We start by cleaning up the data.
# Every convocatoria+year will be translated to an int with format YYMM in order to be able to sort according to this parameter
#
# Data read from Calificaciones... will be stored as:
# alcala2(int StudentId,int Date,float Grade)
#
# Aux function setDate(year,month) gets *year*, expressed in original databases as "YYYY-YY", and 
# *month* which is the month expresed as "MMM" with the three first letters of the name of the month (in Spanish!)
#

setDate<-function(year,month){ 
  # input: year as "YYYY-YY"; month as "MMM" 
  # output: YYMM as integer
  # 2011-12, SEP -> 1209 
  # 2011-12, ENE -> 1201
  # 2011-12, JUN -> 1206
  
  if(month=="JUN"){
    conv<-06
  }
  else if(month=="ENE"){
    conv<-01
  }
  else if(month=="SEP"){
    conv<-09
  }
  else print(paste("Found data in Calificaciones with unknown value for conv: ",month))
  
  if(year=="2009-10"){
    conv<-conv+1000
  }
  else if (year=="2010-11"){
    conv<-conv+1100
  }
  else if (year=="2011-12"){
    conv<-conv+1200
  }
  else if (year=="2012-13"){
    conv<-conv+1300
  }
  else if (year=="2013-14"){
    conv<-conv+1400
  }
  else if (year=="2014-15"){
    conv<-conv+1500
  }
  else print(paste("Found data in Calificaciones with unknown value for anyo: ",year))
  
  return(conv)
}

colConv<-function(data,yearNcol,monthNcol){
  # input: dataframe and column numbers where year ("YYYY-YY") and month ("MMM") are stored.
  # output: vector with ints representing dates in YYMM format
  return(mapply(function(x,y)setDate(x,y),data[,yearNcol],data[,monthNcol]))
}

splitStudentLines<-function(n){
  nl<-{}
  alumno<-as.numeric(acceso$ALUMNO[n])
  if(acceso$Anyo6[n]!=""){
    nl<-rbind(nl,cbind(alumno,setDate(as.character(acceso$Anyo6[n]),as.character(acceso$Conv6[n])),as.character(acceso$TipoBach6[n]),
                   acceso$NotaBach6[n],acceso$Notaacceso6[n],acceso$MatCCSS6[n],acceso$MatII6[n]))
  }
  if(acceso$Anyo5[n]!=""){
    nl<-rbind(nl,cbind(alumno,setDate(as.character(acceso$Anyo5[n]),as.character(acceso$Conv5[n])),as.character(acceso$TipoBach5[n]),
                   acceso$NotaBach5[n],acceso$Notaacceso5[n],acceso$MatCCSS5[n],acceso$MatII5[n]))
  }
  if(acceso$Anyo4[n]!=""){
    nl<-rbind(nl,cbind(alumno,setDate(as.character(acceso$Anyo4[n]),as.character(acceso$Conv4[n])),as.character(acceso$TipoBach4[n]),
                   acceso$NotaBach4[n],acceso$Notaacceso4[n],acceso$MatCCSS4[n],acceso$MatII4[n]))
  }
  if(acceso$Anyo3[n]!=""){
    nl<-rbind(nl,cbind(alumno,setDate(as.character(acceso$Anyo3[n]),as.character(acceso$Conv3[n])),as.character(acceso$TipoBach3[n]),
                   acceso$NotaBach3[n],acceso$Notaacceso3[n],acceso$MatCCSS3[n],acceso$MatII3[n]))
  }  
  if(acceso$Anyo2[n]!=""){
    nl<-rbind(nl,cbind(alumno,setDate(as.character(acceso$Anyo2[n]),as.character(acceso$Conv2[n])),as.character(acceso$TipoBach2[n]),
                   acceso$NotaBach2[n],acceso$Notaacceso2[n],acceso$MatCCSS2[n],acceso$MatII2[n]))
  }
  if(acceso$Anyo1[n]!=""){
    nl<-rbind(nl,cbind(alumno,setDate(as.character(acceso$Anyo1[n]),as.character(acceso$Conv1[n])),as.character(acceso$TipoBach1[n]),
                   acceso$NotaBach1[n],acceso$Notaacceso1[n],acceso$MatCCSS1[n],acceso$MatII1[n]))
  }
  else
    nl<-{}
  als=sapply(nl[,1],FUN=as.numeric)
  cov=sapply(nl[,2],FUN=as.numeric)
  tb=sapply(nl[,3],FUN=as.character)
  nb=sapply(nl[,4],FUN=as.numeric)
  na=sapply(nl[,5],FUN=as.numeric)
  cs=sapply(nl[,6],FUN=as.numeric)
  mi=sapply(nl[,7],FUN=as.numeric)
  nl=data.frame(als,cov,tb,nb,na,cs,mi)
  return(nl)
}

setAccessTable<-function(){  
  d<-data.frame()
  for(i in seq(1,nrow(acceso))){
    d<-rbind(d,splitStudentLines(i))
  }
  colnames(d)<-c("StudentId","Date","TipoBach","GradeBach","GradeAccess","GradeSocMath","GradeMath")
  rownames(d)<-NULL
  return(d)
}

# MAIN PROGRAM
alcala2<-data.frame(StudentId=alcala$ALUMNO,Date=colConv(alcala,3,4),Grade=alcala$cali)
acceso2<-setAccessTable()

write.csv(alcala2,file="UnivGrades.csv")
write.csv(acceso2,file="AccessGrades.csv")



