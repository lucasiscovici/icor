#' @import data.table
#' @import tibble
#' @import Hmisc
#' @import Matrix
#' @import ppcor
#' @import tidyverse
#" @export
.onAttach <- function(...) {
 library("tidyverse")
 library("data.table")
 library("Hmisc")
  library("Matrix")
    library("MASS")
library("magrittr")
 library("rlist")   
 library("R6") 
}

icor = function(data_,verbose=F,pValueMin_=0.05,seuil_=0.3,graph=T,normul=F,signeOK=T,pcorOK=T,criticalSup=T){
    data=data_
    if(!is.matrix(data)) data=as.matrix(data)
    
    
    suppressMessages(library("data.table"))
    #source("./pcor.R")
    suppressMessages(library("tibble"))
   suppressMessages( library("Hmisc"))
    suppressMessages(library("Matrix"))
    seuil=seuil_
    seuilPcor=seuil
    p_valueMin=pValueMin_
    
    #Cor total
    dataCor_=rcorr(data)
    dataCor=dataCor_$r
    dataCorP=dataCor_$P
     if(normul){
         #p_valueMin=1
          #seuil=0
        #signeOK=F
         pcorOK=F
         #criticalSup=F
     }
    if(criticalSup){
        if (seuil_!=0.3) print("WHEN criticalSup, is set, seuil_, is not considerer")
      seuil = icor.critical.r(nrow(data)) 
      seuilPcor = icor.critical.r(nrow(data)-1) 
        cat(paste("seuil: ",seuil,"\nseuilPcor",seuilPcor,"\n"))
      }
    
    dataColsName = colnames(data)
    #On enleve les pas signif
    dataCor[upper.tri(dataCor,diag = T) | (dataCorP > p_valueMin)]=NA
   
    dataCorSum=Matrix::summary(as(dataCor,"dgCMatrix"))
    data.table::setDT(dataCorSum)
    # print(class(dataCorSum))
    #On garde les cor >= seuil
    dataCorT= dataCorSum[!is.na(x) & abs(x)>=seuil,]
    #print("ici")
    nbCol = ncol(data)
    km=1:nbCol
    nbRow=nrow(data)
    nbCorT=nrow(dataCorT)
    signifRel=data.frame()
    PASsignifRel=data.frame()
    keys=rep(NA,nbCol)
    keysName=rep(NA,nbCol)
    keysi=0
    
    #on loop les cor total respectant le seuil et p_value....
    #on regarde si les corPartielle avec chaque var est du meme signe, et sup > seuilPcor, et p_value bien
    for(i in 1:nbCorT){
        elem=dataCorT[i,]
        elemX=elem[,3]
        elemCol=as.integer(elem[,2])
        elemRow=as.integer(elem[,1])
        kmI=km[km!=elemRow & km!=elemCol]
        pb=F
        pcors=list()
        if(verbose)print(elem)
        for(j in kmI){
            if(verbose)print(j)    
            tryCatch({ pcorIJ=pcor.test(data[,elemCol],data[,elemRow],data[,j]) },
                         error = function(err) {
                                print("error")
                                print(err)
                                print(list(data[,elemCol],data[,elemRow],data[,j]))
                         }
                    )
            pcorIJVal=pcorIJ$estimate
            pcorIJP=pcorIJ$p
            pcors=c(pcors,list(pcorIJ))
            if(verbose)print(pcorIJ)
            #check p_value, signe, seuil
            if((pcorOK && (pcorIJP >= p_valueMin || (signeOK && pcorIJVal*elemX < 0) || abs(pcorIJVal) < seuilPcor))){
                if(verbose)print("fini")
                pb=T
                PASsignifRel=rbind(PASsignifRel,
                                      data.frame(i = dataColsName[elemCol],
                                        j = dataColsName[elemRow],
                                       cor=as.numeric(elemX),
                                       cor_pVal=dataCorP[dataColsName[elemCol],dataColsName[elemRow]],
                                       pcor=paste(unlist(lapply(pcors,function(x)paste(x$estimate,"_",x$p))),collapse="|")
                                                 ))
                break
            }
        }
        
        if(!pb){
            if(verbose)print(paste(i,"ok"))
            i_=keys[elemCol]
            if(is.na(i_)){
                keysi=keysi+1
                i_=keysi
                keys[elemCol]=i_
                keysName[i_]=dataColsName[elemCol]
            }
            j_=keys[elemRow]
            if(is.na(j_)){
                keysi=keysi+1
                j_=keysi
                keys[elemRow]=j_
                keysName[j_]=dataColsName[elemRow]
            }
            signifRel=rbind(signifRel,
                            data.frame(i = dataColsName[elemCol],
                                       i_=i_,
                                       j_=j_,
                                       j = dataColsName[elemRow],
                                       cor=as.numeric(elemX),
                                       cor_pVal=dataCorP[dataColsName[elemCol],dataColsName[elemRow]],
                                       pcor=pcors
                                       )
                           )
        }
    }
    if(graph){
        graphO=icor.graph(signifRel,keysName)
    }
    invisible(list(graph=graphO,pasSignif=PASsignifRel,signif=signifRel,dataCorT=dataCorT,dataCorP=dataCorP,dataCor=dataCor,dataCor_=dataCor_$r))
    
}
icor.graph = function(dataSet,names){
    library("visNetwork")
        signif=dataSet
        nodes <- data.frame(id=1:max(signif[,c("i_","j_")]),label=names[!is.na(names)])
    
        edges <- tibble(from=integer(),to=integer(),dashes=logical(),label=numeric(),width=numeric())
        for(i in 1:nrow(signif)){
            elem=signif[i,]
            #print()
             edges=add_row(edges,from=c(elem[,"i_"]),to=c(elem[,"j_"]),dashes=c(elem[,"cor"]<0),label=round(elem[,"cor"],2),width=exp(abs(elem[,"cor"])*2.5))
            
        }
        return(visNetwork(nodes,edges,width = "100%"))
}
#get minimun corr pour etre signif en fonction de n et alpha
icor.critical.r <- function( n, alpha = .05 ) {
  df <- n - 2 # car estimationde muX et muY
  critical.t <- qt( alpha/2, df, lower.tail = F ) # pour avoir le t, correpondant a (a gauche et a droite, alpha/2)
  critical.r <-  icor.studentToCorr(critical.t,df) # pour passer de t à r # critical.t / sqrt( (critical.t^2) + df )
  return( critical.r )
}

#table to html table                                     
icor.dfToHTML = function(df){
    suppressMessages(library("googleVis"))
    IRdisplay::display_html(icor.repr_html.gvis(gvisTable(df,options=list(page='enable'))))
}
icor.repr_text.gvis <- function(obj, ...) 'gvis cannot be represented in plain text (need html)'

icor.repr_html.gvis <- function(obj, ...){
    if (!requireNamespace('googleVis', quietly = TRUE))
        stop('repr_html.gvis called without loadable googleVis')

    htmlfile <- tempfile(fileext = '.html')
    on.exit(unlink(htmlfile))

    print(obj, tag='chart', file=htmlfile)
    readChar(htmlfile, file.info(htmlfile)$size)
}
icor.repr_latex.gvis <- function(obj, ...) NULL
icor.repr_markdown.gvis <- function(obj, ...) NULL
                                                  
icor.studentToCorr = function(t,dll){
    t / sqrt( (t^2) + dll )
}
icor.corrToStudent = function(r,dll){
    r * sqrt(dll/(1-(r^2)))
}
                                                  
                                                  blib=base::library
library = function(pkg,...){
    pkgg=pkg
    if(nchar(pkg) > 4) {
        if(substr(pkg,1,4)=="git:"){
            pkg=substr(pkg,5,nchar(pkg))
            devtools::install_github(pkg,...)
            tg(blib(basename(pkg),character.only = TRUE, quietly = TRUE))
            return(NULL)
        }
    }
    if(!require(pkgg,character.only = TRUE)){
        install.packages(pkgg,...)
    }
    tg(blib(pkgg,character.only = TRUE, quietly = TRUE,...))
}
load=library                                            
tg = function(f){
    suppressMessages(suppressPackageStartupMessages(suppressWarnings(f)))
}
Lib <- function(){
    x <- list()
    class(x) <- "Lib"
    return(x)
}
by = function(right,left){
    rr=right
  #if(is.list(right))rr=rr[[names(rr)[1]]]
  return(base::split(rr,left))
}
`%by%` = by 
 byGrp = function(right,left){
    rr=right
  #if(is.list(right))rr=rr[[names(rr)[1]]]
  graphCatCon(rr,left)
  return(by(rr,left))
}
`%byGraph%` = byGrp 
print.Lib <- function(e1,e2=NULL) {
  return("OK")
}

is.Lib <- function(obj) {
  return(class(obj) == "Lib")
}

plus <- function(e1, e2){
    print(e1)
    tg(library(e2))
    return(Lib())
     #NextMethod(e1,e2)
}
lib = Lib()



plotWH= function(w,h)options(repr.plot.width=w,repr.plot.height=h)
showWarning=function(f)options(warn=ifelse(f,0,-1))
lapplys = function(data,...){a=data;for(i in list(...)){ a=i(a)};return(a)}
startsWithGet=function(tab,pat)tab[startsWith(tab,pat)]
`%.%` = function(a,b){
    paste0(a,b)
}                                                 
update = function(upgrade=F,...){
    devtools::install_github("luluperet/icor",upgrade=upgrade,...)
 }
embed = function(x, height) {
    library("IRdisplay")
    tmp = tempfile(fileext = ".html")
    htmlwidgets::saveWidget(x, tmp)
    rawHTML = base64enc::dataURI(mime = "text/html;charset=utf-8", file = tmp)
    display_html(paste("<iframe src=", rawHTML, "width=100% height=", height, "id=","igraph", "scrolling=","yes","seamless=","seamless", "frameBorder=","0","></iframe>", sep = "\""))
    unlink(tmp)
}
                                                  
                                                  
EQ=function(a,b,ops="+"){
  x <- list()
  x$ops=ops
  x$a=a
  x$b=b
    class(x) <- "EQ"
    return(x)
}

sup <- function(e1, e2){
    #print(class(e1))
    #print(class(e2))
    
    return(do.call(e1$ops,list(e1$a > e2,e1$b > e2)))
     #NextMethod(e1,e2)
}
eq <- function(e1, e2){
    return(do.call(e1$ops,list(e1$a == e2,e1$b == e2)))
     #NextMethod(e1,e2)
}
supEq <- function(e1, e2){
    return(do.call(e1$ops,list(e1$a >= e2,e1$b >= e2)))
     #NextMethod(e1,e2)
}
infEq <- function(e1, e2){
    return(do.call(e1$ops,list(e1$a <= e2,e1$b <= e2)))
     #NextMethod(e1,e2)
} 
inf <- function(e1, e2){
    #print(class(e1))
    #print(class(e2))
    
    return(do.call(e1$ops,list(e1$a < e2,e1$b < e2)))
     #NextMethod(e1,e2)
}
`%&%` = function(rhs,lhs){
  #print(rhs)
   #print(lhs)
   return(EQ(rhs,lhs,"&"))
}
`%|%` = function(rhs,lhs){
  #print(rhs)
   #print(lhs)
   return(EQ(rhs,lhs,"|"))
}
`%&&%` = function(rhs,lhs){
  #print(rhs)
   #print(lhs)
   return(EQ(rhs,lhs,"&&"))
}
`%||%` = function(rhs,lhs){
  #print(rhs)
   #print(lhs)
   return(EQ(rhs,lhs,"||"))
}
                                                  
.onLoad <- function(...) {
  registerS3method("+", "Lib", plus)
  registerS3method(">", "EQ", sup)
  registerS3method(">=", "EQ", supEq)
    registerS3method("==", "EQ", eq)
    registerS3method("<", "EQ", inf)
    registerS3method("<=", "EQ", infEq)
       #registerS3method("+", "L", plusL)

}
                                                  

each = function(lst,fn){
  fns=fn
  datas=lst
  if(!is.list(datas) && !(length(datas)>1) ) datas=list(datas)
  else if(!is.list(datas) ) datas=as.list(datas)
  
      if(!is.list(fns)) fns=list(fns)
  #print(sapply(fns,function(fn)str(fn)))
  
  sapply(datas,function(data)sapply(fns,function(fn)as_mapper(fn)(data)))
}
`%each%` = each
                                    
 eachMap = function(lst,fn){
      fns=fn
  datas=lst
  if(!is.list(datas) && !(length(datas)>1) ) datas=list(datas)
  else if(!is.list(datas) ) datas=as.list(datas)
  
      if(!is.list(fns)) fns=list(fns)
  #print(sapply(fns,function(fn)str(fn)))
  
  lapply(datas,function(data)lapply(fns,function(fn)as_mapper(fn)(data)))
 }
`%map%` = eachMap
l_=list
lold=l_
detachFast = function(name){
    detach("package:"%.%name, unload=TRUE,character.only=TRUE)
 }  
#L = function(le,xc){
#  x <- list()
#  x$list = le
#  x$c=xc
#  class(x) <- "L"
#  return(x)
#}
#l=L(list(),1L)
#is.L <- function(obj) {
#  return(class(obj) == "L")
#}
#plusL = function(left,rigth){
  
#  leftt=ifelse(is.L(left),left$list,ifelse(is.list(left),left,list(left)))
  #append(leftt,rigth)
  
#  leftt[[left$c]]=rigth
#  return(L(leftt,left$c+1L))
#}       
#print.L = function(obj){
#    obj$list  
##  }
graphCatCon <- function(x, gpe) { 
    stripchart(x ~ gpe) 
    points(tapply(x, gpe, mean), 1:length(levels(gpe)), col = "red", pch = 19, cex = 1.5) 
    abline(v = mean(x), lty = 2) 
    moyennes <- tapply(x, gpe, mean) 
    traitnf <- function(n) segments(moyennes[n], n, mean(x), n, col = "blue", lwd = 2) 
    sapply(1:length(levels(gpe)), traitnf) 
}
#http://www.pacea.u-bordeaux1.fr/IMG/pdf/rapport_correl.pdf
vartot <- function(x) {
    sum((x - mean(x))^2)/length(x)
}
varinter <- function(x, gpe) {
    moyennes <- tapply(x, gpe, mean)
    effectifs <- tapply(x, gpe, length)
    (sum(effectifs * (moyennes - mean(x))^2))/length(x)
}
signif.corrCatCon = function(corr,cat,con){
    n=length(con)
    p=length(levels(cat))
    #n ind, p groupes
    K= (corr*(n-p))/((p-1)*(1-corr))
    quantilF=qf(0.05,p-1,n-p,lower.tail=FALSE)
    pval=pf(K,p-1,n-p, lower.tail = FALSE)
    list(K=K,quantileF=quantilF,reject=quantilF<K,pval=pval)
}    
corrCatCon = function(cat,con,signif=FALSE){
    res=varinter(con, cat)/vartot(con)
    if(signif)return(list(corr=res,signif=signif.corrCatCon(res,cat,con)))
    else res
}

eachRowCol = function(ll,rr,INDEX){
     apply(ll, INDEX, rr)
 }
eachCol = function(ll,rr){
     eachRowCol(ll,rr,2)
}
eachRow = function(ll,rr){
     eachRowCol(ll,rr,1)
}
`%eachCol%` = eachCol
`%eachRow%` = eachRow
 
filterCol   = function(a,func){
  a %>% Filter(func,.)
}
numericCol = function(a,b=NULL){
  a %>% filterCol(is.numeric)
}
notNumericCol = function(a,b=NULL){
  a %>% filterCol({. %>% !is.numeric(.)})
}
catCol = function(a,b=NULL){
  a %>% filterCol(is.factor)
}
notCatCol = function(a,b=NULL){
  a %>% filterCol({. %>% !is.factor(.)})
}                                   
`%numericCol%`= numericCol
`%!numericCol%`= notNumericCol
`%catCol%`= catCol  
                                    
`%!catCol%`= notCatCol
                                

`%,%` = function(ll,rr){
  listElems=if(is.list(ll) && inherits(ll,"Args")) ll
            else list(ll)
  class(listElems) = append(class(listElems),"Args")
  if(substitute(rr)==".") return(listElems)
  listElems %<>% list.append(.,rr)
    class(listElems) = append(class(listElems),"Args")
  return(listElems)
}

`%->%` = function(ll,rr){
  if(is.list(ll)) do.call(rr,ll)
  else do.call(rr,list(ll))
}

 reloadIcor = function(){
    detachFast("icor")
    blib("icor") 
  }
                                    
  updateReloadIcor=function(...){
    update(...)
      reloadIcor()
   }
ll=function(...)lold(lold(...))                               
mapFns = function(left,right){
    tg(library("tidyverse"))
     fns=right
  datas=left
  if(!is.list(datas) && !(length(datas)>1) ) datas=list(datas)
  else if(!is.list(datas) ) datas=as.list(datas)
      
    invoke_map(lapply(right,as_mapper),left)
    }
                                  
`%mapFns%`=mapFns
toDF=function(left,rr=NULL){
  if(!is.null(rr) && substitute(rr)==".")rr=NULL
  right=rr
  if(!is.null(right) && substitute(rr)==".")right=NULL
  if(is.matrix(left))return(if(!is.null(right) && (rigth=="t" || substitute(rr)=="t")) t(as.data.frame(left))else as.data.frame(left) )
  if(!is.null(right) && (rigth=="t" || substitute(rr)=="t"))
    return(toDFt(left))
  df=data.frame(matrix(unlist(left),nrow=length(left), byrow=T),stringsAsFactors=FALSE)
  row.names(df)=names(left)
  return(df)
}

`%toDF%`=toDF
toDFt=function(left,right=NULL){
  df=data.frame(matrix(unlist(left),ncol=length(left), byrow=F),stringsAsFactors=FALSE)
  colnames(df)=names(left)
  return(df)
}
`%toDFt%` = toDFt

                            
add_row_with_name = function(data,name="X",...){
  d=nrow(data)
  data2=data
  dd=list(...)
  if(is.list(dd[[1]]))dd=dd[[1]]
  data2=rbind(data2,dd)
  rownamess=rownames(data2)
  rownamess[d+1]=name
  rownames(data2)=rownamess
  return(data2)
}

int.hist = function(x,ylab="Frequency",perc=FALSE,...) {
  tb=table(factor(x,levels=min(x):max(x)))
  ptb=prop.table(tb)
  ims=function(i)if(perc){100*i}else{i}
  percc=if(perc)"%"else ""
  ll=c(min(x),max(x))
  names(tb) = lapply(1:length(ll),function(l)ll[l]%.%"("%.%ims(ptb[l])%.%percc%.%")")
  barplot(tb,space=0,ylab=ylab,...)
}
                     
Aleatoire <- R6Class("Aleatoire",
                     public = list(
                       initialize = function(a=16807,
                                             b=0,
                                             m=0x7FFFFFFF) {
                         private$m_a=a
                         private$m_b=b
                         private$m_m=m
                         private$m_nombre=as.numeric(Sys.time());
                       },
                       generer = function(min=0.0,max=private$m_m,double=FALSE) {
                         private$m_nombre = (private$m_a*private$m_nombre + private$m_b) %% private$m_m;
                         m_nombre2=if(double){private$m_nombre/(as.double(0x7FFFFFFF))*(max-min)+min} else {as.integer(private$m_nombre)%%as.integer(max-min) + min}
                         m_nombre2
                       },
                       unif = function(min=0,max=1){
                         U=self$generer(0,1,double=TRUE)
                         (max-min)*U + min
                       },
                       exp= function(lambda){
                         #https://stats.stackexchange.com/questions/234544/from-uniform-distribution-to-exponential-distribution-and-vice-versa
                         U = self$unif();
                         -(1/lambda)*log(U)
                       },
                       poisson =function(lambda=1){
                         U = self$unif();
                         -log(U)/lambda
                       },
                       gamma = function(a,b=1){
                         N=1
                         #http://www.douillet.info/~douillet/simul/Simulations.pdf
                         if(is.integer(a) && a > 1 ) {
                           k = 0 
                           A = 0
                           w = sqrt(2 * a - 1) 
                           while(k < N){
                             u = pi * (self$unif() - 1/2) 
                             y = tan(u)
                             x = w * y + a - 1
                             if (x> 0 && self$unif() < (1 + y^2)*exp((a - 1)* log(x/(a - 1)) - w * y)){
                               k = k + 1
                               A=x
                             }
                           }
                           return(A)
                         }else{
                           #https://books.google.fr/books?id=dogHCAAAQBAJ&lpg=PA252&ots=tfg2xwcUt7&hl=fr&pg=PA194#v=onepage&q&f=false
                           k = 0
                           A = 0
                           b = 1 + a / exp (-1)
                           while( k < N) {
                             p = b * self$unif() 
                             v = self$unif()
                             if (p > 1){
                               x = -log((b - p) /a)
                               if(v <= (x^(a - 1))) {
                                 k = k + 1
                                 A = x
                               }
                             }else {
                               x = p^(1/(a))
                               if (v < exp(-x)){ 
                                 k = k + 1 
                                 A= x
                               }
                             }
                           }
                           A
                         }
                       },
                       normal=function(m=0,sigma=1,nb=1){
                         if(nb==1){
                           x=self$unif();
                           return ((self$normal(nb=2)[1])*sigma+m)
                         }else if(nb==2){
                           x=self$unif();
                           x2=self$unif();
                           return(c(sqrt(-2*log(x))*cos(2*pi*x2),sqrt(-2*log(x))*sin(2*pi*x2)))
                         }
                       },
                       
                       loi_discrete=function(loi,taille=NULL){
                         if (is.null(taille)) taille=length(loi)
                         i=1;
                         x=self$unif();
                         somme=loi[i];
                         
                         while(somme < x && i < taille){
                           somme = somme+ loi[i+1];
                           i=i+1
                         }
                         return(i);
                       },
                       bernouli = function(p){
                         self$loi_discrete(c(1-p,p))-1
                       },
                       binomial=function(n,p){
                         U = matrix(lapply(1:n,.%>%{self$unif()}),nrow=1)
                         Y = (U < p);
                         X = sum(Y )
                         X
                       },
                       geom =function(p){
                         X = 1;
                         U = self$unif();
                 
                                 while(U > p){
                           X = X + 1; 
                           U = self$unif();
                         }
                         X
                       }
                       
                           ),
                           private = list(
                             m_a=integer(),
                             m_b=integer(),
                             m_m=integer(),
                             m_nombre=integer()
                           )
                             ) 



    test_same_distrib = function(sample1,sample2){
      return(ks.test(sample1,sample2)$p)
    }
    runFnXtimes = function(fn,Xtimes=100){
      p=fn
      {1:Xtimes %each% p }
    }
                     
    l = function(...){
      lazyeval::dots_capture(...)
    }
                     
    `%Xtimes%` = runFnXtimes
    eachFn = function(a,b){
      bb=b
      for(i in 1:length(bb)){
        
        as_mapper(bb[[i]])(a[[i]])
      }
    }
    `%eachFn%` = eachFn

#al = Aleatoire$new()
#al$generer()
#al$generer(max=10)
#{1:100 %each% ~al$generer(max=10,double = T) }%>% hist

#1:100 %each% ~al$loi_discrete(c(0.05,0.05,0.1,0.6,0,0.2))
#{1:500 %each% ~rexp(1) }%>% density %>% plot ; {1:500 %each% ~al$exp(1) }%>% density %>% lines(col="red")
