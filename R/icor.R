#' @import data.table
#' @import tibble
#' @import Hmisc
#' @import Matrix
#' @import ppcor
#' @import tidyverse
#" @export
.onAttach <- function(...) {
 suppressPackageStartupMessages({library("tidyverse")
  library("wrapr")
 library("purrr")
 library("data.table")
 library("Hmisc")
  library("Matrix")
    library("MASS")
library("magrittr")
 library("dplyr")
 library("rlist")   
 library("withr") 
 library("R6") 
 library("future")
                                 
                                 
                                 })
}

#a finir, avec individus en col, et interaction entre col
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
dfToHTML = function(df){
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
     if(is.list(pkgg) || length(pkg) > 1){
          lapply(pkgg,library)
      }else{
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


plotWHDefault=function(w,h){list(repr.plot.width=w,repr.plot.height=h)}
plotWH.= function(w=NULL,h=NULL){
 ww=if(is.null(w))defaultW() else w
 hh=if(is.null(h))defaultH() else h
 options(repr.plot.width=ww,repr.plot.height=hh)
 }
plotWH = function(w=NULL,h=NULL){
 saved=plotWH.(w,h)
 return(invisible(function(){
   options(saved)
 }))
 }
defaultW=.%>%{getOption("repr.plot.width")}
defaultH=.%>%{getOption("repr.plot.height")}
                                                  
with_plotWH = function(w,h,...)with_options(plotWHDefault(w,h),...)
                                                  
                                                  
show_hideWarning.=function(f)options(warn=ifelse(f,0,-1))
showWarning=function()show_hideWarning.(TRUE)
hideWarning=function()show_hideWarning.(FALSE)
toggleWarning=function()show_hideWarning.(ifelse(getOption("warn")==0,FALSE,TRUE))
lapplys = function(data,...){a=data;for(i in list(...)){ a=i(a)};return(a)}
startsWithFromList=function(tab,pat)tab[startsWith(tab,pat)]
`%.%` = function(a,b){
    paste0(a,b)
}                                                 
update = function(upgrade=F,...){
    devtools::install_github("luluperet/icor",upgrade=upgrade,...)
 }
embed = function (x, height="100%",width="100%") 
{
    tg(library("IRdisplay"))
    tmp = tempfile(fileext = ".html")
    htmlwidgets::saveWidget(x, tmp)
    rawHTML = base64enc::dataURI(mime = "text/html;charset=utf-8", 
        file = tmp)
    display_html(paste("<iframe src=", rawHTML, "width=",width,"height=", 
        height, "id=", "igraph", "scrolling=", "yes", "seamless=", 
        "seamless", "frameBorder=", "0", "></iframe>", sep = "\""))
    unlink(tmp)
}
embedDT=function(dt,height="100%",width="100%",...){
 tg(lib + "DT")
    embed(DT::datatable(dt,...),height,width)
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
   
       registerS3method("-", "StrCls", minusStrCls)

}
                                                  
   
l.=list
l.. <- function(...) {
    .env=parent.frame()
  eval(do.call(bquote, list(substitute(list(...)),
                                                   where = .env),
                                     envir = .env),.env,.env)
}
l=l..
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


filterCol   = function(a,func){
  a %>% Filter(func,.)
}
numericCol = function(a,b=NULL){
  a %>% filterCol(is.numeric)
}
notNumericCol = function(a,b=NULL){
  a %>% filterCol(. %>% {!is.numeric(.)})
}
catCol = function(a,b=NULL){
  a %>% filterCol(is.factor)
}
notCatCol = function(a,b=NULL){
  a %>% filterCol(. %>% {!is.factor(.)})
}                                   
`%numericCol%`= numericCol
`%!numericCol%`= notNumericCol
`%catCol%`= catCol  
                                    
`%!catCol%`= notCatCol
                                

`%,.%` = function(ll,rr){
  listElems=if(is.list(ll) && inherits(ll,"Args")) ll
            else list(ll)
  class(listElems) = append(class(listElems),"Args")
  if(substitute(rr)==".") return(listElems)
  listElems %<>% list.append(.,rr)
    class(listElems) = append(class(listElems),"Args")
  return(listElems)
}

`%->.%` = function(ll,rr){
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
ll=function(...)l.(l.(...))                               
mapFns = function(left,right){
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
                         if(b==1) return(private$gamma_Without_b(a))
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
                             m_nombre=integer(),
                             gamma_Without_b = function(a) { 
                               #loi expo -> F(x) =1-exp(x)
                               #r=1-exp(-x)
                               #-ln(r)
                              
                               return(-log(prod(sapply(1:a,self$unif))))
                             }
                           )
                             ) 


      dfRowToList = . %>% t %>% as.data.frame %>% as.list
      densityPlt= . %>% density %>% plot
      densityLines= function(a,...) lines(density(a),...)
      
    listToDotsFn =function(liste,fn){
      do.call(fn, liste)
    }
    listToDotsFn_ =function(liste,fn){
      names(liste)=NULL
      do.call(fn, liste)
    }
    `%listToDotsFn_%` = listToDotsFn_
    `%listToDotsFn%` = listToDotsFn
    test_same_distrib = function(sample1,sample2){
      return(ks.test(sample1,sample2)$p)
    }
    capturePrint = function(w){
      capture.output(print(w))
    }
    runFnXtimes = function(fn,Xtimes=100){
      aa=match.call()
      aa2=aa[[2]]
      #Xti=Xtimes
      #print(capturePrint(aa2))
      if(str_detect(toString(aa2),"^[0-9]+$")){
        aa2=aa[[3]]
        Xti=fn
        #print("d")
      }else{
        Xti=Xtimes
      }
      parent <- parent.frame()
      env    <- new.env(parent = parent)
      dfn=aa2
      p=splitArgsl_(dfn,env,parent)
      if(is.null(p)) p = fn
      .p.=p
        l(1:Xti) %each% .p.
    }
    `%Xtimes%` = runFnXtimes
    eachFn = function(a,b){
      aa=match.call()
      parent <- parent.frame()
      env    <- new.env(parent = parent)
      bb=splitArgsl_(aa[[3]],env,parent)
      listNames=names(a)
      for(i in 1:length(a)){
        k=listNames[i]
        if(is.null(listNames[i]) || listNames[i]==""){
          k=i
        }
        do.call(bb[[i]],list(a[[k]]))
      }
    }
    `%eachFn%` = eachFn
    is.doubledot=function(str){
      if(length(str)==3 && str[[1]] %in% c("$","@"))str=str[[3]]
      str_detect(str,"^\\..+\\.$")
    }
    
    
    eachRowCol = function(ll,rr,INDEX){
      n=if(INDEX==2)colnames(ll)else rownames(ll)
      lapply(1:length(n),function(i){
                                    rr(if(INDEX==2)ll[,i]else ll[i,],.y=n[i])
                                    }
            )
    }
    eachCol = function(ll,rr){
      eachRowCol(ll,rr,2)
    }
    eachRow = function(ll,rr){
      eachRowCol(ll,rr,1)
    }
    `%eachCol%` = eachCol
    `%eachRow%` = eachRow
                     
    eachRowCol. = function(ll,rr,INDEX){
       apply(ll,INDEX,rr)
    }
    `%eachRowCol.%` = eachRowCol.
    
    test_normal = function(sample1){
      return(ks.test(sample1,"pnorm",mean=0,sd=1)$p)
    }
    
  
    map = function(lst,fn,env=NULL){
      d=match.call()
      if (is.null(env)){ 
        parent <- parent.frame()
        env    <- new.env(parent = parent)
      }
      dfn=d[[3L]]
     
     if (is_funexpr(dfn)){
       #pls=as.list(d)
        p=splitArgs(list(dfn),env,parent,withDotP=T)
       #p=l_(dfn,env=env,parent=parent)
       }else{
       p=splitArgsl_(dfn,env,parent)
      }
      
      if(is.null(p)) p = eval(fn,env,env)
      fns=p
      datas=lst
      if(!is.list(datas) && !(length(datas)>1) ) datas=list(datas)
      else if(!is.list(datas) ) datas=as.list(datas)
      
      if(!is.list(fns)) fns=list(fns)
      lapply(datas,function(data)lapply(fns,function(fn)fn(data)))
    }
    
    `%map%` = map
    l___=function(...){
  a=match.call()
  lapply(a[-1],function(a){eval(call("curry",a))})
}
    splitArgsl_=function(dfn,env,parent){
      ae=callToString(dfn)
      if(is.doubledot(ae))return(NULL)
      if(!stringr::str_detect(ae,"^l__?_?.*$")){
        p=splitArgs(list(dfn),env,parent)
      }else{
        p=eval(dfn)
      }
      return(p)
    }
    each = function(lst,fn,env=NULL){
      d=match.call()
      if (is.null(env)){ 
        parent <- parent.frame()
        env    <- new.env(parent = parent)
      }
      dfn=d[[3L]]
      if (is_funexpr(dfn)){
       pls=as.list(d)
       p=splitArgs(list(pls[[3L]]),env,parent,withDotP=F)
       #p=l_(dfn,env=env,parent=parent)
       }else{
        p=splitArgsl_(dfn,env,parent)
       }
      
      
      if(is.null(p)) p = eval(fn,env,env)
      fns=p
      datas=lst
      if(!is.list(datas) && !(length(datas)>1) ) datas=list(datas)
      else if(!is.list(datas) ) datas=as.list(datas)
      
      if(!is.list(fns)) fns=list(fns)
      sapply(datas,function(data)sapply(fns,function(fn)fn(data)))
    } 
    
    `%each%` = each
    
    callToString  = function(call){
      as.character(lazyeval::as_name(call))
    }
    splitArgs = function(calls,env,parent,withDotP=F){
      rhss   <- list()
      i <- 1L 
      stop=F
      namesL=names(calls)
      if(is.null(namesL))namesL=1:length(calls)
      for(ii in 1:length(calls)) {
        rhs=calls[[ii]]
        formula=F
        if (is_parenthesized(rhs))
          rhs <- eval(rhs, env, env)
        rhs <- 
          if (is_funexpr(rhs) || withDotP)
            rhs
        else if(rlang::is_formula(rhs)){
          formula=T
          as.formula(rhs)
        }
        else if (is_function(rhs) || is_colexpr(rhs))
          prepare_function(rhs)
        else if (is_first(rhs)) 
          prepare_first(rhs)
        else {
          stop=F
          rhs
        }
        if(stop)break
        
        if(formula==F){
          rhs=lazyeval::f_capture(rhs)
        }
        rhss[[namesL[i]]]=purrr::as_mapper(rhs)
        i = i + 1L
      }
      if(stop)return(NULL)
      return(rhss)
    }
    l_ = function(...,.env = parent.frame()){
      calls  <- match.call()
      parent <- parent.frame()
      
      env    <- new.env(parent = parent)
      
      if(length(calls)<2){
        return(list())
      }
      .call2 <- do.call(bquote, list(substitute(list(...)),
                                                   where = .env),
                                     envir = .env)
      pls=as.list(.call2)
      splitArgs(pls[-1],env,parent)
    }
`%call%` = function(fn,args){
  return(do.call(fn,args))
}
removeParamsInCall=function(params,calls){
  a=calls
  al=as.list(a)
  #print(al)
  anames=names(al)
  #print(anames)
  if (!is.null(anames)){
  for(i in 1:length(anames)){
    if(anames[i] %in% params){
      al=al[-i]
    }
  }
  }
  al
}

l_.=function(...,x=NULL,n=NULL,i=1){
  a=match.call()
  fn=a[[1]]
  fns=lazyeval::as_name(fn)
  amoins1=a[-1]
  amoins1=removeParamsInCall(c("x","n"),amoins1)
  #print(fns)
  what=NULL
  if(stringr::str_detect(fns,"^l1.*$"))
    what=1
  else if(stringr::str_detect(fns,"^lx.*$"))
    what=x
  else if(stringr::str_detect(fns,"^ln.*$"))
    what=i:n
  #print(what)
  if(!is.null(what)){
    fn=stringr::str_replace(fns,"[1xn]","")
    #fn=lazyeval::as_call(fn)
  }
  #print(what)
  cc=do.call(fn,as.list(amoins1))
             if(!is.null(what)){
                if(length(what)>1)
                  return(cc%getElems%what)
               else
                 return(cc%getElem%what)
             }
             return(cc)
} 
l1_ = l_.
l1__ = l_.
l1___ = l_.

lx_ =  l_.
lx__ =  l_.
lx___ =  l_.
ln_ =  l_.
ln__ =  l_.
ln___ =  l_.
   
    l__ = function(...){
      calls  <- match.call()
      parent <- parent.frame()
      
      env    <- new.env(parent = parent)
      
      if(length(calls)<2){
        return(list())
      }
      pls=as.list(calls)
      splitArgs(pls[-1],env,parent,T)
    }
    
    prepare_function <- function(f)
    {
      as.call(list(f, quote(.)))
    }
    is_placeholder <- function(symbol)
    {
      identical(symbol, quote(.))
    }
    
    is_function <- function(expr)
    {
      is.symbol(expr) || is.function(expr)
    }
    is_colexpr <- function(expr)
    {
      is.call(expr) &&
        (identical(expr[[1L]], quote(`::`)) || identical(expr[[1L]], quote(`:::`)))
    }
    is_funexpr <- function(expr)
    {
      is.call(expr) && identical(expr[[1L]], quote(`{`))
    }
    is_parenthesized <- function(expr)
    {
      is.call(expr) && identical(expr[[1L]], quote(`(`))
    }
    
    prepare_first <- function(expr)
    {
      as.call(c(expr[[1L]], quote(.), as.list(expr[-1L])))
      
    }
    is_first <- function(expr)
    {
      !any(vapply(expr[-1L], identical, logical(1L), quote(.)))
    }
    

    getElem = function(datas,row){
     if(length(row) == 1) row=c(row)
     f=datas
     for(i in row){
      
      f=if(!stringi::stri_detect_regex(i,"^-?[0-9]+$"))f[[i]]
     }
     return(f)
    }
  getElem2 = function(datas,row){
  if(length(row) == 1) row=c(row)
  f=datas
  for(id in row){
    i=id
    if (inherits(i,"StrCls") || is.character(i)) {
      i=if(inherits(i,"StrCls"))i$str else i
      if(length(i) > 1) {
        ind=lapply(i,function(e){
          last=length(f)  
          nn=stringi::stri_match(e,regex = "[0-9]")
          #print(nn)
          if(length(nn)>0 && !is.na(nn[[1]])){
            last=last-as.numeric(nn)
          }
        })
        ind=as.numeric(ind)
        f=f[ind]
      }else{
        last=length(f)  
        nn=stringi::stri_match(i,regex="[0-9]")
        if(length(nn)>0 && !is.na(nn[[1]])){
          last=last-as.numeric(nn)
        }
        f=f[as.numeric(last)]
      }

    }else if(stringi::stri_detect_regex(i,"^[0-9]+$")){
      f=f[[i]]
    }else if(length(i) > 1 || stringi::stri_detect_regex(i,"^-[0-9]+$")){
      f=f[i]
    }
  }
  return(f)
}
"%getElem2%"=getElem2
 `%getElem%` = getElem2
    
    smth = function(...){
      a=list(...)
      str(a)
    }
                                        
 curry = function(...){
  a=match.call()
  parent <- parent.frame()
  env    <- new.env(parent = parent)
  a2=a[[-1]]
  argss=as.list(a2)
  function_name = argss[[1]]
  argsFunc=argss[-1]
  #print(argsFunc)
  eval(as.call(c(purrr::partial,function_name,argsFunc)),env,env)
}
                                        StrCls=function(a=""){
  d=list()
  d$str=a
  class(d) = append(class(d),"StrCls")
  d
}
 .last=StrCls("last")

`%.=%` = function(a,b){
  aa=match.call()
  par=parent.frame()
  aa2=aa%getElem%2
  aa3=aa%getElem%3
  #print(l("<-",as.character(aa2),as.character(aa3)))
  eval(call("<-",as.character(aa2),as.character(a)%.%as.character(aa3)),par,par)
}

StrCls.print=function(l){
  l$str
}
minusStrCls=function(strcls,l){
  strcls$str =  strcls%.%"-"%.%l
  return(strcls)
}
getElems = function(datas,row){
  if(length(row) == 1) row=c(row)
  f=datas
  res=list()
  for(i in row){
    f=getElem2(datas,i)
    res=append(res,l(f))
  }
  return(res)
}
"%getElems%"=getElems      
   
   
 captureListRegex.=function(stri){
  str_extract_all(stri,"~~[^:]:?[^:]+|~[^:]:?[^:]+|[-'\"+*/.0-9a-zA-Z ]+")
}
   
captureCat. = function(a){
  capture.output(cat(a))
}
formulatoList..= function(ee){
  l3=as.list(ee)
  myList=list()
  l32=if(class(ee)=="call")capturePrint(ee) else captureCat(ee)
  lso=captureListRegex(l32)%getElem%1
  #print(l32)
  for(ii in lso){
    #print(ii)
   #print(str_detect(ii,"^~[^:]:?[^:]+$"))
    if(str_detect(ii,"^[0-9.]+$")){
      myList=append(myList,as.numeric(ii))
    }else if(str_detect(ii,"^[0-9.]:[0-9.]$")){
      myList=append(myList,eval(lazyeval::as_call(ii)))
    }else if(str_detect(ii,"^~[^:]:?[^:]+$")){
      dd=str_match(ii,"~([^:]:?[^:]+)")
      #return(dd)
      dd=dd[[2]]
      #print(dd)
      myList=append(myList,eval(lazyeval::as_call(dd)))
      
    }else if(str_detect(ii,"^~~[^:]:?[^:]+$")){
      dd=str_match(ii,"~~([^:]:?[^:]+)")
      dd=dd%getCol%2
      myList=append(myList,as.list(list(eval(lazyeval::as_call(dd)))))
      
    }
  }
  if(str_detect(l32,"~~")) return(myList)
  myList %each% l__(.)
}

toMatchCall.=function(..){
  match.call()[[2]]
}
 captureCat = function(a){
  capture.output(cat(a))
}
captureListRegex=function(stri){
  #print(stri)
  #print(str_extract_all(stri,"~[^:]+:[^:]+"))
  str_extract_all(stri,"(~~[^:]+:?[^:]+)|(~[^:]+:[^:]+)|([-/'\"+/*/.0-9a-zA-Z ]+)")
}
formulatoList.= function(ee){
  l3=as.list(ee)
  myList=list()
  l32=if(class(ee)=="call")capturePrint(ee) else captureCat(ee)
  lso=captureListRegex(l32) %getElem% 1
  #print(lso)
  for(ii in lso){
    #print(ii)
   #print(str_detect(ii,"^~[^:]:?[^:]+$"))
    if(str_detect(ii,"^[0-9.]+$")){
      myList=append(myList,as.numeric(ii))
    }else if(str_detect(ii,"^[0-9.]:[0-9.]$")){
      myList=append(myList,eval(lazyeval::as_call(ii)))
    }else if(str_detect(ii,"^~[^:]+:[^:]+$")){
      dd=str_match(ii,"~([^:]+:[^:]+)")
      #return(dd)
      dd=dd[[2]]
      #print(dd)
      myList=append(myList,eval(lazyeval::as_call(dd)))
      
    }else if(str_detect(ii,"^~~[^:]+:[^:]+$")){
      dd=str_match(ii,"~~([^:]+:[^:]+)")
      dd=dd%getCol%2
      myList=append(myList,as.list(list(eval(lazyeval::as_call(dd)))))
      
    }
  }
  if(str_detect(l32,"~~")) return(myList)
  myList %each% l__(.)
}

toMatchCall=function(a){
 m= match.call()[[2]]
 mm=if(class(m)=="call")capturePrint(m) else captureCat(m)
 #print(m)
 #print(mm)
  if(str_detect(mm,"^\\..+\\.$")){
    capturePrint(a)
  }else{
    m
  }
}




.matchCall = StrCls("matchCall")
.list=StrCls("list")
to = function(tow,what){
  #print(tow)
  .what.=match.call()[[3]]
  switch (tow$str,
    matchCall = toMatchCall(.what.),
    list=formulatoList.(toMatchCall(.what.))
  )
}
"%from%" = to
"%each:%" = function(a,b){
  bb=match.call()[[3]]
  #print(bb)
  cpp=capturePrint(bb)
  cpp=str_sub(cpp,start=2)
  csl=lapply(str_split(cpp,":")[[1]],function(e)as.formula("~"%.%e))
  llee="l_("%.%paste(csl,collapse = ",")%.%")"
  #print(lazyeval::as_name(llee))
  #env=new.env()
  #assign("llee", llee, envir = env)
  eval(call("%each%",a,lazyeval::as_call(llee)))
 # do.call(`%each%`,list(a,quote(lazyeval::as_name(llee))), envir = env)
}
             
`%future%` = function(aa,i){
    a=match.call()
    a=do.call("future",list(a[[3]]))
    value(a)
}
             
suppressWarningsGgplot =  function(s) tg(print(s))

formulaToList = function(a,e){
  ee=match.call()
  formulatoList.(ee[[3]])
  #print("ecalle")
}
"%vtl%"=formulaToList
 loadPlotUsefull = function(){
  tg(lib + "ggiraph" +
    "DT"+
    "git:thomasp85/patchwork")
  }
  getCol. = function(datas,col){
   ar=match.call()
   #print(formulatoList.(ar))
      datas[,formulatoList.(ar[[3]])]
    }
  getCol = function(datas,col){
      datas[,col]
   }
    `%getCol%` = getCol
     `%getCol.%` = getCol. 
    getRow. = function(datas,row){
     ar=match.call()
      datas[formulatoList.(ar[[3]]),]
    }
              getRow = function(datas,row){
      datas[row,]
    }
    `%getRow%` = getRow
     `%getRow.%` = getRow.
  qplotSameGraphEachCol = function(d,...){ 
    qplot(x = ind, y = values,data=stack(d),...)
  } #boxplot, violin
hidePlot= curry(with_(l1__(pdf(NULL)),l1__(invisible(dev.off())))(.))
             
 reduce = function(x,ops){
    Reduce(x,f=ops)
} 
`%reduce%`=reduce
             
 addNamesToList=function(vec,names){
    names(vec)=names
    vec
}


nothing=function(...){}
             
             
             build_matrix <- function(..., cf_eval_environment = parent.frame()) {
  v <- as.list(substitute(list(...))[-1])
  force(cf_eval_environment)
  lv <- length(v)
  # inspect input
  if(lv<1) {
    return(data.frame())
  }
  # unpack
  unpack_val <- function(vi) {
    if(length(vi)<=0) {
      stop("wrapr::build_frame unexpected NULL/empty element")
    }
    if(is.name(vi)) {
      viv <- cf_eval_environment[[as.character(vi)]]
      if(is.name(viv)) {
        stop(paste("wrapr::build_frame name",
                   vi,
                   "resolved to another name:",
                   viv))
      }
      if(is.call(viv)) {
        stop(paste("wrapr::build_frame name",
                   vi,
                   "resolved to call",
                   viv))
      }
      if(length(viv)<=0) {
        stop(paste("wrapr::build_frame name",
                   vi,
                   "resolved to NULL"))
      }
      vi <- viv
    }
    if(is.call(vi)) {
      if((length(vi)==3) && (is_infix(vi[[1]]))) {
        vi <- list(unpack_val(vi[[2]]),
                   as.name("sep"),
                   unpack_val(vi[[3]]))
      } else {
        viv <- eval(vi,
                   envir = cf_eval_environment,
                   enclos = cf_eval_environment)
        if(is.name(viv)) {
          stop(paste("wrapr::build_frame eval",
                     vi,
                     "resolved to another name:",
                     viv))
        }
        if(length(viv)<=0) {
          stop(paste("wrapr::build_frame eval",
                     vi,
                     "resolved to NULL"))
        }
        vi <- viv
      }
    }
    Reduce(c, lapply(vi, as.list))
  }
  vu <- lapply(v, unpack_val)
  vu <- Reduce(c, lapply(vu, as.list))
  ncol <- length(vu)
  if(ncol<1) {
    stop("wrapr::build_frame() zero columns")
  }
  is_name <- vapply(vu, is.name, logical(1))
  if(any(is_name)) {
    ncol <- which(is_name)[[1]]-1
    vu <- vu[!is_name] # filter out names
  }
  nrow <- (length(vu)/ncol) - 1
  if(abs(nrow - round(nrow))>0.1) {
    stop("wrapr::build_frame confused as to cell count")
  }
  matrix(vu,byrow=T,ncol=ncol)
}

is_infix <- function(vi) {
  vi <- as.character(vi)
  if(nchar(vi)<=0) {
    return(FALSE)
  }
  if(substr(vi,1,1)=="`") {
    vi <- substr(vi,2,nchar(vi)-1)
  }
  if(nchar(vi)<=0) {
    return(FALSE)
  }
  if(substr(vi,1,1)=="%") {
    return(TRUE)
  }
  syms <- c("::", "$", "@", "^", ":",
            "*", "/", "+", "-",
            ">", ">=", "<", "<=",  "==", "!=",
            "&", "&&",
            "|", "||",
            "~",
            "->",  "->>",
            "=",
            "<-", "<<-")
  if(vi %in% syms) {
    return(TRUE)
  }
  return(FALSE)
}

layout_build_matrix = function(...){
    a=match.call()
    a[[1]]=build_matrix
    b=eval(a)
    layout(b)
}
"%rep%" = function(ll,rr){
    rep(ll,rr) %>% paste0(collapse = '')
}
 "%join%" = function(ll,rr){
    paste0(ll,collapse = rr)
}
concatList = function(ll,rr,l1=list,l2=list){
    .env=parent.frame()

        #print(substitute(l2))
        call1=as.call(list(substitute(l1),substitute(ll)))
        call2=as.call(list(substitute(l2),substitute(rr)))
        #print(do.call(substitute(l2),list(substitute(rr)),envir = .env))
        eval1=eval(call1,.env,.env)
        eval2=eval(call2,.env,.env)
        if(is.list(eval1)){
            list.append(eval1,eval2)
        }else{
            list(eval1,eval2)
        }
    
 }

lConcat_ <- function(e1, e2,l1=l1,l2=l1) {
  .env <- parent.frame()
    called=do.call(bquote,list(substitute(list(substitute(e1), substitute(e2))),where=.env),envir=.env)
    #concatList2=curry(concatList(l1=l1,l2=l2,both=both))
    #print(concatList2)
     lsm=eval(called)
     lsm[["l1"]]=substitute(l1)
     lsm[["l2"]]=substitute(l2)
     #print(lsm)
    do.call(concatList,lsm,envir = .env)
}
l1=function(...){l(...)%getElem%1}
sepConcat = function(ll,rr){
    a=match.call()
    aa=a[[1]]
    aaa=as.character(aa)
    aaString=str_sub(aaa,2,str_length(aaa)-1)
    l1i=l1
    l2i=l1
    if(!str_detect(aaString,"^_{0,3},_{0,3}$")) stop("respect ^_{,3},_{,3}$")
    if(str_length(aaString)>1){
        splits=str_split(aaString,",")
        splits=splits%getElem%1
        split1=splits%getElem%1
        split2=splits%getElem%2
        #print(str_length(split1))
        split1nb=str_length(split1)
        split2nb=str_length(split2)
        
        lfn1="_" %rep% split1nb
        lfn2="_" %rep% split2nb
        lfn1OK="l1"%.%lfn1
        lfn2OK="l1"%.%lfn2
        
        l1i=as.name(lfn1OK)
        l2i=as.name(lfn2OK)
    }
    do.call(lConcat_,list(l1=l1i,l2=l2i,substitute(ll),substitute(rr)))
   # lConcat_(l1=l1i,l2=l2i,ll,rr)
}
#"%.%"=icor::`%.%`
"%,%" = sepConcat
"%_,%" = sepConcat
"%__,%" = sepConcat
"%___,%" = sepConcat
"%,_%" = sepConcat
"%_,_%" = sepConcat
"%__,_%" = sepConcat
"%___,_%" = sepConcat
"%,__%" = sepConcat
"%_,__%" = sepConcat
"%__,__%" = sepConcat
"%___,__%" = sepConcat
"%,___%" = sepConcat
"%_,___%" = sepConcat
"%__,___%" = sepConcat
"%___,___%" = sepConcat
  
print.rlang_lambda_function <- function(x, ...) {
  #cat_line("<lambda>")
  #x%>%str
    #UseMethod(x,"fn")
  #srcref <- attr(x, "srcref")
   # print(srcref)
  attributes(x) <- NULL
  x <- structure(x)
 catJ=  capturePrint(x) %getElems% l(l(1,1),l(2,1)) %join% "\n" %>%cat
  cat("<icor_list>\n")
  cat(catJ)  
}

             #al = Aleatoire$new()
#al$generer()
#al$generer(max=10)
#{1:100 %each% ~al$generer(max=10,double = T) }%>% hist

#1:100 %each% ~al$loi_discrete(c(0.05,0.05,0.1,0.6,0,0.2))
#{1:500 %each% ~rexp(1) }%>% density %>% plot ; {1:500 %each% ~al$exp(1) }%>% density %>% lines(col="red")
