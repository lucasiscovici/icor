# ICOR, Correlation and Usefull Functions, Operators and Classes

# EQ</br>
 *Test values each in vector*</br>
 < <= == >= ></br>
 </br>
 **%||%**</br> 
 ```R
> c(4,2,1) %||% c(9,4,0)  < 3
[1] FALSE
  ```
 **%|%**</br>
 ```R
   >c(4,2,1) %|% c(9,4,0)  < 3
[1] FALSE  TRUE  TRUE
 ```
   **%&&%** </br>
 ```R
  > c(4,2,1) %&&% c(9,4,0)  < 3
[1] FALSE
 ```
 **%&%**</br>
  ```R
  > c(4,2,1) %&% c(9,4,0)  < 3
[1] FALSE FALSE  TRUE
  ```
# list To Parameters of a Function
 *Call function of the right side, with elements of the list passed in the left side*</br>
## Operators
 - %...>%(#list|#atomic,#function) (keep names)
 ```R
> list(mean=10,n=5,sd = 2) %...>% rnorm
[1] 12.817348 10.284119 11.355081  7.372176  7.795275

> list(mean=10,n=5,sd = 2) %...>% smth
List of 3
 $ mean: num 10
 $ n   : num 5
 $ sd  : num 2
 
 
 ```
  - %..._>%(#list|#atomic,#function) (not keep names)
 ```R
> list(mean=10,n=5,sd = 2) %..._>% rnorm
 [1]  4.5024198  8.4893204 10.0372266  4.5468650  3.7732437  4.1219222
 [7]  3.7688349  0.1318678  0.9097222  7.8097455

> list(mean=10,n=5,sd = 2) %..._>% smth
List of 3
 $ : num 10
 $ : num 5
 $ : num 2
 
 ```
 - %listToDotsFn%(#list,#function) (keep names) 
 ```R
 > list(mean=10,n=5,sd = 2) %listToDotsFn% rnorm
[1] 11.396479 10.744122  8.012919 11.297808 12.601390
 
  > list(ind="a",day=3,month=4) %listToDotsFn% smth
List of 3
 $ ind  : chr "a"
 $ day  : num 3
 $ month: num 4
 
 ```
 - %listToDotsFn_%(#list,#function) (not keep names) 
 *(see "smth" below)*</br>
 ```R
 > list(ind="a",day=3,month=4) %listToDotsFn_% smth
List of 3
 $ : chr "a"
 $ : num 3
 $ : num 4

 ```
# String Concat
 A VOIR%.=%</br>
 **%.%**</br>
 ```R
 > "ll"%.%"kk"%.%"kkd"
[1] "llkkkkd"
 ```
# Usefull Utils</br>
 **curry(fn)**</br>
 ```R
 > a=curry(rnorm(10))
> a
<partialised>
function (...) 
rnorm(10, ...)
> a(10)
 [1]  9.822035  9.897887 10.289826  9.588149  9.705572 11.554598
 [7] 11.485561  8.851399 12.550492 10.740860
 ```
 **smth**</br>
 *Return str of list of dots* w/br>
 ```R
 > 3 %,% 4 %,% l(d=3) %->% smth
 List of 3
 $ : num 3
 $ : num 4
 $ :List of 1
  ..$ d: num 3
  
  > smth(1,2,3,4,5,"kkk",rnorm,l1___(rnorm(10)))
  List of 8
 $ : num 1
 $ : num 2
 $ : num 3
 $ : num 4
 $ : num 5
 $ : chr "kkk"
 $ :function (n, mean = 0, sd = 1)  
 $ :function (...)  
  ..- attr(*, "class")= chr [1:2] "purrr_function_partial" "function"
  ..- attr(*, "body")= language ~(function (n, mean = 0, sd = 1)  .Call(C_rnorm, n, mean, sd))(10, ...)
  .. ..- attr(*, ".Environment")=<environment: 0x177e31b0> 
  ..- attr(*, "fn")= symbol rnorm
  
 ```
 **startsWithFromList(list,string)**</br>
 *filter list with element who begin with string*</br>
 ```R
 > c("hello","he lo","hehe","bonjour","au revoir") %>% startsWithFromList("he")
[1] "hello" "he lo" "hehe"
 
 ```
 **addNamesToList(list,names)**</br>
 *add names to list*</br>
 ```R
 > c("hello","he lo","hehe","bonjour","au revoir") %>% addNamesToList(1:5)
          1           2           3           4           5 
    "hello"     "he lo"      "hehe"   "bonjour" "au revoir" 
 
 > c("hello","he lo","hehe","bonjour","au revoir") %>% addNamesToList(1:5 %each% {"d"%.%.})
         d1          d2          d3          d4          d5 
    "hello"     "he lo"      "hehe"   "bonjour" "au revoir" 
  
 > l("hello","he lo","hehe","bonjour","au revoir") %>% addNamesToList(1:5 %each% {"d"%.%.})
$d1
[1] "hello"

$d2
[1] "he lo"

$d3
[1] "hehe"

$d4
[1] "bonjour"

$d5
[1] "au revoir"

 ```
 **nothing(...)**</br>
 *return nothing with whatever params *</br>
# Usefull DataFrame</br>
 **toDF(list) toDFt(list)** </br>
 *convert an list to a dataframe with good colnames, rownames*</br>
 ```R
 > l("hello","he lo","hehe","bonjour","au revoir") %>% addNamesToList(1:5 %each% {"d"%.%.}) %>% toDF
   [,1]       
d1 "hello"    
d2 "he lo"    
d3 "hehe"     
d4 "bonjour"  
d5 "au revoir"

> l("hello","he lo","hehe","bonjour","au revoir") %>% addNamesToList(1:5 %each% {"d"%.%.}) %>% toDFt
     d1    d2   d3      d4        d5
1 hello he lo hehe bonjour au revoir

 ```
 **add_row_with_name(#data.frame)**</br>
 *add row with rowname in a data frame*</br>
 ```R
 > data.frame(a=6:10,b=1:5%each%{"b"%.%.})
   a  b
1  6 b1
2  7 b2
3  8 b3
4  9 b4
5 10 b5

> data.frame(a=6:10,b=1:5%each%{"b"%.%.}) %>% add_row_with_name(a=9,b="b5",name="9")
   a  b
1  6 b1
2  7 b2
3  8 b3
4  9 b4
5 10 b5
9  9 b5
 
 ```
 **dfRowToList**</br>
 *convert row of data frame to list*</br>
 ```R
 > data.frame(a=6:10,b=1:5%each%{"b"%.%.}) %>% dfRowToList
$V1
 a  b 
 6 b1 
Levels:  6 b1

$V2
 a  b 
 7 b2 
Levels:  7 b2

$V3
 a  b 
 8 b3 
Levels:  8 b3

$V4
 a  b 
 9 b4 
Levels:  9 b4

$V5
 a  b 
10 b5 
Levels: 10 b5


 ```
 **dfToHTML(#data.frame)**</br>
 *show an data frame in html (only in jupyter notebook)*</br>
 ![img](https://raw.githubusercontent.com/luluperet/icor/master/img/dfToHTML.png)
 # Aleatoire</br>
 Aleatoire</br>
 # Categorical</br>
 %by% %byGraph%</br>
 corrCatCon<br></br>
 graphCatCon<br></br>
 int.hist<br></br>
 # Usefull Utils</br>
 ## capture</br>
 captureCat capturePrint</br>
 ## embed</br>
 **embed**(x, height="100%",width="100%") </br>
 *Display widget x, in html in jupyter notebook*</br>
 *(see DT::datatable)*</br></br>
 ```R
graph=dataSim %>% ggplot(aes(x=time,y=bin1)) + geom_point() + geom_point_interactive(aes(data_id=rownames(dataSim)), size = 2) + theme_minimal()
graphWidget= graph %>% girafe(ggobj = .) %>% girafe_options(opts_hover(css = "fill:red;r:4pt;"))
embed(graphWidget,"500px","70%")
```
![Embed](https://raw.githubusercontent.com/luluperet/icor/master/img/embed.png)
*(see ggirafe)*</br></br>
 **embedDT**(dt,height="100%",width="100%",...)</br>
 *Display a data frame in html beautiful table interactive in **jupyter notebook***</br>
  *DT::datatable(dt,...)*
  ```R 
  embedDT(dataSim,"500px","100%",filter="top")
  #if Rstudio
  DT::datatable(iris,filter="top")
  ```
  ![EmbedDT](https://raw.githubusercontent.com/luluperet/icor/master/img/embedDT.png)
 *(see embed)*</br>
 ## plotWH
 **plotWH**(w=NULL,h=NULL)-> reset (it is a function)</br>
 *Change Plot Width/height*</br>
 ```R 
 resetWH = plotWH(w=10)
 ... #plot graphique
 resetWH() # when finish with modified width/height
 ```
 *(options repr.plot.(width/height))*</br>
 ## Warnings 
 **showWarning()**</br>
 **hideWarning()**</br>
 **toggleWarning()**</br>
 ```R 
 > testit <- function() warning("testit")
 > testit() #Warn
Warning message:
In testit() : testit
> hideWarning()
> testit() #not Warn
> showWarning()
> testit() #Warn
Warning message:
In testit() : testit
> toggleWarning()
> testit() #not Warn
> toggleWarning()
> testit() #Warn
Warning message:
In testit() : testit
 ```
 **tg**(smth)</br>
 *hide/suppress warnings and messages*</br>
 ```R 
 tg(smthWithWarningsOrMessages)
 ```
 **suppressWarningsGgplot**(ggplotToPlot)</br>
 *hide ggplots warnings*
 ```R 
 suppressWarningsGgplot(ggplotPlot)
 ```
 ## Reduce
 **%reduce%(x,ops)**</br>
 *Reduce list x with operator ops*
 # Usefull plot </br>
 densityLines densityPlt</br>
 **qplotSameGraphEachCol(d,...)**</br>
 *Plot geom asked for each col in d*(boxplot violin)</br>
 **hidePlot(func)**</br>
 *Hide plot printed in func*</br>
 **loadPlotUsefull**</br>
 # Usefull Library</br>
 lib</br>
 load</br>
 print.Lib</br>
 # Usefull Packages Icor</br>
 reloadIcor</br>
 update</br>
 updateReloadIcor</br>
 detachFast</br>
 # Each + Map</br>
 %each:% %each%</br>
 %eachCol% %eachRow% %eachRowCol.%</br>
 %eachFn%</br>
 %map%</br>
 %mapFns%</br>
 %Xtimes%</br>
 # List Sequence Formula Customize</br>
 formulatoList.</br>
 %from%</br>
 # Getter </br>
 %getCol% %getRow% %getCol.% %getRow.%</br>
 %getElem% %getElem2% %getElems%</br>
 # Icor</br>
 icor</br>
 icor.corrToStudent</br>
 icor.critical.r</br>
 icor.studentToCorr</br>
 icor.graph</br>
 # Lists</br>
 ## LISTS OPS
 *%*1,*2%*</br>
 * *1 must be constructed with _ and .*</br>
 * if you want that the left side must be interprete with "l_", you have to write _ for the left (%1)%</br>
 * *1 = _ *</br>
 * if you want *2 be normal like "l", just write nothing for *1 *</br>
 * *1 = _ *</br>
 * *2 = *</br>
 * Operator = %_,%*</br>
 ```R
 > rnorm(3) %_,% rnorm(3) #SAME AS l( l1_(rnorm(3)), rnorm(3) )
[[1]]
<icor_list>
function (..., .x = ..1, .y = ..2, . = ..1) 
rnorm(., 3)

[[2]]
[1]  0.2080809  1.2826301 -0.4169875

 ```
 * if you want you can change the *2, with "__" for interprete the right side with "l__"*</br>
 ```R
 > rnorm(3) %_,__% rnorm(3) #SAME AS l( l1_(rnorm(3)), l1__(rnorm(3)) )
 [[1]]
<icor_list>
function (..., .x = ..1, .y = ..2, . = ..1) 
rnorm(., 3)

[[2]]
<icor_list>
function (..., .x = ..1, .y = ..2, . = ..1) 
rnorm(3)

 ```
 **l l. %,% %.,.% %.,% %,.% **</br>
 *normal list but understand .()*</br>
 ```R
 > i = 3
> l(.(i),4) #SAME .(i) %,% 4
[[1]]
[1] 3

[[2]]
[1] 4


> l.(.(i),4) #SAME .(i) %.,% 4
Error in .(i) : impossible de trouver la fonction "."

```
 **ll**</br>
 *normal list but nested list(list())*
 **l_ l1_ %_,_% %_,% %,_% %__,_% %___,_% AND  symetrics **</br>
 *normal list but for each parameters return a function | l1_ return the first one | %,% *</br>
 *a parameter: function, formula*</br>
 ```R
 > l_( 
     rnorm(1),
     ~rnorm(4),
     { rnorm(mean=4)},
     rnorm
 ) #SAME  rnorm(1) %_,_% (~rnorm(4)) %,_% { rnorm(mean=4) } %,_% rnorm
[[1]]
<icor_list>
function (..., .x = ..1, .y = ..2, . = ..1) 
rnorm(., 1)

[[2]]
<icor_list>
function (..., .x = ..1, .y = ..2, . = ..1) 
rnorm(4)

[[3]]
<icor_list>
function (..., .x = ..1, .y = ..2, . = ..1) 
{
    rnorm(mean = 4)
}

[[4]]
<icor_list>
function (..., .x = ..1, .y = ..2, . = ..1) 
rnorm(.)

> l1_(
     rnorm(1),
     ~rnorm(4),
     { rnorm(mean=4) },
     rnorm
 ) # SAME AS rnorm(1) %_,_% (~rnorm(4)) %,_% { rnorm(mean=4) } %,_% rnorm %...>% l1
<icor_list>
function (..., .x = ..1, .y = ..2, . = ..1) 
rnorm(., 1)


> 3 %,_% rnorm(1) 
[[1]]
[1] 3

[[2]]
<icor_list>
function (..., .x = ..1, .y = ..2, . = ..1) 
rnorm(., 1)

```
 **l__ %__,__% %__,_% %__,% AND symetrics**</br>
 l___</br>
 %listToDotsFn_% %listToDotsFn%</br>
 # Select Col</br>
 catCol %catCol% notCatCol %!catCol%</br>
 numericCol %numericCol% notNumericCol %!numericCol%</br>
 # StrCls</br>
 StrCls</br>
 </br>
?? lapplys ??</br>
 # Tests </br>
 test_normal</br>
 test_same_distrib</br>
 # Future</br>
 (HenrikBengtsson/future)</br>
 %future%</br>
//e= d %>% girafe(ggobj = .) %>% girafe_options(opts_hover(css = "fill:red;r:4pt;"))
