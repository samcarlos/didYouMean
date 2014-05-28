##Takes String with errors in it. Returns word after either "Did you mean" or "Showing Results for" from google. If no Errors in String then returns original string.
##sam weiss


#for windows users you might need: options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))) 


library(RCurl)
didYouMean=function(input){
  input=gsub(" ", "+", input)
  doc=getURL(paste("https://www.google.com/search?q=",input,"/", sep=""))
  
  
  dym=gregexpr(pattern ='Did you mean',doc)
  srf=gregexpr(pattern ='Showing results for',doc)
  
  
  if(length(dym[[1]])>1){
    doc2=substring(doc,dym[[1]][1],dym[[1]][1]+1000)
    s1=gregexpr("?q=",doc2)
    s2=gregexpr("/&amp;",doc2)
    new.text=substring(doc2,s1[[1]][1]+2,s2[[1]][1]-1)
    return(gsub("[+]"," ",new.text))
    break
  }
  
  else if(srf[[1]][1]!=-1){
    doc2=substring(doc,srf[[1]][1],srf[[1]][1]+1000)
    s1=gregexpr("?q=",doc2)
    s2=gregexpr("/&amp;",doc2)
    new.text=substring(doc2,s1[[1]][1]+2,s2[[1]][1]-1)
    return(gsub("[+]"," ",new.text))
    break
  }
  else(return(gsub("[+]"," ",input)))
}  
