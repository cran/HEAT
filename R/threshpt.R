threshpt <-
function(myformula=myformula, expvar=expvar, expdf=4,  nonparm=nonparm, myfamily=myfamily, mydata=mydata, startrng=startrng, endrng=endrng, searchunit=searchunit, plots=T, title0=NULL, title1_1=NULL, title1_2=NULL, title2=NULL, title3=NULL, title4=NULL, xlab0=NULL, xlab1_1=NULL, xlab1_2=NULL,xlab2=NULL, xlab3=NULL, xlab4=NULL, ylab0=NULL,  ylab1_1=NULL,ylab1_2=NULL, ylab2=NULL, ylab3=NULL, ylab4=NULL){
    #data(mort)
    #seoul=read6city(mort, 11)
    #seoul_lag=lagdata(seoul, c("meantemp","mintemp","meanpm10","meanhumi"), 5)

    #myformula=nonacc~meantemp+meanpm10+meanpress+meanhumi+ns(sn, 40) ; expvar="meantemp";  nonparm="sn"; myfamily="poisson"; mydata=seoul; startrng=23; endrng=30;searchunit=0.2; plots=T

    #myformula=nonacc~meantemp_m3+ns(sn,4*10)+dow+season+meanpm10_m4+meanpress+meanhumi ; expvar="meantemp_m3";  nonparm="sn"; myfamily="poisson"; mydata=seoul_lag; startrng=23; endrng=30;searchunit=0.2; plots=T   ; mydata$season<-as.factor(mydata$season)

    library(splines)

    fom<-paste(as.character(myformula)[c(2,1,3)], collapse="")      

    resvar<-as.character(myformula)[2]   #to get the name of response variable

    var2<-paste(expvar,"_2", sep="")     

    myform_glm=as.formula(paste(fom, var2, sep="+"))  # add expvar_2 to formula where expvar_2 is (expvar-threshold)_+ 

               x11()
        if(plots==T){
        
         
       	  a<-strsplit(as.character(myformula)[3], " ")[[1]]
          # strsplit: to get all elements of character vector
       	  # as.character(myformula)[3] means sum of all predictors.
       	  # "a" has all the names of linear predictors, "+", and others
        	b<-colnames(mydata)[match(a,colnames(mydata))]
        	# match the names of predictor and "a" hence we can pick up only linear predictors
        	para_var<-b[which(is.na(b)==F)]

        	d<-c(para_var, nonparm)


       	  a[which(a==expvar)]<-paste("ns(", expvar,",",expdf, ")",collapse="")
       	  np_glm_form<-paste(paste(as.character(myformula)[c(2,1)], collapse=""),paste(a, collapse=""), collapse="")

          m11<-glm(np_glm_form,data=mydata, family=myfamily)    # np_glm_form: resvar~ ns( expvar , expdf )+others


                
        # To construct data set for plotting nonlinear relationship between response variable and explanatory variable
        # The data consist of mean values of other variables(except for explanatory variable and categorical variable)
        # for the categorical variable, the baseline value is used
        
          wc<-c()
        	for(i in 1:length(d)){
            wc[i]<-class(mydata[, d[i]])}       # find out which variables are categorical 
          l_fac<-which(wc=="factor")            # l_fac: location of categorical variable

          if(length(l_fac)==0){
            mean_data<-apply(mydata[,d], 2, mean, na.rm=T)
            mydata_1<-matrix(rep(mean_data, each=dim(mydata)[1]),   dim(mydata)[1], length(mean_data))    # replicate the mean vector N times where N is the row dimmesion of data 
            mydata_1<-as.data.frame(mydata_1)
            colnames(mydata_1)<-d
            }
          if(length(l_fac)>0){
            mean_data<-apply(mydata[,d[-l_fac]], 2, mean, na.rm=T)
            mydata_1<-matrix(rep(mean_data, each=dim(mydata)[1]),   dim(mydata)[1], length(mean_data))
            mydata_1<-as.data.frame(mydata_1)
            colnames(mydata_1)<-d[-l_fac]
            
            if(length(l_fac)>1){
              mydata_1[,d[l_fac]]<- matrix(rep(apply(mydata[,d[l_fac]], 2, min), each=dim(mydata)[1]),   dim(mydata)[1], length(l_fac))   
              # If there are more than two categorical variables, calculate each baseline value and replicate them N times and factorize each of them
              # and then levels of them should be identical to those original levels
              for(i in 1:length(l_fac)){

              mydata_1[,d[l_fac][i]]<-as.factor(mydata_1[,d[l_fac][i]])
              levels(mydata_1[,d[l_fac][i]])<-levels(mydata[,d[l_fac][i]])
              }
            }
            
              # If there is one categorical variable, calculate baseline value and replicate it N times and factorize it
            if(length(l_fac)==1){
              mydata_1[,d[l_fac]]<- as.factor(matrix(rep(min(as.integer((mydata[,d[l_fac]]))), each=dim(mydata)[1]),   dim(mydata)[1], length(l_fac)))
            }
          }
      
             # For explanatory variable, generate sequence of length N from min(expvar)*0.9 to max(expvar)*1.1  
        mydata_1[,expvar]<-seq(min(mydata[,expvar],na.rm=T)*0.9 ,max(mydata[,expvar],na.rm=T)*1.1, length.out=dim(mydata)[1])




        yhat=predict(m11, se.fit=TRUE, newdata=mydata_1, type="link")
        #set confidence intervals
        uci=yhat$fit + 1.96 * yhat$se.fit
        lci=yhat$fit - 1.96 * yhat$se.fit

      if(is.null(ylab0)){
        if(myfamily=="poisson"){ylab0<-paste("log(E(", resvar, "))", sep="")}
        if(myfamily=="gaussian"){ylab0<-paste("E(", resvar, ")", sep="")}
        if(myfamily=="binomial"){ylab0<-paste("logit(E(", resvar, "))", sep="")}
        }

      if(is.null(xlab0)){xlab0=expvar}

      plot(mydata_1[,expvar],yhat$fit, ylim=c(min(lci), max(uci)), xlab=xlab0, ylab=ylab0 ,main=title0 )
      lines(mydata_1[,expvar],uci, lty=3)
      lines(mydata_1[,expvar],lci, lty=3)
    }# if



    thresh_best<-array("",c(8))

    mydata<-mydata[which(is.na(mydata[,expvar])==F),]

    if(endrng>max(mydata[,expvar])){endrng=floor(  max(mydata[,expvar])-1)}

    range_pt=c(startrng, endrng)
    diff_pt=endrng-startrng
    if (diff_pt<0) {
    print("error on search range ", range_pt)
    print("Range difference must be greater 0")
    stop
    }


    N=((diff_pt)*(1/searchunit)+1)*8
    M1<-matrix(numeric(N),ncol=8,byrow=TRUE,
    dimnames=list(c(),c("beta1","se1","p1","beta1+2","se1+2","p1+2","deviance","threshold")))
    M1[,8]<-seq(range_pt[1],range_pt[2],searchunit)


    # - threshold
    n_for= diff_pt*1/searchunit+1

    for (i in 1:n_for)
    {
    mydata[,var2]<-ifelse(mydata[, expvar]-M1[i,8]>0,mydata[, expvar]-M1[i,8],0)

    m11<-glm(myform_glm,data=mydata, family=myfamily)
    a<-summary(m11)
    ncoef = dim(vcov(m11))[1]
    k=which(rownames(a$coefficients)==expvar)
    c<-rep(0, ncoef)
    c[k]<-1; c[ncoef]<-1;
    want = c %*% m11$coef[which(is.na(m11$coef)==F)]
    se<-sqrt(t(c)%*%vcov(m11)%*%c)
    Z <- want/se
    p<-(1-pnorm(abs(Z)))*2
    m11$aic
    M1[i,1:7]<-c(round(c(a$coef[k,1],a$coef[k,2]),4), round(a$coef[k,4],4),round(c(want,se),4), round(p,4),m11$deviance)

    } # i


    thresh_best=M1[order(M1[,7],decreasing=FALSE)[1],]


    mydata[,var2]<-ifelse(mydata[, expvar]-thresh_best[8]>0,mydata[, expvar]-thresh_best[8],0)

    m11<-glm(myform_glm,data=mydata, family=myfamily)
    a<-summary(m11)
    a$coef


      pred_y<-predict(m11, newdata=mydata, type="link")

     # plot of estimated resvar and 95% C.I with other mean covariates
     # get the variable names from myformula
     # nonparametric variable is hard to get from the myformula hence we need the nonparm variable name.

   	  a<-strsplit(as.character(myformula)[3], " ")[[1]]
    	b<-colnames(mydata)[match(a,colnames(mydata))]
    	para_var<-b[which(is.na(b)==F)]

    	d<-c(para_var, nonparm)

      wc<-c()
    	for(i in 1:length(d)){
        wc[i]<-class(mydata[, d[i]])}
      l_fac<-which(wc=="factor")
      if(length(l_fac)==0){
        mean_data<-apply(mydata[,d], 2, mean, na.rm=T)
        mydata_1<-matrix(rep(mean_data, each=dim(mydata)[1]),   dim(mydata)[1], length(mean_data))
        mydata_1<-as.data.frame(mydata_1)
        colnames(mydata_1)<-d
        } else{
        mean_data<-apply(mydata[,d[-l_fac]], 2, mean, na.rm=T)
        mydata_1<-matrix(rep(mean_data, each=dim(mydata)[1]),   dim(mydata)[1], length(mean_data))
        mydata_1<-as.data.frame(mydata_1)
        colnames(mydata_1)<-d[-l_fac]
      
        if(length(l_fac)>1){
          mydata_1[,d[l_fac]]<- matrix(rep(apply(mydata[,d[l_fac]], 2, min), each=dim(mydata)[1]),   dim(mydata)[1], length(l_fac))
          for(i in 1:length(l_fac)){

          mydata_1[,d[l_fac][i]]<-as.factor(mydata_1[,d[l_fac][i]])
          levels(mydata_1[,d[l_fac][i]])<-levels(mydata[,d[l_fac][i]])
          }
        }
        if(length(l_fac)==1){
          mydata_1[,d[l_fac]]<- as.factor(matrix(rep(min(as.integer((mydata[,d[l_fac]]))), each=dim(mydata)[1]),   dim(mydata)[1], length(l_fac)))
        }
                }

        mydata_1[,expvar]<-seq(min(mydata[,expvar],na.rm=T)*0.9 ,max(mydata[,expvar],na.rm=T)*1.1, length.out=dim(mydata)[1])
        mydata_1[,resvar]<-mydata[,resvar]
        mydata_1[,var2]<-ifelse(mydata_1[, expvar]-thresh_best[8]>0,mydata_1[, expvar]-thresh_best[8],0)



      #### meanpm10 and nonacc

        yhat=predict(m11, se.fit=TRUE, newdata=mydata_1, type="link")
        #set confidence intervals
        uci=yhat$fit + 1.96 * yhat$se.fit
        lci=yhat$fit - 1.96 * yhat$se.fit

        if(myfamily=="poisson"|myfamily=="quasipoisson"){
          #take exponential because of log-link function
          predyhat=exp(yhat$fit)
          predlci=exp(lci)
          preduci=exp(uci)

        }

        if(myfamily=="gaussian"){
          #take exponential because of log-link function
          predyhat=yhat$fit
          predlci=lci
          preduci=uci

          RR<-NULL
        }


        if(myfamily=="binomial"|myfamily=="quasibinomial"){
          #take exponential because of log-link function
          predyhat=exp(yhat$fit)/(1+exp(yhat$fit))
          predlci=exp(lci)/(1+exp(lci))
          preduci=exp(uci)/(1+exp(uci))

          RR<-NULL
        }



    if(plots==T){
      x11()
      par(mfrow=c(2,1))
      if(is.null(title1_1)){title1_1<- paste("timeseries plot of ", resvar, sep="") }
      if(is.null(title1_2)){title1_2<- paste("timeseries plot of ", expvar, sep="") }
      if(is.null(ylab1_1)){ylab1_1<-resvar}
      if(is.null(ylab1_2)){ylab1_2<-expvar}
      if(is.null(xlab1_1)){xlab1_1<-"time"}
      if(is.null(xlab1_2)){xlab1_2<-"time"}

      plot(mydata$date,mydata[,resvar], main=title1_1, ylab=ylab1_1, xlab=xlab1_1)
      plot(mydata$date,mydata[,expvar], main=title1_2, ylab=ylab1_2, xlab=xlab1_1)

      x11()
      par(mfrow=c(1,1))
      if(is.null(title2)){title2<- "Deviance plot"}
      if(is.null(ylab2)){ylab2<-"deviance"}
      if(is.null(xlab2)){xlab2<-"threshold"}

      plot(M1[,8], M1[,7], main=title2, xlab=xlab2, ylab=ylab2)
      abline(v=thresh_best[8], col=2)

      # plot of estimated resvar
      x11()
      if(is.null(title3)){title3<-paste("Estimated ", resvar, " vs ", expvar, sep="")}
      if(is.null(ylab3)){ylab3<-resvar}
      if(is.null(xlab3)){xlab3<-expvar}
      plot(mydata[,expvar], mydata[,resvar],xlim=c(min(mydata[,expvar]), max(mydata[,expvar])), main=title3, xlab=xlab3, ylab=ylab3)
      points(mydata[,expvar], exp(pred_y), col=2)
      abline(v=thresh_best[8], col=4)


      dev.new()
        #plot nonacc and mean temp

        if(is.null(title4)){title4<-paste("Predicted ",resvar, " vs ", expvar, " and 95% C.I with other mean covariates", sep="")}
        if(is.null(ylab4)){ylab4<-paste("Predicted E(",resvar, ")", sep="")}
        if(is.null(xlab4)){xlab4<-expvar}
        plot(seq(min(mydata_1[,expvar]), max(mydata_1[,expvar]),length=100), seq(min(predlci), max(preduci), length=100)
        ,type="n", ylab=ylab4, xlab=xlab4,xlim=c(min(mydata_1[,expvar]), max(mydata_1[,expvar])), main=title4)

        # predicted line
        x<- mydata_1[,expvar]
        lines(x[order(x)], predyhat[order(x)], col="red",lwd=2, lty=1)
        # lower CI
        lines(x[order(x)], predlci[order(x)], col=4,lwd=1, lty=2)
        # upper CI
        lines(x[order(x)], preduci[order(x)], col=4,lwd=1, lty=2)

        abline(v=thresh_best[8], col=3)



    }#if



        aa<-summary(m11)$coef
        parm_coef<-aa[which(is.na(match(rownames(aa),para_var[-which(para_var==expvar)]))==F),-3]

        # get coefficients of categorical variables
        cat_dim<-length(l_fac)
        cat_l<-c()
        cat_tmp<-NULL
        for(i in 1:cat_dim){
          ll<-length(strsplit(d[l_fac] , "")[[i]])      # get the lengths of categorical variables
          cat_tmp<-rbind(cat_tmp,aa[which(match(substr(rownames(aa),1, ll), d[l_fac][i])==1),-3])     #substr: extract substrings in a character vector
          }
          
        parm_coef<-rbind(parm_coef, cat_tmp)

        parm_coef



    return(list(best_fit=thresh_best, formula=myform_glm, glm_obj=m11, parm_coef=parm_coef, expvar=expvar))
    #return(list(best_fit=thresh_best, formula=myform_glm, glm_obj=m11))

}
