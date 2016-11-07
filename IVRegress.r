##IVRegress function found on NoviceMetrics Blog
##http://novicemetrics.blogspot.com/2011/04/video-tutorial-on-iv-regression.html
## ----------------------------------- ## 
 ## A homegrown IV regression command   ## 
 ## that handles the case where there   ## 
 ## is one endogenous variable and many ## 
 ## (or one) instrument.                ## 
 ##                                     ## 
 ## Disadvantage: The command doesn't   ## 
 ## handle multiple endogenous vars     ## 
 ##                                     ## 
 ## Advantage: testing for relevance,   ## 
 ## displaying the first stage and      ## 
 ## conducting overidentification tests ## 
 ## are natural and easy.               ## 
 ## ----------------------------------- ## 
  
 ivregress = function(second, first, data){ 
    s.names = all.vars(second) 
    f.names  = all.vars(first) 
    data.names = names(data) 
    N = length(data[,1]) 
    all.names = c(s.names,f.names) 
    resp   = s.names[1] 
    endog  = f.names[1] 
    inst   = f.names[-1] 
    explan = s.names[-1] 
    exog   = explan[explan!=endog] 
    exog.f = paste(exog,collapse="+") 
    inst.f = paste(inst, collapse="+") 
    RHS    = paste(exog.f, inst.f, sep="+") 
  
    first.form = as.formula( paste(endog, "~", RHS)) 
    first.lm = lm(first.form, data) 
  
    ftest    = linearHypothesis(first.lm, inst, rep(0,length(inst))) 
  
    x.hat    = fitted(first.lm) 
    data2    = cbind(data,x.hat) 
    iname    = paste(endog,".hat",sep="") 
    names(data2) = c(names(data), iname) 
    data2.names = names(data2) 
  
    RHS2 = paste(exog.f,iname,sep="+") 
  
    second.form = as.formula(paste(resp, "~", RHS2)) 
    second.lm = lm(second.form, data2) 
  
    Xmat  = data2[c(exog,endog)] 
    Xmat2  = data2[c(exog,iname)] 
  
    z = summary(second.lm) 
  
    X     = as.matrix(cbind(1,Xmat)) 
    X2    = as.matrix(cbind(1,Xmat2)) 
  
    Y     = data[resp] 
    fit   = X%*%second.lm$coefficients 
    res   = Y - fit 
  
    ## Tests for overidentifying restrictions ## 
  
    data3 = cbind(data2, res) 
    names(data3) = c(names(data2), "res") 
  
    ## J test 
    J.test        = as.data.frame(matrix(c(1,2),nrow=1)) 
    names(J.test) = c("J.stat","P[J > J.stat ]") 
  
    ## Sargan's Test 
    S.test     = as.data.frame(matrix(c(1,2),nrow=1)) 
    names(S.test) = c("S.stat","P[S > S.stat ]") 
  
    if(length(inst)>1){ 
        J.form = as.formula(paste("res", "~", RHS)) 
        J.lm = lm(J.form, data3) 
  
        f.test = linearHypothesis(J.lm,inst, rep(0,length(inst))) 
        J.stat = length(inst)*f.test$F[2] 
  
        S.stat = N*summary(J.lm)$r.squared 
    } 
  
    J.test[1,1] = J.stat 
    J.test[1,2] = 1-pchisq(J.stat, length(inst)-1) 
  
    S.test[1,1] = S.stat 
    S.test[1,2] = 1-pchisq(S.stat, length(inst)-1) 
  
    xPx     = t(X2)%*%X2 
    xPx.inv = solve(xPx) 
    z$cov.unscaled     = xPx.inv 
    z$residuals        = res 
    z$sigma            = sqrt(mean(res^2)) 
    varcovmat          = z$cov.unscaled*z$sigma 
    coef               = z$coefficients[,1] 
    IV.SE              = z$sigma*sqrt(diag(xPx.inv)) 
    t.iv               = coef/IV.SE 
    p.val              = 2*(1-pnorm(abs(t.iv))) 
  
    z$coefficients[,2] = IV.SE 
    z$coefficients[,3] = t.iv 
    z$coefficients[,4] = p.val 
  
    result = list(summary(first.lm),z,ftest,J.test, S.test) 
    names(result) = c("first", "second", "ftest", "Jtest", "Sargan") 
  
 print("IV object successfully created. Use sum.iv() on object") 
 print("to learn about your 2SLS Regression") 
  
 return(invisible(result)) 
  
 } 
  
     
  
 sum.iv = function(reg.iv, first=FALSE, 
       second=TRUE, ftest=FALSE, overid=FALSE) { 
  
 x= rep(0,5) 
 if(first==TRUE) x[1] = 1 
 if(second==TRUE) x[2]= 2 
 if(ftest==TRUE) x[3]= 3 
 if(overid==TRUE) x[4:5] = 4:5 
 print(reg.iv[x]) 
  
 } 
