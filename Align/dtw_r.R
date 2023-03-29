dtw_r <- function(target,candidate,g,edge){


library(matlab)
library(ggplot2)

xo = target[,1]
ro = candidate[,1]

x=xo
r=ro
tx = target[,2]
tr = candidate[,2]

txsort <- sort(tx, decreasing=FALSE,index.return = TRUE)
trsort <- sort(tr, decreasing=FALSE,index.return = TRUE)

tx = txsort$x
x = x[txsort$ix]
tr = trsort$x
r = r[trsort$ix]

N = length(x)
M = length(r)


d=(repmat(x,1,M)-repmat(r,N,1))
d=d*d

d[1,]=d[1,]*edge
d[,M]=d[,M]*edge
d[N,]=d[N,]*edge
d[,1]=d[,1]*edge

D=zeros(size(d))
D[1,1]=d[1,1]

flag1=1
loop1 <- c(2:N)
loop2 <- c(2:M)
if (flag1==1) {
  for (n in loop1) {
    D[n,1]=d[n,1]+D[n-1,1];
  }
  for (m in loop2) {
    D[1,m]=d[1,m]+D[1,m-1];
  }
}


n=2
for (m in loop2) {
  D[n,m]=d[n,m]+min(g*D[n-1,m],D[n-1,m-1],g*D[n,m-1])
}

m=2
for (n in loop1) {
  D[n,m]=d[n,m]+min(g*D[n-1,m],D[n-1,m-1],g*D[n,m-1])
}

loop3 <- c(3:N)
loop4 <- c(3:M)

for (n in loop3) {
  for (m in loop4) {
    D[n,m]=d[n,m]+min(g*D[n-1,m],D[n-1,m-1],g*D[n,m-1],1.1*g*D[n,m-2],1.05*g*D[n-1,m-2],D[n-2,m-2],1.05*g*D[n-2,m-1],1.1*g*D[n-2,m])
  }
}



Dist=D[N,M]
n=N
m=M
k=1
w=vector()
w=t(c(N,M))
while ((n+m)!=2){
  if ((n-1)==0){
    m=m-1
} else if ((m-1)==0){
    n=n-1
} else if ((n-2)==0 & (m-2)==0){
    n=n-1
    m=m-1
} else if ((n-2)==0){
    vals<-c(D[n-1,m],D[n,m-1],D[n-1,m-1],D[n,m-2],D[n-1,m-2])
    number=which(vals==min(vals))
    if (length(number)>1){number<-number[1]}
    if (number==1){
      n=n-1
    } else if (number==2){
        m=m-1
    } else if (number==3){
        n=n-1
        m=m-1
    } else if (number==4){
        m=m-2
    } else if (number==5){
        n=n-1
        m=m-2
    }
} else if ((m-2)==0){
    vals<-c(D[n-1,m],D[n,m-1],D[n-1,m-1],D[n-2,m],D[n-2,m-1])
    number=which(vals==min(vals))
    if (length(number)>1){number<-number[1]}
    if (number==1){
      n=n-1
    } else if (number==2){
      m=m-1
    } else if (number==3){
      n=n-1
      m=m-1
    } else if (number==4){
      n=n-2
    } else if (number==5){
      n=n-2
      m=m-1
    }
} else {
    vals<-c(D[n-1,m],D[n,m-1],D[n-1,m-1],D[n,m-2],D[n-1,m-2],D[n-2,m-2],D[n-2,m-1],D[n-2,m])  
    number=which(vals==min(vals))
    if (length(number)>1){number<-number[1]}
    if (number==1){
      n=n-1
    } else if (number==2){
      m=m-1
    } else if (number==3){
      n=n-1
      m=m-1
    } else if (number==4){
      n=c(n,n) 
      m=c(m-1,m-2)
    } else if (number==5){
      n=c(NaN,n-1)
      m=c(m-1,m-2)
    } else if (number==6){
      n=c(n-1,n-2)
      m=c(m-1,m-2)
    } else if (number==7){
      n=n-2
      m=m-1
    } else if (number==8){
      n=c(n-1,n-2)
      m=c(m,m)
    }
}
k=k+1;
wn = c(n)
wm = c(m)
wnew<-t(rbind(wn,wm))
w<-rbind(w,wnew)

n = n[length(n)]
m = m[length(m)]
}




pl=which( (w[,1]>1 & w[,2]>1) | is.nan(w[,1]))
pl=append(pl, pl[length(pl)]+1)
    

ind1 = which(w[,2]==w[1,2])
pl = pl[ind1[length(ind1)]:length(pl)]

ind8 = which(w[,1]==w[1,1])
pl = pl[ind8[length(ind8)]:length(pl)]

nantest<-is.nan(w[,1])
ireal = which(nantest==FALSE)
inan = which(nantest==TRUE)
    
ind = intersect(ireal,pl)

tmp = matrix(data=NaN,length(pl),2)
indy=ind-(ind[1]-1)

for (j in 1:size(ind)[2]){   
tmp[indy[j],1] = tx[w[pl[j],1]]
tmp[indy[j],2] = ro[w[pl[j],2]]
}

ind2 = intersect(inan,pl)
indy2=ind2-(ind[1]-1)

for (ii in 1:size(ind2)[2]){
  tmp[indy2[ii],2] = ro[w[pl[indy2[ii]],2]]
  tmp[indy2[ii],1] = 0.5*(tx[w[pl[indy2[ii]]-1,1]] + tx[w[pl[indy2[ii]]+1,1]])
}
tmpflip=t(tmp)
tw=rev(tmpflip[1,])
rw=rev(tmpflip[2,])

if (length(ind8)>0){
append_t = rev(tr[w[1:ind8[length(ind8)],2]])
append_r = rev(ro[w[1:ind8[length(ind8)],2]])
} else{
  append_t=vector()
  append_r=vector()
}

if ((pl[length(pl)]+2)<size(w)[1]){
start_t = rev(tr[w[(pl[length(pl)]+2):size(w)[1],2]])
start_r = rev(ro[w[(pl[length(pl)]+2):size(w)[1],2]])
} else{
  start_t=vector()
  start_r=vector()
}
w_scale = (tx[w[1,1]]-tx[w[pl[length(pl)],1]])/(tr[w[pl[1],2]]-tr[w[pl[length(pl)],2]])
    

if (length(start_t)>0){
  start_t = w_scale*(start_t - start_t[length(start_t)]) + tw[1]
}
    
if (length(append_t)>0){
  append_t = w_scale*(append_t - append_t[1]) + tw[length(tw)]
}


pl=which(diff(tw)!=0)
dj=max(diff(c(0,pl,length(tw))))
tw2 = matrix(data=NaN,dj,length(pl)+1)
rw2 = matrix(data=NaN,dj,length(pl)+1)


tw2[1,1]=tw[1]
rw2[1,1]=rw[1]
ct2=1
ct3=1

for (ct in 2:length(tw)){
  if (is.nan(tw[ct])==TRUE & is.nan(tw[ct-1])==TRUE){
    ct3=ct3+1
  } else if (is.na(tw[ct])==TRUE & is.na(tw[ct-1])==TRUE){
      ct3=ct3+1
  } else if (is.nan(tw[ct])==FALSE & is.nan(tw[ct-1])==TRUE){
    #ct2=ct2+1
    ct3=1
  } else if (is.na(tw[ct])==FALSE & is.na(tw[ct-1])==TRUE){
      #ct2=ct2+1
      ct3=1
  } else if (is.nan(tw[ct])==TRUE & is.nan(tw[ct-1])==FALSE){
    #ct2=ct2+1
    ct3=1
  } else if (is.na(tw[ct])==TRUE & is.na(tw[ct-1])==FALSE){
    #ct2=ct2+1
    ct3=1
  } else{
    if (tw[ct]!=tw[ct-1]){
        ct2=ct2+1
        ct3=1
    } else {
        ct3=ct3+1
    }
    tw2[ct3,ct2]=tw[ct]
    rw2[ct3,ct2]=rw[ct]
  }
}

rw3=vector()
tw3=tw2[1,]
if (dj!=1){
  for (i in 1:size(rw2)[2]){
    rw3=c(rw3,mean(rw2[,i],na.rm=TRUE))
  }
  rw3[length(rw3)] = rw2[1,size(rw2)[2]]
} else {
    rw3=rw2;
}

rw2test<-is.nan(rw2)
r_all = rw2[which(rw2test==FALSE)]
tw2test<-is.nan(tw2)
t_all = tw2[which(tw2test==FALSE)]

ind_tx=vector()
if (length(tw3)!=length(tx)){ 

  ind_tw = which(tw3 %in% tx)
  for (i in 1:length(ind_tw)){
    ind=which(tx==tw3[ind_tw[i]])
    if (length(ind)>1){
      ind=ind[1]
    }
    ind_tx=c(ind_tx,ind)
  }

  t_out =tw3[ind_tw]
  ri = rw3[ind_tw]

  tx_out = tx[ind_tx]
  xo_out = xo[ind_tx]

} else {
  t_out = tw3
  ri = rw3

  tx_out = tx
  xo_out = xo
}

if ((length(ri)/length(r))<0.1){
  xc=NaN
} else {
  xc=cor(ri,xo_out,method="pearson")
}

overlap = length(ri)


ri = r_all
t_out = t_all

uniq_start_t=unique(start_t)
ind = vector()
for (jj in 1:length(uniq_start_t)){
  vv=which(start_t %in% uniq_start_t[jj])
  if (length(vv)>1){
    vv=vv[1]
  }
  ind=c(ind,vv)
}

if (length(ind)>1){
  ri = c(start_r[ind[1:length(ind)-1]], ri)
  t_out = c(start_t[ind[1:length(ind)-1]], t_out)
}


uniq_append_t=unique(append_t)
ind = vector()
for (jj in 1:length(uniq_append_t)){
  vv=which(append_t %in% uniq_append_t[jj])
  if (length(vv)>1){
    vv=vv[1]
  }
  ind=c(ind,vv)
}

if (length(ind)>1){
  ri = c(ri, append_r[ind[2:length(ind)]])
  t_out = c(t_out, append_t[ind[2:length(ind)]])
}

outputvals = list(ri,t_out,xc)
return(outputvals)
}


