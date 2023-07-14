**RUNNING MACROS FOR REGRESSIONS;
* BRL Regressions;
%macro hreg(y, xs, dsname, cluster=%str(),
            weight=%str(),
            class=%str(),lc=%str(), print=YES, detail=NO);

   %varcov(&y, &xs, &dsname, cluster=&cluster,
           aweight=%str(), pweight=&weight,
           class=&class, lc=&lc, 
           yname=&y, print=&print, detail=&detail);

%mend;

********************************************************************;

%macro hlogit (y, xlist, dsname, cnt=1, cluster=%str(),
               weight=%str(), 
               class=%str(), lc=%str(), print=YES,detail=NO )  ;

ods listing close ;
%if %upcase(&detail)=YES %then 
ods listing; ;

data _hlogit1;
   set &dsname;
   _cnt = &cnt;
run;

proc genmod  data=_hlogit1   ;
  model &y. / _cnt = &xlist
       / dist=binomial link=logit ;
   output out=_outdata xbeta=_xbeta pred=_p;
  %if &weight ^= %then %str(scwgt &weight)  ; ;
run  ;
ods listing;

data _outdata ;
  merge _hlogit1
  _outdata (keep=_xbeta _p) ;
  _v = _cnt * _p * (1 - _p) ;
  _y01 = _xbeta + (&y-_cnt * _p)/_v ;
* _weight = _v*&weight %if &weight = %str() %then %str(1) ; ;
run ;

%varcov(_y01, &xlist, _outdata, cluster=&cluster,
        aweight=_v, pweight=&weight,
        class=&class,lc=&lc, 
        yname=&y, print=&print, detail=&detail);

%mend  ;

********************************************************************;

*VARCOV MACRO;
%macro varcov(y, xs, dsname, cluster=%str(),
              aweight=%str(), pweight=%str(),
              class=%str(),lc=%str(), 
              yname=%str(), print=YES,detail=NO, noint=%str());

%let p=noprint ;
%if %upcase(&detail)=YES %then %let p=   ;

** Set create a weight variable combining aweights and pweigths **;
data _vc1;
   set &dsname;
   _weight = 1 
               %if &aweight ^= %str() %then * &aweight ;
               %if &pweight ^= %str() %then * &pweight ;  ;;
run;

proc reg data=_vc1 outest=_vcbeta &p ;
   model &y = &xs / &noint;
   output out=_vc1 r=_resid;
   weight _weight;
run;

*title4 "Huber and HC2 Inference"  ;

** Delete observation with missing data or weights **;
** Initialize cluster variable                     **;
** Initialize aweight and pweight variables        **;
**    set weights to 1 if not specified            **;
data _vc1;
   set _vc1;
   intercept = 1;
   if _resid = .  then  delete  ;
   if _weight = . then delete  ;
   %if &cluster = %str() %then _cluster = _n_ ;
   %else _cluster = &cluster; ;
   _aweight = 1 %if &aweight ^= %str() %then * &aweight; ; 
   _pweight = 1 %if &pweight ^= %str() %then * &pweight; ; 
   keep _resid intercept &xs _aweight _pweight _cluster &cluster;
run;

proc sort data=_vc1 ;
  by _cluster  ;
run  ;

%if &noint = %str() %then %let intercep = intercept;
%else %let intercep = ;
proc iml symsize=20000000;

start isymroot(a);

*  call eigen(_eval, _evec, a);
   call svd(_evec, _eval, _v, a);
   _tmp = _evec * diag(_eval##(-0.5)) * t(_evec);
   return(_tmp);

finish isymroot;


start symroot(a);

*  call eigen(_eval, _evec, a);
   call svd(_evec, _eval, _v, a);
   _tmp = _evec * diag(_eval##(0.5)) * t(_evec);
   return(_tmp);

finish symroot;

start satt(l,g,_p,n);

gg = shape(0,n,n);
do jj = 1 to n;
   do kk = 1 to jj;
      _ind = (jj*(jj-1)*_p/2 +(kk-1)*_p + 1):(jj*(jj-1)*_p/2 + kk*_p);
      _tmp = t(l) * g[_ind,] * l;
      gg[jj,kk] = _tmp;
      gg[kk,jj] = _tmp;
      end;
   end;

_eval = eigval(gg);
_tmp = _eval[+,];
_tmp2 = t(_eval) * _eval;
_tmp3 = _tmp##2/_tmp2;

   return(_tmp3);
finish satt;

** Read all necessary data into IML **;
   use _vc1 var {_resid &intercep &xs _aweight _pweight _cluster};
   read all var{&intercep &xs} into x;
   read all var{_resid} into resid;
   read all var{_cluster} into cluster;
   read all var{_aweight} into aweight;
   read all var{_pweight} into pweight;
   close _vc1;


** Algorithm
   1) V = inv(diag(aw)) -- the inverse of the aweights 
   2) W_5 = diag(pw##0.5) -- a diagonal matrix of the square root of pweights
   3) Create omega = sum(xi' wi^1/2 inv(vi) wi^1/2 xi)^{-1}
   4) Loop over i
      5) Loop over j add to qi qij = xi omega xj' wj^1/2 inv(vj) wj^1/2 
      6) qi = i(mi) - qi
      7) Ai = root(vi) * symroot(root(vi) * qi * root(vi)) * root(vi)
      8) adjust the residual and add to the sum
****;

   use _vcbeta var{&intercep &xs};
   read all var{&intercep &xs} into beta;
   close _vcbeta;

   beta = t(beta);

   _p = nrow(beta);

   clusts = unique(cluster);
   n = ncol(clusts)  ;
   nobs = nrow(x);

   w_5 = pweight##0.5;

*** Create omega ***;
   omega = shape(0, _p, _p);

   do j = 1 to n;
      xj = x[loc(cluster=clusts[j]),];
      invvj = diag(aweight[loc(cluster=clusts[j])]);
      wj_5 = diag(w_5[loc(cluster=clusts[j])]);
      omega = omega + t(xj) * wj_5 * invvj * wj_5 * xj;
      end;
   omega = inv(omega);

*** Create gamma ***;
   gamma = t(x) * diag(w_5) * diag(aweight) * diag(w_5);


   xrrx =  repeat(0, _p, _p);
   xrrx2 =  repeat(0, _p, _p);

   do i = 1 to n;

      xi = x[loc(cluster=clusts[i]),];
      ri = resid[loc(cluster=clusts[i])];
      vi = diag(aweight[loc(cluster=clusts[i])]##-1);
      vi_5 = root(vi);
      invvi = diag(aweight[loc(cluster=clusts[i])]);
      wi_5 =  diag(w_5[loc(cluster=clusts[i])]);
      mi = nrow(xi);
      if i = 1 then
         phii = vi_5 - xi * omega * gamma[,t(loc(cluster=clusts[1]))] * vi_5;
      else phii = - xi * omega * gamma[,t(loc(cluster=clusts[1]))] * 
                    diag(aweight[loc(cluster=clusts[1])]##-0.5);
      do ii = 2 to n;
         vj_5 = diag(aweight[loc(cluster=clusts[ii])]##-0.5);
         if ii = i then 
            phiij = vj_5 - 
                    xi * omega * gamma[,t(loc(cluster=clusts[ii]))] * vj_5;
         else phiij = - xi * omega * gamma[,t(loc(cluster=clusts[ii]))] * vj_5;
         phii = phii || phiij;
         end;
      qi = phii * t(phii); 
      ai = vi_5 * isymroot(vi_5 * qi * vi_5) * vi_5;
*     ai = root(inv(qi));

      ui = omega * t(xi) * wi_5 * invvi * wi_5 * ai * phii;
      if i = 1 then u = ui;
      else u = u // ui;

      do k = 1 to i;
         ind = ((k-1) * _p + 1):(k * _p);
         gik = ui * t(u[ind,]);
         if i = 1 & k = 1 then g = gik;
         else g = g // gik;
         end;

   ** Standard Huber Adjustments **;
      tmp = t(xi) * wi_5 * invvi * wi_5 * ri ;
      xrrx = xrrx + tmp * t(tmp) ;

   ** BRL Adjustment **;
      tmp2 = t(xi) * wi_5 * invvi * wi_5 * ai * ri ;

      xrrx2 = xrrx2 + tmp2 * t(tmp2) ;
   end;

   free u;
   
   _df = shape(0,_p,1);
   do pp = 1 to _p;
      l = i(_p)[,pp];
      _df[pp] = satt(l,g,_p,n);
      end;
*  _df = n - 1;
   _tinv = tinv(0.975, _df); 

** Standard Huber/GEE sandwich variance-covariance matrix **;
   vc_hub = (n/(n-1)) * omega * xrrx * omega ;
   var_hub = vecdiag(vc_hub) ;
   se_hub = sqrt(var_hub)  ;
   t_hub = beta / se_hub ;
*  p_hub = 2 * (1 - probnorm(abs(t_hub)));
   p_hub = 2 * (1 - probt(abs(t_hub), _df));

** HC2 Adjusted sandwich variance-covariance matrix **;
   vc_brl = omega * xrrx2 * omega ;
   var_brl = vecdiag(vc_brl) ;
   se_brl = sqrt(var_brl)  ;
   t_brl = beta / se_brl  ;
*  p_brl = 2 * (1 - probnorm(abs(t_brl)));
   p_brl = 2 * (1 - probt(abs(t_brl), _df));
*  table = beta || se_hub || t_hub || p_hub || se_brl || t_brl || p_brl ;
   table = beta || se_brl || t_brl || _df || p_brl ;
   xname = {%if &noint = %str() %then Intercept; &xs} ;
   xname = t(xname);
*  cname = {beta se_hub  t_hub  p_hub  se_brl  t_brl  p_brl};
   cname = {beta se_brl t_brl satt_df p_brl};


   _dfn=t(_df) || nobs ;

   nomiss = 1;

   if type(clusts) = "N" then do;
      if clusts = . then nomiss = 0;
      end;
   else if clusts = "" then nomiss = 0;
if nomiss = 1 then do ;
  %if %upcase(&print)=YES %then %do ;
    print "Model for  %upcase(&yname)"  ;
    print "No. of Observations Used in Analysis:" nobs;
    print table [rowname=xname colname=cname format=8.3];
  %end;
end;

else print "******** WARNING: No Cluster Variable ********";

** Create Output Datasets **;
   create _dfn from _dfn [colname={&intercep &xs  nobs}] ;
   append from _dfn ;
   close _dfn ;


   create _huber4 from table [rowname=xname colname=cname];
   append from table [rowname=xname];
   close _huber4;

   create brl_cov from vc_brl [rowname=xname colname=xname];
   append from vc_brl [rowname=xname];
   close brl_cov;

   create hub_cov from vc_hub [rowname=xname colname=xname];
   append from vc_hub [rowname=xname];
   close hub_cov;

   tbeta=t(beta) ;
   create _beta from tbeta [colname=xname];
   append from tbeta [colname=xname];
   close _beta ;

** creates a row-vector from variance-covariance matrix **;
** used for repeated inference for multiple imputations **;
   vvarcov=t(symsqr(vc_brl)) ;
   create _varcov from vvarcov ;
   append from vvarcov;
   close _varcov;

%put &class ;
%if &class ^=%str() %then %clasloop(&class);
%put &lc ;
%if &lc ^=   %then %lincomb(&lc) ;
quit;

%mend;

