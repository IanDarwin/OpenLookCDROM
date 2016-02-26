/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
#include <model_headers.h>
#define ca    x[0]
#define vv    x[1]
#define nn    x[2]
#define a0    x[3]
#define b0    x[4]
#define aa    x[5]
#define ba1   x[6]
#define ba2   x[7]
#define aca1  x[8]
#define aca2  x[9]
#define bca1  x[10]
#define nah   x[11]
#define rr    x[12]
#define iext  p[0]
#define ga1   p[1]
#define ga2   p[2]
#define goca  p[3]
#define gd    p[4]
#define gca1  p[5]
#define gca2  p[6]
#define gl    p[7]
#define gna   p[24]
#define ek    p[25]
#define el    p[26]
#define eca   p[27]
#define ena   p[28]
#define va    p[13]
#define koa   p[14]
#define kca   p[15]
#define cm    p[16]
#define ka1   p[17]
#define ka2   p[18]
#define knah  p[19]
#define cica  p[20]
#define eh  p[21]
#define vr  p[22]
#define vkr  p[23]
#define sr  p[8]
#define skr p[9]
#define cr p[10]
#define gh p[11]
#define vb p[12]
#define cn 282.0
#define kob 35.0
#define ka 140
#define kaca1 50.0
#define kbca1 16.0
#define kaca2 10.0
#define vkn 10.0
#define vn  (-25.0)
#define vao1  (-20.0)
#define vao2  (-23.0)
#define vaca1  (-11.0)
#define vbca1  (-50.0)
#define vaca2  22.0
#define vah  (-39.0)
#define vbh  (-40.0)
#define vam  (-6.0)
#define vbm  (-34.0)
#define skn (-22.0)
#define sn (-17.0)
#define sao1 (-23.0)
#define sao2 (-6.0)
#define sa (-16.0)
#define sb 6.0
#define saca1 (-7.0)
#define sbca1 8.0
#define saca2 (-7.0)
#define sah (-8.0)
#define sbh (-5.0)
#define sam (-20.0)
#define sbm (-13.0)
#define ff 0.6
#define c1 2.5
#define c2 0.6
#define c3 0.6
#define cao 0.05
#define cah 0.08
#define cam 0.11
#define cbm 15.0
/* ------------------------------------------------------------------------
   function used to define the vector field or map
   ----------------------------------------------------------------------*/
int lp4(f,x,p)
double f[],x[],p[];
{
double dca,dvv,dnn,da0,db0,daa,dba1,dba2,daca1,daca2,dbca1,dnah,drr;
double am,bm,xm,q1,q2,q3,q4,q5,q6,q7;
double ah,bh,fa,fb,an,ainf,taua,binf;
double sinf,sk,aca,cca,kn,xbca,ica;
extern double log(),exp();

/* i_Na inactivation h */
ah = cah*exp((vv- vah)/sah);
bh = 1./(1.+exp((vv- vbh)/sbh));
dnah = knah*(ah- nah*(ah+ bh));

/* Na+ conductance */
am = cam*(vv - vam)/(1.- exp((vv - vam)/sam));
bm = cbm*exp((vv - vbm)/sbm);
xm = am/(am+bm);
q1 = gna*xm*xm*xm*nah;

/* Ca current first activation ACA = a_CA1_inf */
aca=1.0/(1.0+exp(((vv - vaca1)/saca1)));
xbca=1./(1.+exp((vv - vbca1)/sbca1));
daca1=(aca - aca1)*kaca1;
dbca1=(xbca-bca1)*kbca1;
cca=1./(1.+exp((vv-vaca2)/saca2));
daca2=(cca-aca2)*kaca2;

/* Ca conductance */
q2=(gca1*aca1*bca1+gca2*aca2);

/* leak (Cl-) conductance */
q3=gl;

/* K current activation AN */
an=1.0/(1.+exp((vv - vn)/sn));
kn=cn/(1.+exp((vv-vkn)/skn));
dnn=kn*(an-nn);

/* Sag current activation s */
sinf=1./(1.+exp((vv-vr)/sr));
sk=cr*(1.0+exp((vv-(vkr))/skr));
drr=(sinf-rr)*sk;

/* K+ conductance : q4 = g'_K * n^4 */
q4=gd*nn*nn*nn*nn;

/* Ca activated outward current activation a_oCa */
fa = 1./(1.+exp((vv- vao1+ff*ca)/sao1))/(1.+exp((vv- vao2+ff*ca)/sao2));
fa=fa*ca/(c1+ca);
da0=(fa-a0)*koa;

/* Ca activated outward current inactivation FB */
fb=c2/(c3+ca);
ica=q2*(vv-eca);
dca=-cica*ica+kca*(cao-ca);
db0=(fb-b0)*kob;

/* Ca++ activated outward conductance : q5 = g'_KCa * fa * fb */
q5=goca*a0*b0;

/* A current activation ainf */
ainf=1./(1. + exp((vv-va)/sa));
daa=(ainf-aa)*ka;

/* A current inactivation BINF */
binf=1./(1.+exp((vv- vb)/sb));
dba1=(binf-ba1)*ka1;
dba2=(binf-ba2)*ka2;

/* A-conductance */
q6=(ga1*ba1+ga2*ba2)*aa*aa*aa;

/* Sag conductance : g7 = g'sag * a_sag   (identical with g'h) */
q7=gh*rr;

dvv=(iext+q1*ena+q2*eca+q3*el+q4*ek+q5*ek+q6*ek+q7*eh-vv*(q1+q2+q3+q4+q5+q6+q7))/cm;

f[0] = dca;
f[1] = dvv;
f[2] = dnn;
f[3] = da0;
f[4] = db0;
f[5] = daa;
f[6] = dba1;
f[7] = dba2;
f[8] = daca1;
f[9] = daca2;
f[10] = dbca1;
f[11] = dnah;
f[12] = drr;
}

/* ------------------------------------------------------------------------
   function used to define the Jacobian
   ----------------------------------------------------------------------*/
int lp4_jac(m,x,p)
double  **m, *x, *p;
{
 int i,j;
double dfdx[15][15],dca,dvv,dnn,da0,db0,daa,dba1,dba2,daca1,daca2,dbca1,drr;
double dpp,dhh,dvvall,nacontrib,newcontrib,newna,athresh,bthresh;
double am,bm,xm,q1,q2,q3,q4,q5,xa,q6,q7,s1,s2,s3;
double ah,bh,fa,fb,an,ainf,taua,binf,taub1,taub2;
double sinf,sk,pro,aca,xca,xbca,cca,kn;
extern double log(),exp(),pow();
   for(i=1; i<14; i++) for(j=1; j<14; j++) dfdx[i][j] = 0.0;
      dfdx[11][2] = -1/(pow(1.0+exp((vv-vbca1)/sbca1),2.0))/sbca1*exp((vv-vbca1
)/sbca1)*kbca1;
      dfdx[7][2] = -1/(pow(1.0+exp((vv-vb)/sb),2.0))/sb*exp((vv-vb)/sb)*ka1;
      dfdx[8][8] = -ka2;
      dfdx[2][10] = (gca2*eca-vv*gca2)/cm;
      dfdx[1][9] = -cica*gca1*bca1*(vv-eca);
      dfdx[2][8] = (ga2*aa*aa*aa*ek-vv*ga2*aa*aa*aa)/cm;
      dfdx[12][12] = knah*(-cah*exp((vv-vah)/sah)-1/(1.0+exp((vv-vbh)/sbh)));
      dfdx[5][5] = -kob;
      dfdx[9][2] = -1/(pow(0.1E1+exp((vv-vaca1)/saca1),2.0))/saca1*exp((vv-
vaca1)/saca1)*kaca1;
      dfdx[2][13] = (gh*eh-vv*gh)/cm;
      dfdx[7][7] = -ka1;
      dfdx[5][1] = -c2/pow(c3+ca,2.0)*kob;
      dfdx[13][2] = -1/(pow(1.0+exp((vv-vr)/sr),2.0))/sr*exp((vv-vr)/sr)*cr*(
0.1E1+exp((vv-vkr)/skr))+(1.0/(1.0+exp((vv-vr)/sr))-rr)*cr/skr*exp((vv-vkr)/skr
);
      dfdx[2][4] = (goca*b0*ek-vv*goca*b0)/cm;
      dfdx[6][2] = -1/(pow(1.0+exp((vv-va)/sa),2.0))/sa*exp((vv-va)/sa)*ka;
      dfdx[3][3] = -cn/(1.0+exp((vv-vkn)/skn));
      dfdx[1][10] = -cica*gca2*(vv-eca);
      s3 = 3.0*gna*cam*cam*cam*pow(vv-vam,2.0)/pow(1.0-exp((vv-vam)/sam),3.0)/
pow(cam*(vv-vam)/(1.0-exp((vv-vam)/sam))+cbm*exp((vv-vbm)/sbm),3.0)*nah*ena+3.0
*gna*cam*cam*cam*pow(vv-vam,3.0)/pow(1.0-exp((vv-vam)/sam),4.0)/pow(cam*(vv-vam
)/(1.0-exp((vv-vam)/sam))+cbm*exp((vv-vbm)/sbm),3.0)*nah*ena/sam*exp((vv-vam)/
sam)-3.0*gna*cam*cam*cam*pow(vv-vam,3.0)/pow(1.0-exp((vv-vam)/sam),3.0)/pow(cam
*(vv-vam)/(1.0-exp((vv-vam)/sam))+cbm*exp((vv-vbm)/sbm),4.0)*nah*ena*(cam/(1.0-
exp((vv-vam)/sam))+cam*(vv-vam)/pow(1.0-exp((vv-vam)/sam),2.0)/sam*exp((vv-vam)
/sam)+cbm/sbm*exp((vv-vbm)/sbm));
      s2 = s3-gna*cam*cam*cam*pow(vv-vam,3.0)/pow(1.0-exp((vv-vam)/sam),3.0)/
pow(cam*(vv-vam)/(1.0-exp((vv-vam)/sam))+cbm*exp((vv-vbm)/sbm),3.0)*nah-gca1*
aca1*bca1-gca2*aca2;
      s1 = s2-gl-gd*pow(nn,4.0)-goca*a0*b0-(ga1*ba1+ga2*ba2)*aa*aa*aa-gh*rr-vv*
(3.0*gna*cam*cam*cam*pow(vv-vam,2.0)/pow(1.0-exp((vv-vam)/sam),3.0)/pow(cam*(vv
-vam)/(1.0-exp((vv-vam)/sam))+cbm*exp((vv-vbm)/sbm),3.0)*nah+3.0*gna*cam*cam
*cam*pow(vv-vam,3.0)/pow(1.0-exp((vv-vam)/sam),4.0)/pow(cam*(vv-vam)/(1.0-exp((
vv-vam)/sam))+cbm*exp((vv-vbm)/sbm),3.0)*nah/sam*exp((vv-vam)/sam)-3.0*gna*
cam*cam*cam*pow(vv-vam,3.0)/pow(1.0-exp((vv-vam)/sam),3.0)/pow(cam*(vv-vam)/(
1.0-exp((vv-vam)/sam))+cbm*exp((vv-vbm)/sbm),4.0)*nah*(cam/(1.0-exp((vv-vam)/
sam))+cam*(vv-vam)/pow(1.0-exp((vv-vam)/sam),2.0)/sam*exp((vv-vam)/sam)+cbm/sbm
*exp((vv-vbm)/sbm)));
      s2 = 1/cm;
      dfdx[2][2] = s1*s2;
      dfdx[2][11] = (gca1*aca1*eca-vv*gca1*aca1)/cm;
      dfdx[6][6] = -ka;
      dfdx[4][1] = (-1/(pow(1.0+exp((vv-vao1+ff*ca)/sao1),2.0))/(1.0+exp((vv-
vao2+ff*ca)/sao2))*ca/(c1+ca)*ff/sao1*exp((vv-vao1+ff*ca)/sao1)-1/(1.0+exp((vv-
vao1+ff*ca)/sao1))/pow(1.0+exp((vv-vao2+ff*ca)/sao2),2.0)*ca/(c1+ca)*ff/sao2*
exp((vv-vao2+ff*ca)/sao2)+1.0/(1.0+exp((vv-vao1+ff*ca)/sao1))/(1.0+exp((vv-vao2
+ff*ca)/sao2))/(c1+ca)-1/(1.0+exp((vv-vao1+ff*ca)/sao1))/(1.0+exp((vv-vao2+ff*
ca)/sao2))*ca/pow(c1+ca,2.0))*koa;
      dfdx[10][10] = -kaca2;
      dfdx[12][2] = knah*(cah/sah*exp((vv-vah)/sah)-nah*(cah/sah*exp((vv-vah)/
sah)-1/(pow(1.0+exp((vv-vbh)/sbh),2.0))/sbh*exp((vv-vbh)/sbh)));
      dfdx[2][7] = (ga1*aa*aa*aa*ek-vv*ga1*aa*aa*aa)/cm;
      dfdx[4][2] = (-1/(pow(1.0+exp((vv-vao1+ff*ca)/sao1),2.0))/(1.0+exp((vv-
vao2+ff*ca)/sao2))*ca/(c1+ca)/sao1*exp((vv-vao1+ff*ca)/sao1)-1/(1.0+exp((vv-
vao1+ff*ca)/sao1))/pow(1.0+exp((vv-vao2+ff*ca)/sao2),2.0)*ca/(c1+ca)/sao2*exp((
vv-vao2+ff*ca)/sao2))*koa;
      dfdx[8][2] = -1/(pow(1.0+exp((vv-vb)/sb),2.0))/sb*exp((vv-vb)/sb)*ka2;
      dfdx[13][12] = 0.0;
      dfdx[2][9] = (gca1*bca1*eca-vv*gca1*bca1)/cm;
      dfdx[1][11] = -cica*gca1*aca1*(vv-eca);
      dfdx[12][9] = 0.0;
      dfdx[3][2] = -cn/pow(1.0+exp((vv-vkn)/skn),2.0)*(0.1E1/(1.0+exp((vv-vn)/
sn))-nn)/skn*exp((vv-vkn)/skn)-cn/(1.0+exp((vv-vkn)/skn))/pow(1.0+exp((vv-vn)/
sn),2.0)/sn*exp((vv-vn)/sn);
      dfdx[4][4] = -koa;
      dfdx[2][5] = (goca*a0*ek-vv*goca*a0)/cm;
      dfdx[13][13] = -cr*(0.1E1+exp((vv-vkr)/skr));
      dfdx[9][9] = -kaca1;
      dfdx[11][11] = -kbca1;
      dfdx[10][2] = -1/(pow(1.0+exp((vv-vaca2)/saca2),2.0))/saca2*exp((vv-vaca2
)/saca2)*kaca2;
      dfdx[2][12] = (gna*cam*cam*cam*pow(vv-vam,3.0)/pow(1.0-exp((vv-vam)/sam),
3.0)/pow(cam*(vv-vam)/(1.0-exp((vv-vam)/sam))+cbm*exp((vv-vbm)/sbm),3.0)*ena-vv
*gna*cam*cam*cam*pow(vv-vam,3.0)/pow(1.0-exp((vv-vam)/sam),3.0)/pow(cam*(vv-vam
)/(1.0-exp((vv-vam)/sam))+cbm*exp((vv-vbm)/sbm),3.0))/cm;
      dfdx[2][6] = (3.0*(ga1*ba1+ga2*ba2)*aa*aa*ek-3.0*vv*(ga1*ba1+ga2*ba2)*
aa*aa)/cm;
      dfdx[1][2] = -cica*(gca1*aca1*bca1+gca2*aca2);
      dfdx[1][1] = -kca;
      dfdx[2][3] = (4.0*gd*nn*nn*nn*ek-4.0*vv*gd*nn*nn*nn)/cm;
   for(i=0; i<14; i++) for(j=0; j<14; j++) m[i][j] = dfdx[i+1][j+1];
   
}

/* ------------------------------------------------------------------------
   function used to define inverse or approximate inverse
   ----------------------------------------------------------------------*/
int lp4_inv(y,x,p)
double    *y,*x,*p;
{
}


/* ------------------------------------------------------------------------
   function used to define aux functions of the varbs, time, or params
   ----------------------------------------------------------------------*/
int lp4_aux_func(f,x,p)
double *f,*x,*p;
{
double id,ioca,ia,ina,il,ih;
double am,bm,xm,q1,q2,q3,q4,q5,q6,q7;
double ah,bh,fa,fb,an,ainf,taua,binf;
double sinf,sk,aca,cca,kn,xbca,ica;
extern double log(),exp();

/* i_Na inactivation h */
ah = cah*exp((vv- vah)/sah);
bh = 1./(1.+exp((vv- vbh)/sbh));

/* Na+ conductance */
am = cam*(vv - vam)/(1.- exp((vv - vam)/sam));
bm = cbm*exp((vv - vbm)/sbm);
xm = am/(am+bm);
q1 = gna*xm*xm*xm*nah;

/* Ca current first activation ACA = a_CA1_inf */
aca=1.0/(1.0+exp(((vv - vaca1)/saca1)));
xbca=1./(1.+exp((vv - vbca1)/sbca1));
cca=1./(1.+exp((vv-vaca2)/saca2));

/* Ca conductance */
q2=(gca1*aca1*bca1+gca2*aca2);

/* leak (Cl-) conductance */
q3=gl;

/* K current activation AN */
an=1.0/(1.+exp((vv - vn)/sn));
kn=cn/(1.+exp((vv-vkn)/skn));

/* K+ conductance : q4 = g'_K * n^4 */
q4=gd*nn*nn*nn*nn;

/* Ca activated outward current activation a_oCa */
fa = 1./(1.+exp((vv- vao1+ff*ca)/sao1))/(1.+exp((vv- vao2+ff*ca)/sao2));
fa=fa*ca/(c1+ca);

/* Ca activated outward current inactivation FB */
fb=c2/(c3+ca);
ica=q2*(vv-eca);

/* Ca++ activated outward conductance : q5 = g'_KCa * fa * fb */
q5=goca*a0*b0;

/* A current activation ainf */
ainf=1./(1. + exp((vv-va)/sa));

/* A current inactivation BINF */
binf=1./(1.+exp((vv- vb)/sb));

/* A-conductance */
q6=(ga1*ba1+ga2*ba2)*aa*aa*aa;
q7=gh*rr;
id=q4*(vv-ek);
ioca=q5*(vv-ek);
ia=q6*(vv-ek);
ica=q2*(vv-eca);
ina=q1*(vv-ena);
il=q3*(vv-el);
ih = q7*(vv-eh);

   f[0] = id;
   f[1] = ioca;
   f[2] = ia;
   f[3] = ica;
   f[4] = ina;
   f[5] = il;
   f[6] = ih;
   f[7] = iext - id - ioca - ia - ica  - ina  - ih - il;
}
/* ------------------------------------------------------------------------
   Procedure to define default data for the dynamical system. NOTE: You
   may change the entries for each variable but DO NOT change the list of
   items.  If a variable is unused, NULL or zero the entry, as appropriate.
   ----------------------------------------------------------------------*/
int lp4_init()
{
/* ----------define the dynamical system in this segment -------------*/

int            n_varb=13;                       /* dim of phase space */
static char    *variable_names[]={"ca","v","n","ao","bo","aa","ba1","ba2","aca1","aca2","bca1","h","r"};    /* list of phase varb names */
static double  variables[]={1.08352,-43.2556,0.558158,0.184921E-03,0.338586,0.220593,0.0621127,0.0336595,0.0500917,0.0869492,0.308212,0.353208,0.1};            /* default varb initial values */
static double  variable_min[]={0.,-80.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.};         /* default varb min for display */
static double  variable_max[]={2.,60.0,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.};         /* default varb max for display */

static char    *indep_varb_name="time";        /* name of indep variable  */
double         indep_varb_min=0.;              /* default indep varb min for display */
double         indep_varb_max=1.;          /* default indep varb max for display */

int            n_param=29;                      /* dim of parameter space */
static char    *parameter_names[]={"iext","ga1","ga2","goca","gd","gca1","gca2","gl","sr","skr","cr","gh","vb","va","koa","kca","cm","ka1","ka2","kh","cica","eh","vr","vkr","gna","ek","el","eca","ena"};   /* list of param names */
static double  parameters[]={0.0,1.,1.3,5.,0.841,0.21,0.047,0.1,12.,-13.,0.1,0.1,-62.0,-43.0,45.,360.,0.0017,30.0,10.0,500.,300.,-10.,-110.,-100.,2300.,-86.0,-50.0,140.0,50.0};           /* initial parameter values */
static double  parameter_min[]={-1.,0.,0.,0.,0.,0.,0.,0.,0.,-20.,0.,0.,-70.,-50.,0.,0.,0.,0.,0.,0.,0.,-50.,-120.,-120.,0.,-100.,-60.,100.,40.};          /* default param min for display */
static double  parameter_max[]={1.,2.,2.,10.,1.,1.,1.,1.,20.,0.,1.,1.,-40.,-15.,1000.0,1000.0,2.0,100.,10.,1000.,1000.,0.,-50.,-50.,3000.,-50.,-40.,150.,60.};        /* default param max for display */

int            n_funct=8;                      /* number of user-defined functions */
static char    *funct_names[]={"id", "ioca", "ia", "ica", "ina", "il", "ih","itot"};   /* list of funct names; {""} if none */
static double  funct_min[]={-10.,-10.,-10.,-10.,-10.,-10.,-10.,-10.};            /* default funct min for display */
static double  funct_max[]={ 10., 10., 10., 10., 10., 10., 10.,10.};            /* default funct max for display */

int            manifold_type=EUCLIDEAN;        /* PERIODIC (a periodic varb) or EUCLIDEAN */
static int     periodic_varb[]={FALSE, FALSE}; /* if PERIODIC, which varbs are periodic? */
static double  period_start[]={0.,0.};         /* if PERIODIC, begin fundamental domain */
static double  period_end[]={1., 1.};          /* if PERIODIC, end of fundamental domain */

int            mapping_toggle=FALSE;            /* this is a map? TRUE or FALSE */
int            inverse_toggle=FALSE;           /* if so, is inverse FALSE, APPROX_INV, */
					       /* or EXPLICIT_INV? FALSE for vec field */

/*  In this section, input NULL or the name of the function which contains... */
int            (*def_name)()=lp4;             /* the eqns of motion */
int            (*jac_name)()=lp4_jac;             /* the jacobian (deriv w.r.t. space) */
int            (*aux_func_name)()=lp4_aux_func; /* the auxiliary functions */
int            (*inv_name)()=NULL;             /* the inverse or approx inverse */
int            (*dfdt_name)()=NULL;            /* the deriv w.r.t time */
int            (*dfdparam_name)()=NULL;        /* the derivs w.r.t. parameters */

/* ----------------end of dynamical system definition ----------------*/
#include <ds_define.c>
}
