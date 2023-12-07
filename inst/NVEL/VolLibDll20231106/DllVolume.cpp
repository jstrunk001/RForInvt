// DllVolume.cpp: implementation of the DllVolume class.
//
//////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include "stdafx.h"
#include "Dlltest.h"
#include "DlltestDlg.h"
#include "DllVolume.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

DllVolume::DllVolume()
{
   m_iErrflag = 0;
}

DllVolume::~DllVolume()
{

}

void DllVolume::Get_DllVolume()
{
// standard variables
   int REGN,HTLOG,HTREF,FCLASS,HTTFLL,ERRFLAG,TLOGS,BA,SI,SPCODE;
   int CUTFLG,BFPFLG,CUPFLG,CDPFLG,CUSFLG,CDSFLG,SPFLG,VERSION;
   float DBHOB,DRCOB,HTTOT,HT1PRD,HT2PRD,STUMP;
   float UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2;
   float DBTBH,BTR,MTOPP,MTOPS,NOLOGP,NOLOGS;
   int DIST;
   
//   array defintions
   int I3 = 3;
   int I7 = 7;
   int I15 = 15;
   int I20 = 20;
   int I21 = 21;

   const int i3 = 3;
   const int i7 = 7;
   const int i15 = 15;
   const int i20 = 20;
   const int i21 = 21;

   float VOL[i15];
   float LOGLEN[i20];
   float BOLHT[i21];
// 2-dimentional array definitions
   FMATRIX<float> LOGVOL(i7,i20);
   FMATRIX<float> LOGDIA(i21,i3);
   
// character defintions
   const size_t len2=2;
   const size_t len3=3;
   const size_t len5=5;
   const size_t len11=11;
//   char buff[10];
   char cVoleq[11],cHttype[2],cLive[2],cForst[3],cProd[3],cConspec[5],cCtype[2],cDist[3];
// assign the Fortran variables
   sprintf(cVoleq,"%s",m_sVoleq);
   sprintf(cHttype,"%s",m_sHttype);
   sprintf(cLive,"%s",m_sLive);
   if(m_sForst == "") m_sForst = "1";   // check for null forest number
   sprintf(cForst,"%s",m_sForst);
   if(m_sDist == "") m_sDist = "1";   // check for null forest number
   sprintf(cDist,"%s",m_sDist);
   sprintf(cProd,"%s",m_sProd);
   sprintf(cConspec,"%s",m_sConspec);
   CString sCtype = "F";
   sprintf(cCtype,"%s",sCtype);
   

   CHARACTER HTTYPE(cHttype,len2);
   CHARACTER LIVE(cLive,len2);
   CHARACTER FORST(cForst,len3);
   CHARACTER DIST(cDist,len3);
   CHARACTER PROD(cProd,len3);
   CHARACTER CONSPEC(cConspec,len5);
   CHARACTER VOLEQ(cVoleq,len11);
   CHARACTER CTYPE(cCtype,len2);
   
   REGN =   m_iRegn;
   MTOPP =  m_fMtopp;
   MTOPS =  m_fMtops;
   STUMP =  m_fStump;
   DBHOB =  m_fDbhob;
   DRCOB =  m_fDrcob;
   HTTOT =  m_fHttot;
   HTLOG =  m_iHtlog;
   HT1PRD = m_fHt1prd;
   HT2PRD = m_fHt2prd;
   UPSHT1 = m_fUpsht1;
   UPSHT2 = m_fUpsht2;
   UPSD1 =  m_fUpsd1;
   UPSD2 =  m_fUpsd2;
   HTREF =  m_iHtref;
   AVGZ1 =  m_fAvgz1;
   AVGZ2 =  m_fAvgz2;
   FCLASS = m_iFclass;
   DBTBH =  m_fDbtbh;
   BTR =    m_fBtr;
   CUTFLG = m_iCutflg;
   BFPFLG = m_iBfpflg;
   CUPFLG = m_iCupflg;
   CDPFLG = m_iCdpflg;
//   CUSFLG = m_iCusflg;
//   CDSFLG = m_iCdsflg;
   SPFLG = m_iSpflg;
   HTTFLL = m_iHttfll;
   TLOGS = m_iTlogs;
   NOLOGP = 0.0;
   NOLOGS = 0.0;
   BA = 0;
   SI = 0;
   DIST = m_iDist;
   for(int nn=0;nn<20;nn++)
      LOGLEN[nn] = m_fLoglen[nn];

   ERRFLAG = 0;
   // call to version number
   VERNUM(&VERSION);
   m_nVersionNum = VERSION;

   // check volume equation number
   if(m_sVoleq.GetLength() < 10)
   {
      GETVOLEQ(&REGN,FORST,DIST,&SPCODE,PROD,VOLEQ,&ERRFLAG);
   }


// call to fortan dll
   VOLUMELIBRARY(&REGN,FORST,VOLEQ,&MTOPP,&MTOPS,&STUMP,&DBHOB,&DRCOB,HTTYPE,&HTTOT,
         &HTLOG,&HT1PRD,&HT2PRD,&UPSHT1,&UPSHT2,&UPSD1,&UPSD2,&HTREF,&AVGZ1,
         &AVGZ2,&FCLASS,&DBTBH,&BTR,I3,I7,I15,I20,I21,VOL,LOGVOL,LOGDIA,LOGLEN,
         BOLHT,&TLOGS,&NOLOGP,&NOLOGS,&CUTFLG,&BFPFLG,&CUPFLG,&CDPFLG,&SPFLG,
         CONSPEC,PROD,&HTTFLL,LIVE,&BA,&SI,CTYPE,&ERRFLAG, &DIST);
// save output
   m_sVoleq = VOLEQ;
   m_fMtopp = MTOPP;
   m_fHt1prd = HT1PRD;
   m_iErrflag = ERRFLAG;
   m_iTlogs = TLOGS;
   m_fNologp = NOLOGP;
   m_fNologs = NOLOGS;
   m_iFclass = FCLASS;
   for(int i=0;i<15;i++)
      m_fVol[i] = VOL[i];
   for(int j=0;j<20;j++)
   {
      m_fLoglen[j] = LOGLEN[j];
      m_fLogvol_Bdft[j] = LOGVOL(0,j);
      m_fLogvol_Cuft[j] = LOGVOL(3,j);
   }
   for(int k=0;k<20;k++)
   {
      m_fLogsc[k] = LOGDIA(k,0);
      m_fLogdib[k] = LOGDIA(k,1);
      m_fLogdob[k] = LOGDIA(k,2);
      m_fBolht[k] = BOLHT[k];
   }
}
//                               Voleq
CString DllVolume::Get_Voleq()
{
   return m_sVoleq;
}
void DllVolume::Set_Voleq(CString sVoleq)
{
   m_sVoleq = sVoleq;
}
//                         iRegn
int DllVolume::Get_Regn()
{
   return m_iRegn;
}
void DllVolume::Set_Regn(int iRegn)
{
   m_iRegn = iRegn;
}
//                               sForst
CString DllVolume::Get_Forst()
{
   return m_sForst;
}
void DllVolume::Set_Forst(CString sForst)
{
   m_sForst = sForst;
//   int iLen = m_sForst.GetLength();
//   if(iLen < 2)
//      m_sForst = '0' + sForst;
}
//                               sDist
CString DllVolume::Get_Dist()
{
   return m_sDist;
}
void DllVolume::Set_Dist(CString sDist)
{
   m_sDist = sDist;
}
//                             sProd
CString DllVolume::Get_Prod()
{
   return m_sProd;
}
void DllVolume::Set_Prod(CString sProd)
{
   m_sProd = sProd;
}
//                               VosHttyp
CString DllVolume::Get_Httype()
{
   return m_sHttype;
}
void DllVolume::Set_Httype(CString sHttype)
{
   m_sHttype = sHttype;
}
//                             sLive
CString DllVolume::Get_Live()
{
   return m_sLive;
}
void DllVolume::Set_Live(CString sLive)
{
   m_sLive = sLive;
}
//                               sConspec
CString DllVolume::Get_Conspec()
{
   return m_sConspec;
}
void DllVolume::Set_Conspec(CString sConspec)
{
   m_sConspec = sConspec;
}
//                           iHtref
int DllVolume::Get_Htref()
{
   return m_iHtref;
}
void DllVolume::Set_Htref(int iHtref)
{
   m_iHtref = iHtref;
}
//                             iFclass
int DllVolume::Get_Fclass()
{
   return m_iFclass;
}
void DllVolume::Set_Fclass(int iFclass)
{
   m_iFclass = iFclass;
}
//                             iHttfll
int DllVolume::Get_Httfll()
{
   return m_iHttfll;
}
void DllVolume::Set_Httfll(int iHttfll)
{
   m_iHttfll = iHttfll;
}
//                           iHtlog
int DllVolume::Get_Htlog()
{
   return m_iHtlog;
}
void DllVolume::Set_Htlog(int iHtlog)
{
   m_iHtlog = iHtlog;
}
//                             iCutflg
int DllVolume::Get_Cutflg()
{
   return m_iCutflg;
}
void DllVolume::Set_Cutflg(int iCutflg)
{
   m_iCutflg = iCutflg;
}
//                             iBfpflg
int DllVolume::Get_Bfpflg()
{
   return m_iBfpflg;
}
void DllVolume::Set_Bfpflg(int iBfpflg)
{
   m_iBfpflg = iBfpflg;
}
//                             iCupflg
int DllVolume::Get_Cupflg()
{
   return m_iCupflg;
}
void DllVolume::Set_Cupflg(int iCupflg)
{
   m_iCupflg = iCupflg;
}
//                             iCdpflg
int DllVolume::Get_Cdpflg()
{
   return m_iCdpflg;
}
void DllVolume::Set_Cdpflg(int iCdpflg)
{
   m_iCdpflg = iCdpflg;
}
//                             iCusflg
int DllVolume::Get_Cusflg()
{
   return m_iCusflg;
}
void DllVolume::Set_Cusflg(int iCusflg)
{
   m_iCusflg = iCusflg;
}
//                             iCdsflg
int DllVolume::Get_Cdsflg()
{
   return m_iCdsflg;
}
void DllVolume::Set_Cdsflg(int iCdsflg)
{
   m_iCdsflg = iCdsflg;
}
//                             iCdsflg
int DllVolume::Get_Spflg()
{
   return m_iSpflg;
}
void DllVolume::Set_Spflg(int iSpflg)
{
   m_iSpflg = iSpflg;
}

//                           iTlogs
int DllVolume::Get_Tlogs()
{
   return m_iTlogs;
}
void DllVolume::Set_Tlogs(int iTlogs)
{
   m_iTlogs = iTlogs;
}
//                               iErrflag
int DllVolume::Get_Errflag()
{
// 1 = No volume equation match
// 2 = No form class
// 3 = DBH less than one
// 4 = Tree height less than 4.5
// 5 = D2H is out of bounds
// 6 = No species match
// 7 = Illegal primary product log height (Ht1prd)
// 8 = Illegal secondary product log height (Ht2prd)
// 9 = Upper stem measurements required
   return m_iErrflag;
}
void DllVolume::Set_Errflag(int iErrflag)
{
   m_iErrflag = iErrflag;
}
//                               fDbhob
float DllVolume::Get_Dbhob()
{
   return m_fDbhob;
}
void DllVolume::Set_Dbhob(float fDbhob)
{
   m_fDbhob = fDbhob;
}
//                                fDrcob
float DllVolume::Get_Drcob()
{
   return m_fDrcob;
}
void DllVolume::Set_Drcob(float fDrcob)
{
   m_fDrcob =  fDrcob;
}
//                               fHttot
float DllVolume::Get_Httot()
{
   return m_fHttot;
}
void DllVolume::Set_Httot(float fHttot)
{
   m_fHttot = fHttot;
}
//                               fHt1prd
float DllVolume::Get_Ht1prd()
{
   return m_fHt1prd;
}
void DllVolume::Set_Ht1prd(float fHt1prd)
{
   m_fHt1prd = fHt1prd;
}
//                               fHt2prd
float DllVolume::Get_Ht2prd()
{
   return m_fHt2prd;
}
void DllVolume::Set_Ht2prd(float fHt2prd)
{
   m_fHt2prd = fHt2prd;
}
//                               fDbtbh
float DllVolume::Get_Dbtbh()
{
   return m_fDbtbh;
}
void DllVolume::Set_Dbtbh(float fDbtbh)
{
   m_fDbtbh = fDbtbh;
}
//                               fBtr
float DllVolume::Get_Btr()
{
   return m_fBtr;
}
void DllVolume::Set_Btr(float fBtr)
{
   m_fBtr = fBtr;
}
//                               fUpsht1
float DllVolume::Get_Upsht1()
{
   return m_fUpsht1;
}
void DllVolume::Set_Upsht1(float fUpsht1)
{
   m_fUpsht1 = fUpsht1;
}
//                               fUpsht2
float DllVolume::Get_Upsht2()
{
   return m_fUpsht2;
}
void DllVolume::Set_Upsht2(float fUpsht2)
{
   m_fUpsht2 = fUpsht2;
}
//                               fUpsd1
float DllVolume::Get_Upsd1()
{
   return m_fUpsd1;
}
void DllVolume::Set_Upsd1(float fUpsd1)
{
   m_fUpsd1 = fUpsd1;
}
//                               fUpsd2
float DllVolume::Get_Upsd2()
{
   return m_fUpsd2;
}
void DllVolume::Set_Upsd2(float fUpsd2)
{
   m_fUpsd2 = fUpsd2;
}
//                               fAvgz1
float DllVolume::Get_Avgz1()
{
   return m_fAvgz1;
}
void DllVolume::Set_Avgz1(float fAvgz1)
{
   m_fAvgz1 = fAvgz1;
}
//                               fAvgz2
float DllVolume::Get_Avgz2()
{
   return m_fAvgz2;
}
void DllVolume::Set_Avgz2(float fAvgz2)
{
   m_fAvgz2 = fAvgz2;
}
//                               fMtopp
float DllVolume::Get_Mtopp()
{
   return m_fMtopp;
}
void DllVolume::Set_Mtopp(float fMtopp)
{
   m_fMtopp = fMtopp;
}
//                               fMtops
float DllVolume::Get_Mtops()
{
   return m_fMtops;
}
void DllVolume::Set_Mtops(float fMtops)
{
   m_fMtops = fMtops;
}
//                               fStump
float DllVolume::Get_Stump()
{
   return m_fStump;
}
void DllVolume::Set_Stump(float fStump)
{
   m_fStump = fStump;
}
//                               fNologp
float DllVolume::Get_Nologp()
{
   return m_fNologp;
}
void DllVolume::Set_Nologp(float fNologp)
{
   m_fNologp = fNologp;
}
//                               fNologs
float DllVolume::Get_Nologs()
{
   return m_fNologs;
}
void DllVolume::Set_Nologs(float fNologs)
{
   m_fNologs = fNologs;
}
//                               Vol
float DllVolume::Get_Vol(int i)
{
// 0 = total cubic
// 1 = gross scribner boards
// 2 = net scribner boards
// 3 = gross cubic
// 4 = net cubic
// 5 = gross cordwood
// 6 = Gross secondary product volume in cubic feet
// 7 = Net secondary product volume in cubic feet
// 8 = Secondary product in cordwood
// 9 = Gross International ¼ board foot volume
// 10 = Net International ¼ board foot volume

   return m_fVol[i];
}
void DllVolume::Set_Vol(float fVol,int i)
{
   m_fVol[i] = fVol;
}
//                               fLoglen
float DllVolume::Get_Loglen(int i)
{
// i = log number 
   return m_fLoglen[i];
}
void DllVolume::Set_Loglen(float fLoglen,int i)
{
   m_fLoglen[i] = fLoglen;
}
//                               fLogvol_Bdft
float DllVolume::Get_Logvol_Bdft(int i)
{
// i = log number 
   return m_fLogvol_Bdft[i];
}
void DllVolume::Set_Logvol_Bdft(float fLogvol_Bdft,int i)
{
   m_fLogvol_Bdft[i] = fLogvol_Bdft;
}
//                               fLogvol_Cuft
float DllVolume::Get_Logvol_Cuft(int i)
{
// i = log number 
   return m_fLogvol_Cuft[i];
}
void DllVolume::Set_Logvol_Cuft(float fLogvol_Cuft,int i)
{
   m_fLogvol_Cuft[i] = fLogvol_Cuft;
}
//                               fLogsc
float DllVolume::Get_Logsc(int i)
{
// i = large end diameter for log number i, small diameter for log number i-1
   return m_fLogsc[i];
}
void DllVolume::Set_Logsc(float fLogsc,int i)
{
   m_fLogsc[i] = fLogsc;
}
//                               fLogdib
float DllVolume::Get_Logdib(int i)
{
// i = large end diameter for log number i, small diameter for log number i-1
   return m_fLogdib[i];
}
void DllVolume::Set_Logdib(float fLogdib,int i)
{
   m_fLogdib[i] = fLogdib;
}
//                               fLogdob
float DllVolume::Get_Logdob(int i)
{
// i = large end diameter for log number i, small diameter for log number i-1
   return m_fLogdob[i];
}
void DllVolume::Set_Logdob(float fLogdob,int i)
{
   m_fLogdob[i] = fLogdob;
}
//                               fBolht
float DllVolume::GetBolhtl(int i)
{
// i = log number 
   return m_fBolht[i];
}
void DllVolume::Set_Bolht(float fBolht,int i)
{
   m_fBolht[i] = fBolht;
}

//                           iFiaCode
int DllVolume::Get_Spcode()
{
   return m_iSpCode;
}
void DllVolume::Set_Spcode(int iSpCode)
{
   m_iSpCode = iSpCode;
}
