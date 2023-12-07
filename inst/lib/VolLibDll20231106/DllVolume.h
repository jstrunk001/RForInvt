// DllVolume.h: interface for the DllVolume class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_DLLVOLUME_H__7B232070_8E04_11D4_B542_0006290DDAA5__INCLUDED_)
#define AFX_DLLVOLUME_H__7B232070_8E04_11D4_B542_0006290DDAA5__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "f77matrx.h"
#include "f77char.h"
// Define version number
extern "C" __declspec(dllimport) void _stdcall VERNUM(int *VERSION);

// Define volume equation number default routine
extern "C" __declspec(dllimport) void _stdcall GETVOLEQ(int *REGN,CHARACTER FORST,CHARACTER DIST,
                                      int *SPEC,CHARACTER PROD,CHARACTER VOLEQ,int *ERRFLAG);

// Define Fortran DLL calling parameters
// Added DIST to the call parameter 2017/02/27
extern "C" __declspec(dllimport) void _stdcall VOLUMELIBRARY(int *REGN,CHARACTER FORST,
         CHARACTER VOLEQ,float *MTOPP,float *MTOPS,float *STUMP,float *DBHOB,
         float *DRCOB,CHARACTER HTTYPE,float *HTTOT,int *HTLOG,float *HT1PRD,
         float *HT2PRD,float *UPSHT1,float *UPSHT2,float *UPSD1,float *UPSD2,
         int *HTREF,float *AVGZ1,float *AVGZ2,int *FCLASS,float *DBTBH,float *BTR,
         int& I3,int& I7,int& I15,int& I20,int& I21,float* VOL,float* LOGVOL,
         float* LOGDIA,float* LOGLEN,float* BOLHT, int *TLOGS,float *NOLOGP,
         float *NOLOGS,int *CUTFLG,int *BFPFLG,int *CUPFLG,int *CDPFLG,int *SPFLG,
         CHARACTER CONSPEC,CHARACTER PROD,int *HTTFLL,CHARACTER LIVE,
         int *BA,int *SI,CHARACTER CTYPE,int *ERRFLAG, int *DIST);

//The following SUBROUTINE does not exist in vollib.dll, so comment out 2017/02/27
// Define Fortran DLL calling parameters
//extern "C" __declspec(dllimport) void _stdcall VOLUMELIB(int *REGN,CHARACTER FORST,
//         CHARACTER VOLEQ,float *MTOPP,float *MTOPS,float *STUMP,float *DBHOB,
//         float *DRCOB,CHARACTER HTTYPE,float *HTTOT,int *HTLOG,float *HT1PRD,
//         float *HT2PRD,float *UPSHT1,float *UPSHT2,float *UPSD1,float *UPSD2,
//         int *HTREF,float *AVGZ1,float *AVGZ2,int *FCLASS,float *DBTBH,float *BTR,
//         int& I3,int& I7,int& I11,int& I20,int& I21,float* VOL,float* LOGVOL,
//         float* LOGDIA,float* LOGLEN,float* BOLHT, int *TLOGS,float *NOLOGP,
//         float *NOLOGS,int *CUTFLG,int *BFPFLG,int *CUPFLG,int *CDPFLG,int *CUSFLG,
//         int *CDSFLG,CHARACTER CONSPEC,CHARACTER PROD,int *HTTFLL,CHARACTER LIVE,
//         int *BA,int *SI,CHARACTER CTYPE,int *ERRFLAG);

// Define Fortran DLL calling parameters
//extern "C" __declspec(dllimport) void _stdcall VOLLIB(int *REGN,CHARACTER FORST,
//         CHARACTER VOLEQ,float *MTOPP,float *MTOPS,float *STUMP,float *DBHOB,
//         float *DRCOB,CHARACTER HTTYPE,float *HTTOT,int *HTLOG,float *HT1PRD,
//         float *HT2PRD,float *UPSHT1,float *UPSHT2,float *UPSD1,float *UPSD2,
//         int *HTREF,float *AVGZ1,float *AVGZ2,int *FCLASS,float *DBTBH,float *BTR,
//         int& I3,int& I7,int& I11,int& I20,int& I21,float* VOL,float* LOGVOL,
//         float* LOGDIA,float* LOGLEN,float* BOLHT, int *TLOGS,float *NOLOGP,
//         float *NOLOGS,int *CUTFLG,int *BFPFLG,int *CUPFLG,int *CDPFLG,int *CUSFLG,
//         int *CDSFLG,CHARACTER CONSPEC,CHARACTER PROD,int *HTTFLL,CHARACTER LIVE,
//         int *ERRFLAG);

class DllVolume  
{
public:
   CString m_sVoleq;    // Volume equation number
   CString m_sForst;    // Forest number
   CString m_sDist;     // District number
   CString m_sProd;     // Product code
   CString m_sHttype;   // Height type (F = feet, L = logs)
   CString m_sLive;     // Live code (L = live, D = dead)
   CString m_sConspec;  // Contract Species
   int m_iHtref;        // Reference Height
   int m_iFclass;       // Form Class
   int m_iHttfll;       // Height to first live limb
   int m_iRegn;         // Region number
   int m_iHtlog;        // Length of logs used if Httype = L
   int m_iTlogs;        // Number of predicted logs in the tree
   int m_iErrflag;      // Error flag
   int m_iCutflg;       // Total cubic foot volume flag
   int m_iBfpflg;       // Board foot volume flag
   int m_iCupflg;       // Merch cubic volume flag
   int m_iCdpflg;       // Merch cord wood volume flag
   int m_iCusflg;       // Cubic secondary product volume flag
   int m_iCdsflg;       // Cord wood secondary product volume flag
   int m_iSpCode;       // Species - 3 digit FIA code (ie: 122 = Ponderosa Pine)
   int m_iSpflg;        // Secondary product volume flag
   int m_iDist          // district number
   float m_fDbhob;      // Dbh outside bark
   float m_fDrcob;      // Diameter at root collar
   float m_fHttot;      // Total tree height in feet
   float m_fHt1prd;     // Height to primary product minimum top diameter (feet or logs)
   float m_fHt2prd;     // Height to secondary product minimum top diameter (feet or logs)
   float m_fDbtbh;      // Double bark thickness at breast height
   float m_fBtr;        // Bark thickness ratio
   float m_fUpsht1;     // Upper stem height
   float m_fUpsht2;     // Second Upper stem height
   float m_fUpsd1;      // Upper stem diameter taken at Upper stem height
   float m_fUpsd2;       // Second Upper stem diameter taken at Second Upper stem height
   float m_fAvgz1;      // Average Z-score:  applied at Upsht1 or Refht
   float m_fAvgz2;      // Second Average Z-score: applied at Usht2
   float m_fMtopp;      // Merchantable top diameter for primary product. inside bark
   float m_fMtops;      // Merchantable top diameter for secondary product, inside bark
   float m_fStump;      // Stump height
   float m_fNologp;     // Number of 16 foot logs for primary product
   float m_fNologs;     // Number of 16 foot logs for secondary product

   float m_fVol[15];    // Tree volumes
   float m_fLoglen[20];       // Individual log lengths
   float m_fLogvol_Bdft[20];  // Individual log board foot volumes
   float m_fLogvol_Cuft[20];  // Individual log cubic foot volumes
   float m_fLogsc[21];        // Individual log scaling diameters (rounded to nearest inch)
   float m_fLogdib[21];       // Individual log diameters inside bark
   float m_fLogdob[21];       // Individual log diameters outside bark
   float m_fBolht[21];        // Height up stem where each log diameter was predicted
   int m_nVersionNum;
public:
   DllVolume();
   virtual ~DllVolume();
   void Get_DllVolume();
   
   CString Get_Voleq();
   void Set_Voleq(CString sVoleq);
   CString Get_Forst();
   void Set_Forst(CString sForst);
   CString Get_Dist();
   void Set_Dist(CString sDist);
   CString Get_Prod();
   void Set_Prod(CString sProd);
   CString Get_Httype();
   void Set_Httype(CString sHttype);
   CString Get_Live();
   void Set_Live(CString sLive);
   CString Get_Conspec();
   void Set_Conspec(CString sConspec);
   int Get_Htref();
   void Set_Htref(int iHtref);
   int Get_Fclass();
   void Set_Fclass(int iFclass);
   int Get_Httfll();
   void Set_Httfll(int iHttfll);
   int Get_Regn();
   void Set_Regn(int iRegn);
   int Get_Htlog();
   void Set_Htlog(int iHtlog);
   int Get_Cutflg();
   void Set_Cutflg(int iCutflg);
   int Get_Bfpflg();
   void Set_Bfpflg(int iBfpflg);
   int Get_Cupflg();
   void Set_Cupflg(int iCupflg);
   int Get_Cdpflg();
   void Set_Cdpflg(int iCdpflg);
   int Get_Cusflg();
   void Set_Cusflg(int iCusflg);
   int Get_Cdsflg();
   void Set_Cdsflg(int iCdsflg);
   int Get_Spflg();
   void Set_Spflg(int iSpflg);
   int Get_Tlogs();
   void Set_Tlogs(int iTlogs);
   int Get_Errflag();
   void Set_Errflag(int iErrflag);
   float Get_Dbhob();
   void Set_Dbhob(float fDbhob);
   float Get_Drcob();
   void Set_Drcob(float fDrcob);
   float Get_Httot();
   void Set_Httot(float fHttot);
   float Get_Ht1prd();
   void Set_Ht1prd(float fHt1prd);
   float Get_Ht2prd();
   void Set_Ht2prd(float fHt2prd);
   float Get_Dbtbh();
   void Set_Dbtbh(float fDbtbh);
   float Get_Btr();
   void Set_Btr(float fBtr);
   float Get_Upsht1();
   void Set_Upsht1(float fUpsht1);
   float Get_Upsht2();
   void Set_Upsht2(float fUpsht2);
   float Get_Upsd1();
   void Set_Upsd1(float fUpsd1);
   float Get_Upsd2();
   void Set_Upsd2(float fUpsd2);
   float Get_Avgz1();
   void Set_Avgz1(float fAvgz1);
   float Get_Avgz2();
   void Set_Avgz2(float fAvgz2);
   float Get_Mtopp();
   void Set_Mtopp(float fMtopp);
   float Get_Mtops();
   void Set_Mtops(float fMtops);
   float Get_Stump();
   void Set_Stump(float fStump);
   float Get_Nologp();
   void Set_Nologp(float fNologp);
   float Get_Nologs();
   void Set_Nologs(float fNologs);
   float Get_Vol(int i);
   void Set_Vol(float fVol,int i);
   float Get_Loglen(int i);
   void Set_Loglen(float fLoglen,int i);
   float Get_Logvol_Bdft(int i);
   void Set_Logvol_Bdft(float fLogvol_Bdft,int i);
   float Get_Logvol_Cuft(int i);
   void Set_Logvol_Cuft(float fLogvol_Cuft,int i);
   float Get_Logsc(int i);
   void Set_Logsc(float fLogsc,int i);
   float Get_Logdib(int i);
   void Set_Logdib(float fLogdib,int i);
   float Get_Logdob(int i);
   void Set_Logdob(float fLogdob,int i);
   float GetBolhtl(int i);
   void Set_Bolht(float fBolht,int i);
   int Get_Spcode();
   void Set_Spcode(int iSpCode);
};


#endif // !defined(AFX_DLLVOLUME_H__7B232070_8E04_11D4_B542_0006290DDAA5__INCLUDED_)



