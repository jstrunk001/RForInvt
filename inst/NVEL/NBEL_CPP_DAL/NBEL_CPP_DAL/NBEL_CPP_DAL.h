#pragma once
#pragma warning (disable: 4278)
#include <atlstr.h>
#include <vector>

// To use managed-code servers like the C# server,
// we have to import the common language runtime:
#import <mscorlib.tlb> raw_interfaces_only

// For simplicity, we ignore the server namespace and use named guids:
#if defined (USINGPROJECTSYSTEM)
#import "..\NBEL.tlb" no_namespace named_guids
#else  // Compiling from the command line, all files in the same directory
#import "NBEL.tlb" no_namespace named_guids
#endif

class NBEL_CPP_DAL
{
public:
	NBEL_CPP_DAL(void);
	~NBEL_CPP_DAL(void);
	static __declspec(dllexport) std::vector <CString> GetSpeciesByRegion(CString);
	static __declspec(dllexport) std::vector <CString> GetEqnNumbersByInputs(int spp, int component);
	static __declspec(dllexport) bool IsValidEqnNum(CString eqnm);
	static __declspec(dllexport) int GetSppCode(CString species);
	static __declspec(dllexport) CString GetCitation(CString eqnNum);
	static __declspec(dllexport) CString GetLocation(CString eqnNum);
	static __declspec(dllexport) double SolveEquation(CString eqNum, double var1, double var2, double var3);
	
//	IManagedInterface *cpi;// = NULL;
public:
	 
};


