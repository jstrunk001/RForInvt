#include "NBEL_CPP_DAL.h"
#include <windows.h>
#include <stdio.h>
#include <atlstr.h>
#include <iostream>
using namespace std;


NBEL_CPP_DAL::NBEL_CPP_DAL(void)
{
	
}

vector<CString> NBEL_CPP_DAL::GetEqnNumbersByInputs(int spp, int component)
{
	vector<CString> vresult;
	HRESULT hresult;
	long index, lBound, uBound;

	//intialize the pointer to the managed interface
	IManagedInterface *cpi = NULL;
	CoInitialize(NULL);
    HRESULT hr = CoCreateInstance(CLSID_InterfaceImplementation,
                                  NULL,
                                  CLSCTX_INPROC_SERVER,
                                  IID_IManagedInterface,
                                  reinterpret_cast<void**>(&cpi));
	if (FAILED(hr))
    {
		MessageBox(NULL, (LPCTSTR)"Could not create the NBEL instance.", NULL, NULL);
        printf("Couldn't create the instance!... 0x%x\n", hr);
    }

	try
	{
		//this is the call to the NBEL
		SAFEARRAY* safearray;
		
		safearray = cpi->GetEqnNumbersbyInputs(spp, component);
		
		//unicode string to get values from array	
		wchar_t * fArray;

		//get the array bounds
		hresult = SafeArrayGetUBound(safearray, 1, &uBound);

		if(uBound >0)
		{
			hresult = SafeArrayGetLBound(safearray, 1, &lBound);		
			
			//cout<<"returned from GetEqnNumbersbyInputs \n";
			
			for (; lBound <= uBound; lBound++)
			{
				index = lBound;
				
				//get the values (in unicode, though)
				hresult = SafeArrayGetElement(safearray, &index, (void*)&fArray);

				CString s(fArray);
				vresult.push_back(s);
			}
		}
		else
		{
			MessageBox(NULL, (LPCTSTR)"Unable to get equation numbers by input from NBEL.", NULL,NULL);
		}	
	}
	catch(exception e)
	{
		MessageBox(NULL, (LPCTSTR)"Unable to get equation numbers by input from NBEL.", NULL,NULL);
	}
	// Be a good citizen and clean up COM:
    CoUninitialize();
	return vresult;
}
vector<CString> NBEL_CPP_DAL::GetSpeciesByRegion(CString region)
{
	//test the GetSpeciesByRegion method
	vector<CString> vresult;

	HRESULT hresult;
	long index, lBound, uBound;
	
	//intialize the pointer to the managed interface
	IManagedInterface *cpi = NULL;
	CoInitialize(NULL);
    HRESULT hr = CoCreateInstance(CLSID_InterfaceImplementation,
                                  NULL,
                                  CLSCTX_INPROC_SERVER,
                                  IID_IManagedInterface,
                                  reinterpret_cast<void**>(&cpi));
	if (FAILED(hr))
    {
		MessageBox(NULL, (LPCTSTR)"Could not create the NBEL instance.", NULL, NULL);
        printf("Couldn't create the instance!... 0x%x\n", hr);
    }

	//this is the call to the NBEL. we have to use safearray since we are
	//calling managed code
	SAFEARRAY* safearray = cpi->GetSpeciesByRegion(region.AllocSysString());
	
	//unicode string to get values from array	
	wchar_t * fArray;

	//get the upper and lower bounds of the array
	hresult = SafeArrayGetUBound(safearray, 1, &uBound);
	
    if(uBound >0)
	{
		hresult = SafeArrayGetLBound(safearray, 1, &lBound);		
		
		//loop thru array and get species list
		for (; lBound <= uBound; lBound++)
			{
				index = lBound;
				//get the values (in unicode, though)
				hresult = SafeArrayGetElement(safearray, &index, (void*)&fArray);

				//Convert from unicode to ansii;
				CString s = (LPCWSTR)fArray;
				
				//put results in a vector of CStrings
				vresult.push_back(s);
			}		
	}
	else
	{
		cout<<"\nno species for this region #\n";
	}	
	// Be a good citizen and clean up COM:
    CoUninitialize();
	return vresult;
}

bool NBEL_CPP_DAL::IsValidEqnNum(CString eqNum)
{    
	//intialize the pointer to the managed interface
	IManagedInterface *cpi = NULL;
	CoInitialize(NULL);
    HRESULT hr = CoCreateInstance(CLSID_InterfaceImplementation,
                                  NULL,
                                  CLSCTX_INPROC_SERVER,
                                  IID_IManagedInterface,
                                  reinterpret_cast<void**>(&cpi));
	if (FAILED(hr))
    {
		MessageBox(NULL, (LPCTSTR)"Could not create the NBEL instance.", NULL, NULL);
        printf("Couldn't create the instance!... 0x%x\n", hr);
    }

	//this is the call to the NBEL
    bool isValidEqNm = cpi->isValidEqnNum(eqNum.AllocSysString());
	
	// uncomment below if you need testing
	//if(cpi->isValidEqnNum(eqNum.AllocSysString()))
	//	MessageBox(NULL,CString("valid eqn num!"),NULL,NULL);
	//else
	//	MessageBox(NULL,(LPCTSTR) "invalid eqn num!",NULL,NULL);
	//
	// Be a good citizen and clean up COM:
    CoUninitialize();
	return isValidEqNm;

}

int NBEL_CPP_DAL::GetSppCode(CString species)
{
	IManagedInterface *cpi = NULL;
	CoInitialize(NULL);
    HRESULT hr = CoCreateInstance(CLSID_InterfaceImplementation,
                                  NULL,
                                  CLSCTX_INPROC_SERVER,
                                  IID_IManagedInterface,
                                  reinterpret_cast<void**>(&cpi));
	if (FAILED(hr))
    {
		MessageBox(NULL, (LPCTSTR)"Could not create the NBEL instance.", NULL, NULL);
        printf("Couldn't create the instance!... 0x%x\n", hr);
    }
	
	//this is the call to the NBEL
	int sppCode = cpi->GetSppCode(species.AllocSysString());
	
	// Be a good citizen and clean up COM:
    CoUninitialize();
	return sppCode;
}


CString NBEL_CPP_DAL::GetCitation(CString eqnNum)
{
	CString inStr;

	IManagedInterface *cpi = NULL;
	CoInitialize(NULL);
    HRESULT hr = CoCreateInstance(CLSID_InterfaceImplementation,
                                  NULL,
                                  CLSCTX_INPROC_SERVER,
                                  IID_IManagedInterface,
                                  reinterpret_cast<void**>(&cpi));
	if (FAILED(hr))
    {
		MessageBox(NULL, (LPCTSTR)"Could not create the NBEL instance.", NULL, NULL);
        printf("Couldn't create the instance!... 0x%x\n", hr);
    }
	try
	{
		//this is the call to the NBEL
		BSTR bs = cpi->GetCitation(eqnNum.AllocSysString());
		CString s(bs);
		inStr = s;

		cpi->Release();
        cpi = NULL;
	}
	catch (exception e)
	{
		MessageBox(NULL, (LPCTSTR)"Unable to get citation from NBEL.", NULL,NULL);
	}
	// Be a good citizen and clean up COM:
    CoUninitialize();
	return inStr;
}

CString NBEL_CPP_DAL::GetLocation(CString eqnNum)
{
	CString inStr;

	IManagedInterface *cpi = NULL;
	CoInitialize(NULL);
    HRESULT hr = CoCreateInstance(CLSID_InterfaceImplementation,
                                  NULL,
                                  CLSCTX_INPROC_SERVER,
                                  IID_IManagedInterface,
                                  reinterpret_cast<void**>(&cpi));
	if (FAILED(hr))
    {
		MessageBox(NULL, (LPCTSTR)"Could not create the NBEL instance.", NULL, NULL);
        printf("Couldn't create the instance!... 0x%x\n", hr);
    }
	try
	{
		//this is the call to the NBEL
		BSTR bs = cpi->GetLocation(eqnNum.AllocSysString());
		CString s(bs);
		inStr = s;
		
		cpi->Release();
        cpi = NULL;
	}
	catch (exception e)
	{
		MessageBox(NULL, (LPCTSTR)"Unable to get citation from NBEL.", NULL,NULL);
	}
	// Be a good citizen and clean up COM:
    CoUninitialize();
	return inStr;

}

double NBEL_CPP_DAL::SolveEquation(CString eqNum, double var1, double var2, double var3)
{
	double inDb = -1;

	IManagedInterface *cpi = NULL;
	CoInitialize(NULL);
    HRESULT hr = CoCreateInstance(CLSID_InterfaceImplementation,
                                  NULL,
                                  CLSCTX_INPROC_SERVER,
                                  IID_IManagedInterface,
                                  reinterpret_cast<void**>(&cpi));
	if (FAILED(hr))
    {
		MessageBox(NULL, (LPCTSTR)"Could not create the NBEL instance.", NULL, NULL);
        printf("Couldn't create the instance!... 0x%x\n", hr);
    }

	try
	{
		double solution = cpi->SolveEquation(eqNum.AllocSysString(), 1.5, 10.0, 0.0);
		inDb = solution;
	}
	catch (exception e)
	{
		MessageBox(NULL, (LPCTSTR)"Unable to solve equation from NBEL.", NULL,NULL);
	}
	// Be a good citizen and clean up COM:
    CoUninitialize();
	return inDb;
}


NBEL_CPP_DAL::~NBEL_CPP_DAL(void)
{
}
