// nbel_cpp_tester.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "NBEL_CPP_DAL.h"
#include <windows.h>
//#include <stdio.h>
#include <atlstr.h>
//#include <string.h>
#include <iostream>
#include <vector>
using namespace std;


int _tmain(int argc, _TCHAR* argv[])
{
	try
	{
		//test the GetCitation method	
		CString getC = NBEL_CPP_DAL::GetCitation("6");
		wcout<<"\ncitation = "<<(LPCTSTR)getC<<"\n\n";

		//test the GetLocation method	
		CString getL = NBEL_CPP_DAL::GetLocation("6");
		wcout<<"\nloctation = "<<(LPCTSTR)getL<<"\n\n";
	
		//test the GetSppCode method
		int test;		
		test = NBEL_CPP_DAL::GetSppCode("balsam fir");
		cout<<"\nspecies code = "<<test<<"\n\n";

		//test the GetSpeciesByRegion method
		vector<CString> species = NBEL_CPP_DAL::GetSpeciesByRegion("6");
		for(int i = 0; i < species.size(); i++)
		{
			wcout<<"species = "<<(LPCTSTR)species[i]<<"\n";
		}

		//test IsValidEqNum method for a valid num and an invalid num
		bool valid = NBEL_CPP_DAL::IsValidEqnNum("19");
		cout<<"\n19 is a valid eq num? "<<valid<<"\n";
		valid = NBEL_CPP_DAL::IsValidEqnNum("19879");
		cout<<"19879 is a valid eq num? "<<valid<<"\n\n";

		//test the GetEqnNumbersbyInputs method
		vector<CString> eqnNums = NBEL_CPP_DAL::GetEqnNumbersByInputs(17, 35);
		for(int i = 0; i < eqnNums.size(); i++)
		{
			wcout<<"equation number = "<<(LPCTSTR)eqnNums[i]<<"\n";
		}

		//test and the SolveEquation method
		double solution = NBEL_CPP_DAL::SolveEquation("9", 1.5, 10.0, 0.0);
		cout<<"\nsolved equation #9, result = "<<solution << "\n";
		
	}
	catch (exception e)
	{
		cout<<"\nerror accessing biomass database\n";
	}

	return 0;
}

