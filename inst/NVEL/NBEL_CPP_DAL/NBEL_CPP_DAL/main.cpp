#include "NBEL_CPP_DAL.h"
#include <vector>
int main(int argc, char* argv[])
{
	NBEL_CPP_DAL myNBELdb;
	//std::vector <CString> test;
	//if(myNBELdb.isValidEqnNum("6"))
	//	test = myNBELdb.GetSpeciesByRegion("6");
	
	int res = myNBELdb.GetSppCode("balsam fir");

	//CString s = myNBELdb.GetCitation("9");
	//CString inStr;
	//for(int i=0; i<test.size(); i++)
	//{
	//	inStr += test[i] + "\n";
	//}
	//MessageBox(NULL, inStr, NULL, NULL);
	return 1;
}