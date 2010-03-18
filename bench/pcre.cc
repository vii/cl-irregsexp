#include <fstream>
#include <iostream>
#include <cassert>
#include <pcre++.h>
#include <boost/timer.hpp>
using namespace std;
using namespace pcrepp;

string slurp_file(const char*name)
{
  ifstream f(name,ios::binary );

  f.seekg (0, ios::end);
  unsigned long length = f.tellg();
  f.seekg (0, ios::beg);
  
  char*buf=new char[length];
  f.read(buf,length);
  string s = buf;
  delete buf;
  return s;
}

inline
unsigned find_it(Pcre&re,const string&buf)
{
  re.search(buf);
  return re.get_match_start(0);
}

int main ()
{
  string buf=slurp_file("test-data");
  Pcre re("(indecipherable|undecipherable)");
  re.study();
  unsigned len = find_it(re,buf);
  boost::timer start;
  for(int i=1000;i;--i)
    assert(find_it(re,buf)==len);
  cout << len << " " << start.elapsed() << endl;
  return 0;
}
