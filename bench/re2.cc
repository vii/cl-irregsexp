#include <iostream>
#include <fstream>
#include <cassert>
#include <re2/re2.h>
#include <boost/timer.hpp>
using namespace std;

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
unsigned find_it(RE2::RE2&re,const string&buf)
{
  re2::StringPiece sp(buf);
  RE2::FindAndConsume(&sp,re);
  return sp.length();
}

int main ()
{
  string buf=slurp_file("test-data");
  RE2::RE2 re("(indecipherable|undecipherable)");
  unsigned len = find_it(re,buf);
  boost::timer start;
  for(int i=1000;i;--i)
    assert(len == find_it(re,buf));
  cout << (buf.length() - len) << " " << start.elapsed() << endl;
  return 0;
}
