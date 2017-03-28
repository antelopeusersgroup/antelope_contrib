#include <string>
using namespace std;
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "ensemble.h"
using namespace std;
using namespace SEISPP;
int main(int argc, char **argv)
{
  string testfile("test.dat");
  StreamObjectReader<ThreeComponentEnsemble> r1;
  StreamObjectWriter<ThreeComponentEnsemble> w1;
  StreamObjectReader<ThreeComponentEnsemble> r2(testfile);
}
