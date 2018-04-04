#ifndef fractals_h
#define fractals_h

using namespace std;
typedef function<complex<double>(complex<double>&)> cfunc;

cfunc poly(const vector<double> &coef);
cfunc dif(const vector<double> &coef);
pair<complex<double>, int> solve(complex<double> num, cfunc eq, cfunc der, int itr);

#endif
