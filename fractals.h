#ifndef fractals_h
#define fractals_h

using namespace std;

class polynomial {
    private:
        vector<double> coef;
    public:
        polynomial(vector<double> &coef);
        const inline complex<double> operator()(complex<double> &x);
        const polynomial dif();
};

template <typename F>
pair<complex<double>, int> solve(complex<double> num, F eq, F der, int itr);

void image(vector<vector<pair<complex<double>, int> > >, int it, double c);

#endif
