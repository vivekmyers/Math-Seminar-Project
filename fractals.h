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

void image(pair<complex<double>, int> **grid, int it, double c, int w, int h);

#endif
