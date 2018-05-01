#ifndef fractals_h
#define fractals_h

using namespace std;

/**
    Polynomial function
*/
class polynomial {
    private:
        vector<double> coef;
    public:
        /**
            Polynomial from vector of coefficients.
        */
        polynomial(vector<double> &coef);
        /**
            Call polynomial.
        */
        const inline complex<double> operator()(complex<double> &x);
        /**
            Get derivative.
        */
        const polynomial dif();
};

/**
    Converges a single value.
*/
template <typename F>
pair<complex<double>, int> solve(complex<double> num, F eq, F der, int itr);

/**
    Generates an image from a grid of complex convergence values.
*/
void image(pair<complex<double>, int> **grid, int it, double c, int w, int h);

#endif
