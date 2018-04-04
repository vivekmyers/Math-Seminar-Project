#include <iostream>
#include <string>
#include <string.h>
#include <vector>
#include <complex>
#include <utility>
#include "fractals.h"

using namespace std;

int main(int argc, const char * argv[]) {
    string in;
    getline(cin, in);
    vector<double> coef;
    const char *sep = " ,";
    char ins[in.length()];
    strcpy(ins, in.c_str());
    char *next = strtok(ins, sep);
    while (next != nullptr) {
        coef.push_back(atof(next));
        next = strtok(nullptr, sep);
    }
    int n;
    double z;
    int it;
    double c;
    cin >> n;
    cin >> z;
    cin >> it;
    cin >> c;
    cfunc eq = poly(coef);
    cfunc der = dif(coef);
    for (double i = -n; i <= n; i ++) {
        for (double j = -n; j <= n; i ++) {
            complex<double> num(i / z, j / z);
            auto v = solve(num, eq, der, it);
            cout << "(" << v.first << "," << v.second << ")" << " ";
        }
        cout << endl;
    }
    return 0;
}

pair<complex<double>, int> solve(complex<double> num, cfunc eq, cfunc der, int itr) {
    while (itr > 0 && itr--) {
        auto dd = der(num);
        if (dd == complex<double>(0,0)) {
            auto r = make_pair(complex<double>(0, 0), 0);
            return r;
        }
        auto dx = eq(num) / dd;
        num -= dx;
        if (abs(num) < 1e-15) {
            auto r = make_pair(num, itr);
            return r;
        }
    }
    auto r = make_pair(complex<double>(0, 0), 0);
    return r;
}

cfunc poly(const vector<double> &coef) {
    return [coef] (complex<double> &x) {
        auto c = complex<double>(0, 0);
        for (auto i = coef.begin(); i < coef.end(); i++) {
            c = c * x + complex<double>(*i, 0);
        }
        return c;
    };
}

cfunc dif (const vector<double> &coef) {
    vector<double> end;
    double c = coef.size() - 1;
    for (auto i = coef.begin(); i < coef.end() - 1; i++) {
        end.push_back((*i) * c--);
    }
    return poly(end);
}


