#include <iostream>
#include <string>
#include <cstring>
#include <vector>
#include <complex>
#include <utility>
#include <cmath>
#include <map>
#include <cstdlib>
#include <getopt.h>
#include <thread>
#include "fractals.h"

using namespace std;

/**
    Prompts for coefficients in polynomial. Flags can be provided to specify extra parameters.
*/
int main(int argc, char *const *argv) {
    string in;
    cout << "Coefficients: ";
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
    int n = 600;
    double z = 1;
    int it = 128;
    double c = 2;
    int ch;
    while ((ch = getopt(argc, argv, ":gzic")) != -1) {
        switch (ch) {
            case 'g':
                cout << "Grid Size: ";
                cin >> n;
                break;
            case 'z':
                cout << "Zoom Factor: ";
                cin >> z;
                break;
            case 'i':
                cout << "Iterations: ";
                cin >> it;
                break;
            case 'c':
                cout << "Contrast: ";
                cin >> c;
                break;
        }
    }
    z = n * z / 2;
    int dim = 2 * n + 1;
    polynomial eq(coef);
    auto der = eq.dif();
    auto grid = new pair<complex<double>, int>*[dim];
    for (int i = 0; i < dim; i++) {
        grid[i] = new pair<complex<double>, int>[dim];
    }
    thread t1([eq, der, &grid, n, it, z]() {
        for (int i = -n; i <= 0; i++) {
            for (int j = -n; j < n; j++) {
                complex<double> num(i / z, j / z);
                auto v = solve(num, eq, der, it);
                double precision = 1e10;
                grid[i + n][j + n] = (make_pair(complex<double>(round(v.first.real() * precision) / precision, round(v.first.imag() * precision) / precision), v.second));
            }
        }
    });
    thread t2([eq, der, &grid, n, it, z]() {
        for (int i = 0; i <= n; i++) {
            for (int j = -n; j <= n; j++) {
                complex<double> num(i / z, j / z);
                auto v = solve(num, eq, der, it);
                double precision = 1e10;
                grid[i + n][j + n] = (make_pair(complex<double>(round(v.first.real() * precision) / precision, round(v.first.imag() * precision) / precision), v.second));
            }
        }
    });
    t1.join();
    t2.join();
    image(grid, it, c, dim, dim);
    for (int i = 0; i < dim; i++) {
        delete[] grid[i];
    }
    delete[] grid;
    cout << "Done" << endl;
    return 0;
}

void image(pair<complex<double>, int> **grid, int it, double cont, int w, int h) {
    typedef unsigned char byte;
    srand(time(NULL));
    struct color {
        byte r;
        byte g;
        byte b;
        static color random() {
            color r = { (byte) (rand() % 256), (byte) (rand() % 256), (byte) (rand() % 256) };
            return r;
        }
    };
    byte *img = new byte[3 * w * h];
    int filesize = 54 + 3 * w * h;
    map<pair<double, double>, color> colors;
    memset(img, 0, 3 * w * h);
    
    for (int i = 0; i < w; i++) {
        for (int j = 0; j < h; j++) {
            auto ent = grid[i][j];
            pair<double, double> val = make_pair(ent.first.real(), ent.first.imag());
            double shade = ent.second;
            color c;
            if (colors.find(val) != colors.end()) {
                c = colors[val];
            }
            else {
                c = color::random();
                colors[val] = c;
            }
            img[(i + j * w) * 3 + 2] = c.r * pow(shade / ((double)it), cont);
            img[(i + j * w) * 3 + 1] = c.g * pow(shade / ((double)it), cont);
            img[(i + j * w) * 3 + 0] = c.b * pow(shade / ((double)it), cont);
        }
    }
    
    byte bmpfileheader[14] = { 'B', 'M', 0, 0, 0, 0, 0, 0, 0, 0, 54, 0, 0, 0 };
    byte bmpinfoheader[40] = { 40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 24, 0 };
    byte bmppad[3] = { 0, 0, 0 };
    
    bmpfileheader[2] = (byte)(filesize);
    bmpfileheader[3] = (byte)(filesize >> 8);
    bmpfileheader[4] = (byte)(filesize >> 16);
    bmpfileheader[5] = (byte)(filesize >> 24);
    
    bmpinfoheader[4] = (byte)(w);
    bmpinfoheader[5] = (byte)(w >> 8);
    bmpinfoheader[6] = (byte)(w >> 16);
    bmpinfoheader[7] = (byte)(w >> 24);
    bmpinfoheader[8] = (byte)(h);
    bmpinfoheader[9] = (byte)(h >> 8);
    bmpinfoheader[10] = (byte)(h >> 16);
    bmpinfoheader[11] = (byte)(h >> 24);
    
    FILE *f = fopen("output.bmp", "wb");
    fwrite(bmpfileheader, 1, 14, f);
    fwrite(bmpinfoheader, 1, 40, f);
    for (int i = 0; i < h; i++) {
        fwrite(img + (w * (h - i - 1) * 3), 3, w, f);
        fwrite(bmppad, 1, (4 - (w * 3) % 4) % 4, f);
    }
    
    delete[] img;
    fclose(f);
}

polynomial::polynomial(vector<double> &coef) {
    this->coef = coef;
}

const inline complex<double> polynomial::operator()(complex<double> &x) {
    complex<double> c(0, 0);
    for (auto &i : coef) {
        c = c * x + complex<double>(i, 0);
    }
    return c;
}

const polynomial polynomial::dif() {
    vector<double> end;
    double c = coef.size() - 1;
    for (auto i = coef.begin(); i < coef.end() - 1; i++) {
        end.push_back((*i) * c--);
    }
    return polynomial(end);
}

template <typename F>
pair<complex<double>, int> solve(complex<double> num, F eq, F der, int itr) {
    while (itr > 0) {
        itr--;
        auto dd = der(num);
        if (dd == complex<double>(0, 0)) {
            return make_pair(complex<double>(0, 0), 0);
        }
        auto dx = eq(num) / dd;
        num -= dx;
        if (abs(dx) < 1e-15) {
            return make_pair(num, itr);
        }
    }
    return make_pair(complex<double>(0, 0), 0);
}
