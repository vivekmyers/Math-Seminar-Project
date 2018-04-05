#include <iostream>
#include <string>
#include <cstring>
#include <vector>
#include <complex>
#include <utility>
#include <cmath>
#include <cmath>
#include <map>
#include <cstdlib>
#include <getopt.h>
#include <thread>
#include "fractals.h"

using namespace std;

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
    polynomial eq(coef);
    auto der = eq.dif();
    vector<vector<pair<complex<double>, int> > > grid(2 * n + 1);
    thread t1([eq, der, &grid, n, it, z]() {
        for (double i = -n; i <= 0; i++) {
            vector<pair<complex<double>, int> > row(2 * n + 1);
            for (double j = -n; j < n; j++) {
                complex<double> num(i / z, j / z);
                auto v = solve(num, eq, der, it);
                double precision = 1e10;
                row[j + n] = (make_pair(complex<double>(round(v.first.real() * precision) / precision, round(v.first.imag() * precision) / precision), v.second));
            }
            grid[i + n] = row;
        }
    });
    thread t2([eq, der, &grid, n, it, z]() {
        for (double i = 0; i <= n; i++) {
            vector<pair<complex<double>, int> > row(2 * n + 1);
            for (double j = -n; j <= n; j++) {
                complex<double> num(i / z, j / z);
                auto v = solve(num, eq, der, it);
                double precision = 1e10;
                row[j + n] = (make_pair(complex<double>(round(v.first.real() * precision) / precision, round(v.first.imag() * precision) / precision), v.second));
            }
            grid[i + n] = row;
        }
    });
    t1.join();
    t2.join();
    image(grid, it, c);
    return 0;
}

void image(vector<vector<pair<complex<double>, int> > > grid, int it, double cont) {
    FILE *f;
    srand(time(NULL));
    struct color {
        int r;
        int g;
        int b;
        static color random() {
            color r = { rand() % 256, rand() % 256, rand() % 256 };
            return r;
        }
    };
    unsigned char *img = NULL;
    int w = (int)grid.size();
    int h = (int)grid[0].size();
    int filesize = 54 + 3 * w * h;
    map<pair<double, double>, color> colors;
    img = new unsigned char[3 * w * h];
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
            int r = c.r * pow(shade / ((double)it), cont);
            int g = c.g * pow(shade / ((double)it), cont);
            int b = c.b * pow(shade / ((double)it), cont);
            img[(i + j * w) * 3 + 2] = (unsigned char)(r);
            img[(i + j * w) * 3 + 1] = (unsigned char)(g);
            img[(i + j * w) * 3 + 0] = (unsigned char)(b);
        }
    }
    
    unsigned char bmpfileheader[14] = { 'B', 'M', 0, 0, 0, 0, 0, 0, 0, 0, 54, 0, 0, 0 };
    unsigned char bmpinfoheader[40] = { 40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 24, 0 };
    unsigned char bmppad[3] = { 0, 0, 0 };
    
    bmpfileheader[2] = (unsigned char)(filesize);
    bmpfileheader[3] = (unsigned char)(filesize >> 8);
    bmpfileheader[4] = (unsigned char)(filesize >> 16);
    bmpfileheader[5] = (unsigned char)(filesize >> 24);
    
    bmpinfoheader[4] = (unsigned char)(w);
    bmpinfoheader[5] = (unsigned char)(w >> 8);
    bmpinfoheader[6] = (unsigned char)(w >> 16);
    bmpinfoheader[7] = (unsigned char)(w >> 24);
    bmpinfoheader[8] = (unsigned char)(h);
    bmpinfoheader[9] = (unsigned char)(h >> 8);
    bmpinfoheader[10] = (unsigned char)(h >> 16);
    bmpinfoheader[11] = (unsigned char)(h >> 24);
    
    f = fopen("output.bmp", "wb");
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
