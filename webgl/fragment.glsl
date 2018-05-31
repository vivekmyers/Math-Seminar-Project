#define count 50
#define m0 mat2(vec2(0.0, 0.0), vec2(0.0, 0.0))
#define i mat2(vec2(1.0, 0.0), vec2(0.0, 1.0))
#define j mat2(vec2(0.0, 1.0), vec2(-1.0, 0.0))
#define MAX_ITER 256

precision highp float;

uniform float coefs[count];
uniform float seed;
varying float x;
varying float y;

float abs(mat2 x) {
    return x[0][0] * x[0][0] + x[0][1] * x[0][1];
}

mat2 poly(mat2 x) {
    mat2 result = m0;
    for (int c = 0; c < count; c++) {
        result *= x;
        result += coefs[c] * i;
    }
    return result;
}

mat2 diff(mat2 x) {
    mat2 result = m0;
    for (int c = 1; c < count; c++) {
        result *= x;
        result += float(count - c) * coefs[c - 1] * i;
    }
    return result;
}

mat2 inverse(mat2 x) {
    float det = x[0][0] * x[1][1] - x[0][1] * x[1][0];
    if (det == 0.0) det = 1e-15;
    return mat2(vec2(x[1][1] / det, -x[0][1] / det), vec2(-x[1][0] / det, x[0][0] / det));
}

float shade(float x) {
    float contrast = 10.0;
    return (exp(contrast * x) - 1.0) / (exp(contrast) - 1.0);
}

vec3 hash(mat2 root) {
    float a = float(int(root[0][0] * 1e10)) / 1e10;
    float b = float(int(root[0][1] * 1e10)) / 1e10;
    float alt = 0.9 + 0.1 * seed;
    float rand = a * 1.0 / alt + b * exp(1.0 / alt);
    return vec3(mod(547.0 * rand, 1.0), mod(103.0 * rand, 1.0), mod(709.0 * rand, 1.0));
}

void main(void) {
    mat2 num;
    num[0] = vec2(x, y);
    num[1] = vec2(-y, x);
    mat2 current = num + 2.0 * i;
    int itr = MAX_ITER;
    for (int c = 0; c < MAX_ITER; c++) {
        if (abs(num - current) < 1e-10)
            break;
        itr--;
        mat2 dx = poly(num) * inverse(diff(num));
        current = num;
        num -= dx;
    }
    gl_FragColor = vec4(hash(num) * shade(float(itr) / float(MAX_ITER)) * (itr == 0 ? 0.0 : 1.0), 1.0);
}

