scale = 2;
size = 800;
seedval = Math.random();
centerx = 0;
centery = 0;
// algorithm = prompt('Algorithm:')
algorithm = 'n'
numerical = false;
// numerical = prompt('Numerical?:') == 'y';
iterations = 0;
// iterations = prompt('Iterations:');
requation = [1,0,0,-1];
iequation = [0,0,0,0];
start = [1,0];
pi_s = '3.1415926535897932384626433832795'

window.onload = function () {
    const options_conv = document.getElementById('options-conv');
    const options_conv2 = document.getElementById('options-conv2');
    const options_cw = document.getElementById('options-cw');
    const options_iter = document.getElementById('options-iter');
    const options_iter2 = document.getElementById('options-iter2');

    const algn = document.getElementById('algn');
    algn.onclick = function () {
        options_conv.style.display = 'block';
        options_conv2.style.display = 'none';
        options_cw.style.display = 'none';
        options_iter.style.display = 'none';
        options_iter2.style.display = 'none';
        algorithm = 'n';
        recompile();
    }
    const algh = document.getElementById('algh');
    algh.onclick = function () {
        options_conv.style.display = 'block';
        options_conv2.style.display = 'none';
        options_cw.style.display = 'none';
        options_iter.style.display = 'none';
        options_iter2.style.display = 'none';
        algorithm = 'h';
        recompile();
    }
    const algl = document.getElementById('algl');
    algl.onclick = function () {
        options_conv.style.display = 'block';
        options_conv2.style.display = 'none';
        options_cw.style.display = 'none';
        options_iter.style.display = 'none';
        options_iter2.style.display = 'none';
        algorithm = 'l';
        recompile();
    }
    const algs = document.getElementById('algs');
    algs.onclick = function () {
        options_conv.style.display = 'none';
        options_conv2.style.display = 'block';
        options_cw.style.display = 'none';
        options_iter.style.display = 'none';
        options_iter2.style.display = 'none';
        algorithm = 's';
        recompile();
    }
    const algcw = document.getElementById('algcw');
    algcw.onclick = function () {
        options_conv.style.display = 'none';
        options_conv2.style.display = 'none';
        options_cw.style.display = 'block';
        options_iter.style.display = 'none';
        options_iter2.style.display = 'none';
        algorithm = 'cw';
        recompile();
    }
    const algni = document.getElementById('algni');
    algni.onclick = function () {
        options_conv.style.display = 'none';
        options_conv2.style.display = 'none';
        options_cw.style.display = 'none';
        options_iter.style.display = 'block';
        options_iter2.style.display = 'none';
        algorithm = 'ni';
        recompile();
    }
    const alghi = document.getElementById('alghi');
    alghi.onclick = function () {
        options_conv.style.display = 'none';
        options_conv2.style.display = 'none';
        options_cw.style.display = 'none';
        options_iter.style.display = 'block';
        options_iter2.style.display = 'none';
        algorithm = 'hi';
        recompile();
    }
    const algli = document.getElementById('algli');
    algli.onclick = function () {
        options_conv.style.display = 'none';
        options_conv2.style.display = 'none';
        options_cw.style.display = 'none';
        options_iter.style.display = 'block';
        options_iter2.style.display = 'none';
        algorithm = 'li';
        recompile();
    }
    const algsi = document.getElementById('algsi');
    algsi.onclick = function () {
        options_conv.style.display = 'none';
        options_conv2.style.display = 'none';
        options_cw.style.display = 'none';
        options_iter.style.display = 'none';
        options_iter2.style.display = 'block';
        algorithm = 'si';
        recompile();
    }

    const canvas = document.getElementById('canvas');
    canvas.width = size * 1.5;
    canvas.height = size * 1.5;
    canvas.style.width = size + 'px';
    canvas.style.height = size + 'px';
    ctx = canvas.getContext('webgl');
    if (!ctx) {
        alert('Unable to initialize WebGL.');
        return;
    }

    const realc = document.getElementById('realc');
    realc.onchange = function () {
        requation = realc.value.split(' ').map(x => parseFloat(x, 10));
        if (requation.length > iequation.length) {
            iequation.unshift.apply(iequation,[...Array(requation.length - iequation.length)].map(x=>0));
            imagc.value = iequation.join(' ');
        } else if (requation.length < iequation.length) {
            iequation.splice(0, iequation.length - requation.length);
            imagc.value = iequation.join(' ');
        }
        recompile();
    }
    const imagc = document.getElementById('imagc');
    imagc.onchange = function () {
        iequation = imagc.value.split(' ').map(x => parseFloat(x, 10));
        if (iequation.length > requation.length) {
            requation.unshift.apply(requation,[...Array(iequation.length - requation.length)].map(x=>0));
            realc.value = requation.join(' ');
        } else if (iequation.length < requation.length) {
            requation.splice(0, requation.length - iequation.length);
            realc.value = requation.join(' ');
        }
        recompile();
    }
    const startf = document.getElementById('start');
    const startf2 = document.getElementById('start2');
    startf.onchange = function () {
        start = startf.value.split(' ');
        recompile();
        startf2.value = startf.value;
    }
    startf2.onchange = function () {
        start = startf2.value.split(' ');
        recompile();
        startf.value = startf2.value;
    }
    canvas.addEventListener('click', function (e) {
        var rect = canvas.getBoundingClientRect();
        centerx += (e.clientX - rect.left - size / 2) / (size / 2) * scale;
        centery += -(e.clientY - rect.top - size / 2) / (size / 2) * scale;
        redraw(centerx, centery, scale /= 2);
    }, false);
    recompile();
}
function recompile() {
    const vertex = compile(ctx, ctx.VERTEX_SHADER, `
precision highp float;

attribute vec4 position;
uniform float scale;
uniform float cx;
uniform float cy;
varying vec2 num;

void main(void) {
    gl_Position = position;
    num = gl_Position.xy * scale + vec2(cx, cy);
}
`);
    const fragment = compile(ctx, ctx.FRAGMENT_SHADER, `
#define count ${requation.length}
#define MAX_ITER 256

precision highp float;

uniform vec2 coefs[count];
uniform float seed;
varying vec2 num;

vec2 complex_mul(vec2 a, vec2 b)
{
  return vec2(a.x*b.x - a.y*b.y, a.x*b.y + a.y*b.x);
}
vec2 complex_div(vec2 a, vec2 b)
{
  return vec2(dot(a, b), a.y*b.x - a.x*b.y) / dot(b, b);
}
vec2 complex_sqrt(vec2 a)
{
  float ab = length(a);
  return vec2(sqrt(0.5 * (ab + a.x)), (a.y < 0.0 ? -1.0 : 1.0) * sqrt(0.5 * (ab - a.x)));
}

vec2 poly(vec2 x) {` + (numerical ? `
    return exp(x.x)*vec2(cos(x.y),sin(x.y)) - coefs[count-1];
` : `
    vec2 result = coefs[0];
    for (int c = 1; c < count; c++) {
        result = complex_mul(result, x);
        result += coefs[c];
    }
    return result;
`) + `
}

vec2 diff(vec2 x) {` + (numerical ? `
    float epsilon = 1e-1*(0.1+sqrt(dot(x,x)));
    return (poly(x + vec2(epsilon, 0.0)) - poly(x - vec2(epsilon, 0.0))) / (2.0 * epsilon);
` : `
    vec2 result = coefs[0] * float(count - 1);
    for (int c = 1; c < count - 1; c++) {
        result = complex_mul(result, x);
        result += float(count - c - 1) * coefs[c];
    }
    return result;
`) + `
}

vec2 diff2(vec2 x) {` + (numerical ? `
    float epsilon = 1e-2;
    return (poly(x - vec2(epsilon, 0.0)) - 2.0 * poly(x) + poly(x + vec2(epsilon, 0.0))) / (epsilon * epsilon);
` : `
    vec2 result = coefs[0] * float(count - 1) * float(count - 2);
    for (int c = 1; c < count - 2; c++) {
        result = complex_mul(result, x);
        result += float(count - c - 1) * float(count - c - 2) * coefs[c];
    }
    return result;
`) + `
}

float shade(float x) {
    float contrast = 10.0;
    return (exp(contrast * x) - 1.0) / (exp(contrast) - 1.0);
}

vec3 hash(vec2 z) {
    z += vec2(1e-2, 1e-2);
    float a = float(int(z.x * 1e10)) / 1e10;
    float b = float(int(z.y * 1e10)) / 1e10;
    float alt = 0.9 + 0.1 * seed;
    float rand = a * 1.0 / alt + b * exp(1.0 / alt);
    return vec3(mod(547.0 * rand, 1.0), mod(103.0 * rand, 1.0), mod(709.0 * rand, 1.0));
}

vec4 cw(vec2 z)
{
    float h = atan(-z.y,-z.x) * 3.0 / ${pi_s} + 3.0;
    float l = 1.0 - (1.0 / (log(length(z) + 1.0) + 1.0));
    float c = 1.0 - abs(2.0*l - 1.0);
    float x = c * (1.0 - abs(mod(h,2.0) - 1.0));
    vec3 r = vec3(0.0,0.0,0.0);
     if (h < 1.0) r = vec3(c,x,0.0);
else if (h < 2.0) r = vec3(x,c,0.0);
else if (h < 3.0) r = vec3(0.0,c,x);
else if (h < 4.0) r = vec3(0.0,x,c);
else if (h < 5.0) r = vec3(x,0.0,c);
else              r = vec3(c,0.0,x);
    float m = l - 0.5*c;
    return vec4(r+vec3(m,m,m),1.0);
}

void main(void) {
` + ((algorithm == 'n') ? `
    vec2 zp = num + vec2(0.0, 2.0);
    vec2 z = num;
    int itr = MAX_ITER;
    for (int c = 0; c < MAX_ITER; c++) {
        if (dot(z - zp, z - zp) < 1e-10)
            break;
        itr--;
        vec2 dz = complex_div(poly(z), diff(z));
        zp = z;
        z -= dz;
    }
    gl_FragColor = vec4(hash(z) * shade(float(itr) / float(MAX_ITER)) * (itr == 0 ? 0.0 : 1.0), 1.0);
}
` : (algorithm == 's') ? `
    vec2 zp = num + vec2(${start[0]}, ${start[1]});
    vec2 z = num;
    int itr = MAX_ITER;
    for (int c = 0; c < MAX_ITER; c++) {
        if (dot(z - zp, z - zp) < 1e-10)
            break;
        itr--;
        vec2 dz = complex_div(complex_mul((z - zp), poly(z)), poly(z) - poly(zp));
        zp = z;
        z -= dz;
    }
    gl_FragColor = vec4(hash(z) * shade(float(itr) / float(MAX_ITER)) * (itr == 0 ? 0.0 : 1.0), 1.0);
}
` : (algorithm == 'h') ? `
    vec2 zp = num + vec2(0.0, 2.0);
    vec2 z = num;
    int itr = MAX_ITER;
    for (int c = 0; c < MAX_ITER; c++) {
        if (dot(z - zp, z - zp) < 1e-10)
            break;
        itr--;
        vec2 f = poly(z);
        vec2 fp = diff(z);
        vec2 fpp = 0.5*diff2(z);
        vec2 d = complex_mul(fp, fp) - complex_mul(f, fpp);
        vec2 dz = complex_div(complex_mul(f, fp), d);

        zp = z;
        z -= dz;
    }
    gl_FragColor = vec4(hash(z) * shade(float(itr) / float(MAX_ITER)) * (itr == 0 ? 0.0 : 1.0), 1.0);
}
` : (algorithm == 'l') ? ` // FIX!
    vec2 zp = num + vec2(0.0, 2.0);
    vec2 z = num;
    int itr = MAX_ITER;
    for (int c = 0; c < MAX_ITER; c++) {
        if (dot(z - zp, z - zp) < 1e-2/sqrt(float(itr)))
            break;
        itr--;
        vec2 f = poly(z);
        vec2 fp = diff(z);
        vec2 fpp = diff2(z);

        vec2 g = complex_div(fp, f);
        vec2 h = complex_div(fpp, f);
        vec2 q = complex_sqrt(float(count - 2) * (float(count - 2) * (complex_mul(g, g) - h) - h));
        vec2 d1 = g + q;
        vec2 d2 = g - q;
        vec2 d = dot(d1, d1) > dot(d2, d2) ? d1 : d2;
        vec2 dz = complex_div(vec2(count - 1, 0.0), d);

        zp = z;
        z -= dz;
    }
    gl_FragColor = vec4(hash(z) * shade(float(itr) / float(MAX_ITER)) * (itr == 0 ? 0.0 : 1.0), 1.0);
}
` : (algorithm == 'cw') ? `
gl_FragColor = cw(poly(num));
}
` : (algorithm == 'ni') ? `
    vec2 z = num;
    for (int c = 0; c < ${iterations}; c++) {
        vec2 dz = complex_div(poly(z), diff(z));
        z -= dz;
    }
    gl_FragColor = cw(z);
}
` : (algorithm == 'hi') ? `
    vec2 z = num;
    for (int c = 0; c < ${iterations}; c++) {
        vec2 f = poly(z);
        vec2 fp = diff(z);
        vec2 fpp = 0.5*diff2(z);
        vec2 d = complex_mul(fp, fp) - complex_mul(f, fpp);
        vec2 dz = complex_div(complex_mul(f, fp), d);
        z -= dz;
    }
    gl_FragColor = cw(z);
}
` : (algorithm == 'li') ? ` // FIX!
    vec2 z = num;
    for (int c = 0; c < ${iterations}; c++) {
        vec2 f = poly(z);
        vec2 fp = diff(z);
        vec2 fpp = diff2(z);

        vec2 g = complex_div(fp, f);
        vec2 h = complex_div(fpp, f);
        vec2 q = complex_sqrt(float(count - 2) * (float(count - 2) * (complex_mul(g, g) - h) - h));
        vec2 d1 = g + q;
        vec2 d2 = g - q;
        vec2 d = dot(d1, d1) > dot(d2, d2) ? d1 : d2;
        vec2 dz = complex_div(vec2(count - 1, 0.0), d);
        z -= dz;
    }
    gl_FragColor = cw(z);
}
` : (algorithm == 'si') ? `
    vec2 zp = num + vec2(${start[0]}, ${start[1]});
    vec2 z = num;
    for (int c = 0; c < ${iterations}; c++) {
        if (dot(z - zp, z - zp) < 1e-10)
            break;
        vec2 dz = complex_div(complex_mul((z - zp), poly(z)), poly(z) - poly(zp));
        zp = z;
        z -= dz;
    }
    gl_FragColor = cw(z);
}
` : (algorithm[1] == 'd') ? `
    gl_FragColor = vec4(1.0,1.0,1.0,1.0);
    vec2 zp = num + vec2(${start[0]}, ${start[1]});
    vec2 z = num;
    for (int c = 0; c < ${iterations}+1; c++) {
` +((algorithm[0] == 'n') ? `
        vec2 dz = complex_div(poly(z), diff(z));
` : (algorithm[0] == 'h') ? `
        vec2 f = poly(z);
        vec2 fp = diff(z);
        vec2 fpp = 0.5*diff2(z);
        vec2 d = complex_mul(fp, fp) - complex_mul(f, fpp);
        vec2 dz = complex_div(complex_mul(f, fp), d);
` : (algorithm[0] == 'l') ? `
        vec2 f = poly(z);
        vec2 fp = diff(z);
        vec2 fpp = diff2(z);

        vec2 g = complex_div(fp, f);
        vec2 h = complex_div(fpp, f);
        vec2 q = complex_sqrt(float(count - 2) * (float(count - 2) * (complex_mul(g, g) - h) - h));
        vec2 d1 = g + q;
        vec2 d2 = g - q;
        vec2 d = dot(d1, d1) > dot(d2, d2) ? d1 : d2;
        vec2 dz = complex_div(vec2(count - 1, 0.0), d);
` : (algorithm[0] == 's') ? `
        if (dot(z - zp, z - zp) < 1e-10)
            break;
        vec2 dz = complex_div(complex_mul((z - zp), poly(z)), poly(z) - poly(zp));
        zp = z;
`:'')+((algorithm[2] == '1') ? `
        gl_FragColor = vec4(0.5*length(dz)*vec3(1.0,1.0,1.0),1.0);
        // gl_FragColor = vec4((-log(length(dz)))*vec3(1.0,1.0,1.0),1.0);
        z -= dz;
    }
}
` : (algorithm[2] == '2') ? `
        if (mod(float(c), 3.0) < 0.9) {
            gl_FragColor.r = 0.5*(gl_FragColor.r + log(length(dz)));
        } else if (mod(float(c), 3.0) < 1.9) {
            gl_FragColor.g = 0.5*(gl_FragColor.g + log(length(dz)));
        } else {
            gl_FragColor.b = 0.5*(gl_FragColor.b + log(length(dz)));
        }
        z -= dz;
    }
}
` : (algorithm[2] == 'c') ? `
        gl_FragColor = cw(dz);
        z -= dz;
    }
}
` : ''): '}'));

    program = ctx.createProgram();
    ctx.attachShader(program, vertex);
    ctx.attachShader(program, fragment);
    ctx.linkProgram(program);
    if (!ctx.getProgramParameter(program, ctx.LINK_STATUS)) {
        alert('Could not load: ' + ctx.getProgramInfoLog(program));
        return;
    }

    buffer = ctx.createBuffer();
    ctx.bindBuffer(ctx.ARRAY_BUFFER, buffer);
    const positions = [
        1.0, 1.0,
        -1.0, 1.0,
        1.0, -1.0,
        -1.0, -1.0,
    ];
    ctx.bufferData(ctx.ARRAY_BUFFER, new Float32Array(positions), ctx.STATIC_DRAW);
    redraw(centerx, centery, scale);
}

function redraw(cx, cy, scale) {
    ctx.clearColor(0.0, 0.0, 0.0, 1.0);
    ctx.clear(ctx.COLOR_BUFFER_BIT | ctx.DEPTH_BUFFER_BIT);

    const pos = ctx.getAttribLocation(program, 'position')
    const numComponents = 2;
    const type = ctx.FLOAT;
    const normalize = false;
    const stride = 0;
    const offset = 0;
    ctx.bindBuffer(ctx.ARRAY_BUFFER, buffer);
    ctx.vertexAttribPointer(
        pos,
        numComponents,
        type,
        normalize,
        stride,
        offset);
    ctx.enableVertexAttribArray(pos);
    ctx.useProgram(program);

    const seed = ctx.getUniformLocation(program, "seed");
    ctx.uniform1f(seed, seedval);
    const coefs = ctx.getUniformLocation(program, "coefs");
    ctx.uniform2fv(coefs, new Float32Array([...Array(2*requation.length).keys()].map(i => i%2 ? iequation[(i-1)/2] : requation[i/2])));
    const cxl = ctx.getUniformLocation(program, "cx");
    ctx.uniform1f(cxl, cx);
    const cyl = ctx.getUniformLocation(program, "cy");
    ctx.uniform1f(cyl, cy);
    const scalel = ctx.getUniformLocation(program, "scale");
    ctx.uniform1f(scalel, scale);
    const vertexCount = 4;
    ctx.drawArrays(ctx.TRIANGLE_STRIP, offset, vertexCount);
}

function compile(ctx, type, source) {
    const shader = ctx.createShader(type);
    ctx.shaderSource(shader, source);
    ctx.compileShader(shader);
    if (!ctx.getShaderParameter(shader, ctx.COMPILE_STATUS)) {
        alert('Compile error: \n' + ctx.getShaderInfoLog(shader));
        ctx.deleteShader(shader);
        return null;
    }
    return shader;
}
