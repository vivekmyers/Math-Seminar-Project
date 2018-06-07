scale = 2;
size = 800;
seedval = Math.random();
centerx = 0;
centery = 0;
equation = prompt('Coefficients: ').split(' ').map(x => parseInt(x, 10));

window.onload = function () {
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
    canvas.addEventListener('click', function (e) {
        var rect = canvas.getBoundingClientRect();
        centerx += (e.clientX - rect.left - size / 2) / (size / 2) * scale;
        centery += -(e.clientY - rect.top - size / 2) / (size / 2) * scale;
        redraw(centerx, centery, scale /= 2);
    }, false);

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
#define count 50
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

vec2 poly(vec2 x) {
    vec2 result = coefs[0];
    for (int c = 1; c < count; c++) {
        result = complex_mul(result, x);
        result += coefs[c];
    }
    return result;
}

vec2 diff(vec2 x) {
    vec2 result = coefs[0] * float(count - 1);
    for (int c = 2; c < count; c++) {
        result = complex_mul(result, x);
        result += float(count - c) * coefs[c - 1];
    }
    return result;
}

float shade(float x) {
    float contrast = 10.0;
    return (exp(contrast * x) - 1.0) / (exp(contrast) - 1.0);
}

vec3 hash(vec2 root) {
    float a = float(int(root.x * 1e10)) / 1e10;
    float b = float(int(root.y * 1e10)) / 1e10;
    float alt = 0.9 + 0.1 * seed;
    float rand = a * 1.0 / alt + b * exp(1.0 / alt);
    return vec3(mod(547.0 * rand, 1.0), mod(103.0 * rand, 1.0), mod(709.0 * rand, 1.0));
}

void main(void) {
    vec2 current = num + vec2(0.0, 2.0);
    vec2 next = num;
    int itr = MAX_ITER;
    for (int c = 0; c < MAX_ITER; c++) {
        if (dot(next - current, next - current) < 1e-10)
            break;
        itr--;
        vec2 dx = complex_div(poly(next), diff(next));
        current = next;
        next -= dx;
    }
    gl_FragColor = vec4(hash(next) * shade(float(itr) / float(MAX_ITER)) * (itr == 0 ? 0.0 : 1.0), 1.0);
}
`);

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
    redraw(0, 0, scale);
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
    const nums = equation.slice();
    while (nums.length < 50) {
        nums.splice(0, 0, 0);
    }
    ctx.uniform2fv(coefs, new Float32Array([...Array(100).keys()].map(i => i%2 ? 0 : nums[i/2])));
    let degree = ctx.getUniformLocation(program, "degree");
    ctx.uniform1f(degree, nums.length);
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
