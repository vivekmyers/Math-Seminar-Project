window.onload = main;

scale = 1;
size = 800;
seedval = Math.random();
centerx = 0;
centery = 0;
equation = [1, 0, 0, -1];

function get(f) {
    const x = new XMLHttpRequest();
    x.open('GET', f, false);
    x.send();
    return x.responseText;
}

function click(e) {
    var rect = this.getBoundingClientRect();
    centerx += (e.clientX - rect.left - size / 2) / (size / 2) * scale;
    centery += -(e.clientY - rect.top - size / 2) / (size / 2) * scale;
    redraw(centerx, centery, scale /= 2);
}

function main() {
    const canvas = document.getElementById('fractal');
    canvas.width = size * 1.5;
    canvas.height = size * 1.5;
    canvas.style.width = size + 'px';
    canvas.style.height = size + 'px';
    ctx = canvas.getContext('webgl');
    if (!ctx) {
        alert('Unable to initialize WebGL.');
        return;
    }
    canvas.addEventListener('click', click.bind(canvas), false);

    const vssrc = get('vertex.glsl');
    const fssrc = get('fragment.glsl');
    const vertex = compile(ctx, ctx.VERTEX_SHADER, vssrc);
    const fragment = compile(ctx, ctx.FRAGMENT_SHADER, fssrc);

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
    ctx.uniform1fv(coefs, new Float32Array(nums));
    let degree = ctx.getUniformLocation(program, "degree");
    ctx.uniform1f(degree, nums.length);
    const cxl = ctx.getUniformLocation(program, "cx");
    ctx.uniform1f(cxl, cx);
    const cyl = ctx.getUniformLocation(program, "cy");
    ctx.uniform1f(cyl, cy);
    const scalel = ctx.getUniformLocation(program, "scale");
    ctx.uniform1f(scalel, scale);
    const offset = 0;
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

