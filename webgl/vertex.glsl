precision highp float;

attribute vec4 position;
uniform float scale;
uniform float cx;
uniform float cy;
varying float x;
varying float y;

void main(void) {
    gl_Position = position;
    x = gl_Position.x * scale + cx;
    y = gl_Position.y * scale + cy;
}

