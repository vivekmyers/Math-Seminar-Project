#version 460

layout(location = 0) uniform vec2 bottom_right;
layout(location = 1) uniform vec2 view_size;

layout(location = 2) uniform int max_iterations;
layout(location = 3) uniform float tolerance;

layout(location = 4) uniform int root_count;
layout(location = 5) uniform sampler1D root_colors;
layout(location = 6) uniform float color_scaling;
layout(location = 7) uniform vec2 offset;
layout(location = 8) uniform vec2 roots[16];

in vec2 texture_coord;
out vec4 color;

vec2 complex_mul(vec2 a, vec2 b)
{
  return vec2(a.x*b.x - a.y*b.y, a.x*b.y + a.y*b.x);
}
vec2 complex_div(vec2 a, vec2 b)
{
  return vec2(dot(a, b), a.y*b.x - a.x*b.y) / dot(b, b);
}
void main()
{
  vec2 point = bottom_right + (texture_coord + offset) * view_size +
    (0.01f / (tolerance + max_iterations));
  float best_distance = dot(point - roots[0], point - roots[0]);
  int closest_root = -1;
  for (int k = 1; k < root_count; ++k) {
    float d = dot(point - roots[k], point - roots[k]);
    if (d < best_distance) {
      best_distance = d;
      closest_root = k;
    }
  }
  if (closest_root < 0) discard;
  color = color_scaling * texelFetch(root_colors, closest_root, 0) / pow(10*float(best_distance),0.5);
  color.a = 1.f;
}

