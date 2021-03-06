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
vec2 complex_sqrt(vec2 a)
{
  float ab = sqrt(dot(a,a));
  return vec2(sqrt(0.5 * (ab + a.x)), (a.y < 0.f ? -1.f : 1.f) * sqrt(0.5 * (ab - a.x)));
}
void main()
{
  vec2 point = bottom_right + (texture_coord + offset) * view_size;
  int closest_root = -1;
  int iterations = 0;
  while (iterations < max_iterations && closest_root < 0) {
    vec2 f = vec2(1.f, 0.f);
    vec2 f_prime = vec2(0.f, 0.f);
    vec2 f_2prime = vec2(0.f, 0.f);
    for (int i = 0; i < root_count; ++i) {
      for (int j = i + 1; j < root_count; ++j) {
        vec2 a = vec2(1.f, 0.f);
        for (int k = 0; k < root_count; ++k)
          if (k != i && k != j)
            a = complex_mul(a, point - roots[k]);
        f_2prime += a;
      }
      vec2 a = vec2(2.f, 0.f);
      for (int j = 0; j < root_count; ++j)
        if (j != i)
          a = complex_mul(a, point - roots[j]);
      f_prime += a;
      f = complex_mul(f, point - roots[i]);
    }
    vec2 g = complex_div(f_prime, f);
    vec2 h = complex_div(f_2prime, f);
    float n1 = root_count - 1;
    vec2 q = complex_sqrt(n1 * (n1 * (complex_mul(g, g) - h) - h));
    vec2 d1 = g + q;
    vec2 d2 = g - q;
    vec2 d = dot(d1, d1) > dot(d2, d2) ? d1 : d2;
    if (d == vec2(0.f, 0.f)) discard;
    point -= complex_div(vec2(root_count, 0.f), d);
    for (int k = 0; k < root_count; ++k)
      if (dot(point - roots[k], point - roots[k]) < tolerance) {
        closest_root = k;
        break;
      }
    ++iterations;
  }
  if (closest_root < 0) {
    float best_distance = dot(point - roots[0], point - roots[0]);
    for (int k = 1; k < root_count; ++k) {
      float d = dot(point - roots[k], point - roots[k]);
      if (d < best_distance) {
        best_distance = d;
        closest_root = k;
      }
    }
  }
  color = color_scaling * texelFetch(root_colors, closest_root, 0) / pow(float(iterations),0.5);
  color.a = 1.f;
}

