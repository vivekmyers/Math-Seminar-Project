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
  float epsilon = 1e-2;
  vec2 point = bottom_right + (texture_coord + offset) * view_size;
  int closest_root = -1;
  int iterations = 0;
  while (iterations < max_iterations && closest_root < 0) {
    vec2 point1 = point + vec2(epsilon, 0.f);
    vec2 point2 = point - vec2(epsilon, 0.f);
    vec2 point3 = point + vec2(0.f, epsilon);
    vec2 point4 = point - vec2(0.f, epsilon);
    vec2 f = point - roots[0];
    vec2 f1 = point1 - roots[0];
    vec2 f2 = point2 - roots[0];
    vec2 f3 = point3 - roots[0];
    vec2 f4 = point4 - roots[0];
    for (int i = 1; i < root_count; ++i) {
      f = complex_mul(f, point - roots[i]);
      f1 = complex_mul(f1, point1 - roots[i]);
      f2 = complex_mul(f2, point2 - roots[i]);
      f3 = complex_mul(f3, point3 - roots[i]);
      f4 = complex_mul(f4, point4 - roots[i]);
    }
    vec2 fp1 = (f1 - f) / epsilon;
    vec2 fp2 = -(f2 - f) / epsilon;
    vec2 fp3 = (f3 - f) / epsilon;
    fp3 = vec2(fp3.y, -fp3.x);
    vec2 fp4 = (f4 - f) / epsilon;
    fp4 = vec2(-fp4.y, fp4.x);
    vec2 f_prime = 0.25 * (fp1 + fp2 + fp3 + fp4);
    if (f_prime == vec2(0.f, 0.f)) discard;
    point -= complex_div(f, f_prime);
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

