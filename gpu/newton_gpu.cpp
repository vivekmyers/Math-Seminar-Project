#define GLEW_STATIC
#include <GL/glew.h>

#include <GLFW/glfw3.h>

#include <cstdio>
#include <fstream>

const int WINDOW_WIDTH = 900;
const int WINDOW_HEIGHT = 900;

const char *WINDOW_TITLE = "Newton GPU";

void glfw_debug_callback(int error, const char *description)
{
  printf("[GLFW] %d: %s\n", error, description);
}

GLuint load_shader_from_file(const char *path, GLenum shader_type)
{
  char *buffer;
  std::ifstream file;
  std::streampos size;
  file.open(path, std::ios::binary | std::ios::ate);
  if (file.is_open()) {
    size = file.tellg();
    buffer = new char[(size_t) size + 1];
    file.seekg(0, std::ios::beg);
    file.read(buffer, size);
    file.close();
    buffer[size] = 0;
  } else {
    printf("Unable to open shader file: %s\n", path);
    return 0;
  }

  GLuint shader_id = glCreateShader(shader_type);
  glShaderSource(shader_id, 1, &buffer, NULL);
  glCompileShader(shader_id);

  GLint shader_compiled = GL_FALSE;
  glGetShaderiv(shader_id, GL_COMPILE_STATUS, &shader_compiled);
  if (shader_compiled != GL_TRUE) {
    printf("Failed to compile shader:\n%s\n", buffer);

    GLint log_length, max_length;
    glGetShaderiv(shader_id, GL_INFO_LOG_LENGTH, &max_length);
    char *log = new char[max_length];
    glGetShaderInfoLog(shader_id, max_length, &log_length, log);
    if (log_length > 0) {
      printf("%s\n", log);
    }
    delete[] log;

    glDeleteShader(shader_id);
    return 0;
  }

  delete[] buffer;
  return shader_id;
}

int main()
{
  int num_roots, samples, iterations;
  GLfloat real_min, real_max, imag_min, imag_max, tolerance;
  printf("number of roots (max 10): ");
  scanf("%d", &num_roots);
  GLfloat *roots = new GLfloat[num_roots * 2];
  for (int i = 0; i < num_roots; ++i) {
    printf("root %d: ", i + 1);
    scanf("%f %f", roots + 2*i, roots + 2*i + 1);
  }
  printf("real range: ");
  scanf("%f %f", &real_min, &real_max);
  printf("imaginary range: ");
  scanf("%f %f", &imag_min, &imag_max);
  printf("tolerance: ");
  scanf("%f", &tolerance);
  printf("max iterations: ");
  scanf("%d", &iterations);
  printf("supersampling: ");
  scanf("%d", &samples);

  if (!glfwInit()) {
    printf("GLFW initialization failed\n");
  }
  glfwSetErrorCallback(glfw_debug_callback);

  glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);
  GLFWwindow *window = glfwCreateWindow(WINDOW_WIDTH, WINDOW_HEIGHT, WINDOW_TITLE, nullptr, nullptr);
  if (!window) {
    printf("Failed to create window\n");
  }

  glfwMakeContextCurrent(window);
  GLenum glew_error = glewInit();
  if (glew_error != GLEW_OK) {
    printf("GLEW initialization failed: %s\n", glewGetErrorString(glew_error));
  }

  GLubyte palette_data[10*3] = {
    0xff, 0x00, 0x00,
    0x00, 0xff, 0x00,
    0x00, 0x00, 0xff,
	0xff, 0xff, 0x00,
	0x00, 0xff, 0xff,
	0xff, 0x00, 0xff,
	0xff, 0x80, 0x00,
	0x00, 0xff, 0x80,
	0x80, 0x00, 0xff,
	0xbb, 0xbb, 0xbb,
  };
  unsigned char palette_size = 10;
  GLuint palette_texture;
  glGenTextures(1, &palette_texture);
  glBindTexture(GL_TEXTURE_1D, palette_texture);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexImage1D(GL_TEXTURE_1D, 0, GL_RGB8, palette_size, 0, GL_RGB, GL_UNSIGNED_BYTE, &palette_data);
  glBindTexture(GL_TEXTURE_1D, 0);

  GLuint vertex_shader, fragment_shader;
  vertex_shader = load_shader_from_file("vertex.glsl", GL_VERTEX_SHADER);
  fragment_shader = load_shader_from_file("fragment.glsl", GL_FRAGMENT_SHADER);
  if (vertex_shader == 0 || fragment_shader == 0) {
    glDeleteShader(vertex_shader);
    glDeleteShader(fragment_shader);
    printf("Failed to create shaders\n");
    return 1;
  }
  GLuint program = glCreateProgram();
  glAttachShader(program, vertex_shader);
  glAttachShader(program, fragment_shader);
  glLinkProgram(program);

  GLint link_status = GL_FALSE;
  glGetProgramiv(program, GL_LINK_STATUS, &link_status);
  if (link_status != GL_TRUE) {
    printf("Failed to link program\n");

    GLint log_length, max_length;
    glGetProgramiv(program, GL_INFO_LOG_LENGTH, &max_length);
    char *log = new char[max_length];
    glGetProgramInfoLog(program, max_length, &log_length, log);
    if (log_length > 0) {
      printf("%s\n", log);
    }
    delete[] log;

    glDeleteShader(vertex_shader);
    glDeleteShader(fragment_shader);
    glDeleteProgram(program);
    return 1;
  }

  glDeleteShader(vertex_shader);
  glDeleteShader(fragment_shader);

  glUseProgram(program);

  glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
  glClear(GL_COLOR_BUFFER_BIT);
  glBindTexture(GL_TEXTURE_1D, palette_texture);


  glUniform2f(0, real_min, imag_min);//bottom_right
  glUniform2f(1, real_max - real_min, imag_max - imag_min);//view_size
  glUniform1i(2, iterations);//iterations
  glUniform1f(3, tolerance);//tolerance
  glUniform1i(4, num_roots);//root_count
  glUniform1i(5, 0);//root_colors
  glUniform1f(6, 1.f/samples/samples);//color_scaling
  glUniform2fv(8, num_roots, roots);//roots
  delete[] roots;

  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE, GL_ONE);
  for (int x = 0; x < samples; ++x) {
    for (int y = 0; y < samples; ++y) {
      glUniform2f(7, 2.f*x/WINDOW_WIDTH/samples - 1.f/WINDOW_WIDTH, 2.f*y/WINDOW_HEIGHT/samples - 1.f/WINDOW_HEIGHT);//offset
      glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
    }
  }

  glfwSwapBuffers(window);
  while (!glfwWindowShouldClose(window)) {
    glfwWaitEvents();
  }

  glfwDestroyWindow(window);
  glfwTerminate();
  return 0;
}
