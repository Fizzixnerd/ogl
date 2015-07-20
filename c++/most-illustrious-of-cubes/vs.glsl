
attribute vec3 pos;
attribute vec3 color;
varying vec3 f_color;
uniform mat4 mvp;
void main() {
  gl_Position = mvp * vec4(pos, 1.0);
  f_color = color;
}

