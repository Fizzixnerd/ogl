attribute vec3 Position;

uniform mat4 gWorld;

varying vec3 Color;

void main() {
  gl_Position = gWorld * vec4(Position, 1.0);
  Color = clamp(Position, 0.0, 1.0);
}
