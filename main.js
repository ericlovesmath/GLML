// Procedural full-screen triangle trick, covers whole square
// Vertex 0: (-1, -1), Vertex 1: (3, -1), Vertex 2: (-1, 3)
const vs_source = `#version 300 es
  void main() {
    float x = -1.0 + float((gl_VertexID & 1) << 2);
    float y = -1.0 + float((gl_VertexID & 2) << 1);
    gl_Position = vec4(x, y, 0, 1);
  }`;

let gl, program;
let mouseX = 0;
let mouseY = 0;

// FPS Capping variables
let lastTime = 0;
const fpsLimit = 120;
const interval = 1000 / fpsLimit;

function compileAndLinkGLSL() {
  const compile = (source, type) => {
    let s = gl.createShader(type);
    gl.shaderSource(s, source);
    gl.compileShader(s);
    if (!gl.getShaderParameter(s, gl.COMPILE_STATUS))
      throw Error(gl.getShaderInfoLog(s));
    return s;
  };

  let vs = compile(vs_source, gl.VERTEX_SHADER);
  let fs = compile(mlsl.shader, gl.FRAGMENT_SHADER);

  program = gl.createProgram();
  gl.attachShader(program, vs);
  gl.attachShader(program, fs);
  gl.linkProgram(program);
  if (!gl.getProgramParameter(program, gl.LINK_STATUS))
    throw Error(gl.getProgramInfoLog(program));
}

function render(currentTime) {
  requestAnimationFrame(render);

  const delta = currentTime - lastTime;

  if (delta >= interval) {
    lastTime = currentTime - (delta % interval);

    gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
    gl.useProgram(program);
    gl.uniform2f(
      gl.getUniformLocation(program, "u_resolution"),
      gl.canvas.width,
      gl.canvas.height,
    );
    gl.uniform2f(gl.getUniformLocation(program, "u_mouse"), mouseX, mouseY);
    gl.drawArrays(gl.TRIANGLES, 0, 3);
    requestAnimationFrame(render);
  }
}

function main() {
  const canvas = document.getElementById("gl-canvas");
  const container = canvas.parentElement;
  gl = canvas.getContext("webgl2");

  // Find the smallest dimension to maintain a square, resize canvas
  const resize = () => {
    const rect = container.getBoundingClientRect();
    const size = Math.min(rect.width, rect.height);

    canvas.width = size;
    canvas.height = size;

    canvas.style.width = size + "px";
    canvas.style.height = size + "px";
  };

  // TODO: Make this react to container size, not window
  window.addEventListener("resize", resize);

  // Update mouseX and mouseY to be coords on canvas
  canvas.addEventListener("mousemove", (e) => {
    const rect = canvas.getBoundingClientRect();
    mouseX = e.clientX - rect.left;
    mouseY = rect.height - (e.clientY - rect.top);
  });

  compileAndLinkGLSL();
  resize();
  requestAnimationFrame((time) => {
    lastTime = time;
    render(time);
  });
}

main();
