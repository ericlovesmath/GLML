// Procedural full-screen triangle trick, covers whole square
// Vertex 0: (-1, -1), Vertex 1: (3, -1), Vertex 2: (-1, 3)

//Provides: vs_source
const vs_source = `#version 300 es
  void main() {
    float x = -1.0 + float((gl_VertexID & 1) << 2);
    float y = -1.0 + float((gl_VertexID & 2) << 1);
    gl_Position = vec4(x, y, 0, 1);
  }`;

// FPS Capping variables

//Provides: lastTime
let lastTime = 0;
//Provides: fpsLimit
const fpsLimit = 120;
//Provides: interval
const interval = 1000 / fpsLimit;

//Provides: compileAndLinkGLSL
function compileAndLinkGLSL(shader) {
  let gl = window.gl;
  const compile = (source, type) => {
    let s = gl.createShader(type);
    gl.shaderSource(s, source);
    gl.compileShader(s);
    if (!gl.getShaderParameter(s, gl.COMPILE_STATUS))
      throw Error(gl.getShaderInfoLog(s));
    return s;
  };

  let vs = compile(vs_source, gl.VERTEX_SHADER);
  console.log("hello!");
  console.log(shader);

  let fs;
  try {
    fs = compile(shader, gl.FRAGMENT_SHADER);
  } catch (_) {
    window.program = null;
    return null;
  }

  window.program = gl.createProgram();
  gl.attachShader(window.program, vs);
  gl.attachShader(window.program, fs);
  gl.linkProgram(window.program);
  if (!gl.getProgramParameter(window.program, gl.LINK_STATUS))
    throw Error(gl.getProgramInfoLog(window.program));
}

window.render = function (currentTime) {
  let gl = window.gl;
  requestAnimationFrame(render);

  const delta = currentTime - window.lastTime;

  if (delta >= interval && window.program != null) {
    window.lastTime = currentTime - (delta % interval);

    gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
    gl.useProgram(window.program);
    gl.uniform2f(
      gl.getUniformLocation(window.program, "u_resolution"),
      gl.canvas.width,
      gl.canvas.height,
    );
    gl.uniform2f(
      gl.getUniformLocation(window.program, "u_mouse"),
      window.mouseX,
      window.mouseY,
    );
    gl.uniform1f(gl.getUniformLocation(window.program, "u_time"), currentTime);
    gl.drawArrays(gl.TRIANGLES, 0, 3);
    requestAnimationFrame(render);
  }
};

//Provides: main
function main() {
  window.gl = null;
  window.program;
  window.mouseX = 0;
  window.mouseY = 0;
  window.lastTime = 0;

  console.log("hello from main!");
  const canvas = document.getElementById("gl-canvas");
  const container = canvas.parentElement;
  window.gl = canvas.getContext("webgl2");
  console.log(window.gl);

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
    window.mouseX = e.clientX - rect.left;
    window.mouseY = rect.height - (e.clientY - rect.top);
  });

  compileAndLinkGLSL("");
  resize();
  requestAnimationFrame((time) => {
    window.lastTime = time;
    window.render(time);
  });
}
