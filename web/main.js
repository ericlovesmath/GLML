// Provides: state
const state = {
  gl: null,
  program: null,
  mouseX: 0,
  mouseY: 0,
  lastTime: 0,
};

// Provides: vs_source const
//
// Procedural full-screen triangle trick, covers whole square
// Vertex 0: (-1, -1), Vertex 1: (3, -1), Vertex 2: (-1, 3)
const vs_source = `#version 300 es
  void main() {
    float x = -1.0 + float((gl_VertexID & 1) << 2);
    float y = -1.0 + float((gl_VertexID & 2) << 1);
    gl_Position = vec4(x, y, 0, 1);
  }`;

// Provides: interval const
const interval = 1000 / 120;

// Provides: compileAndLinkGLSL
// Requires: vs_source, render, state
function compileAndLinkGLSL(shader) {
  const gl = state.gl;

  const compile = (source, type) => {
    let s = gl.createShader(type);
    gl.shaderSource(s, source);
    gl.compileShader(s);
    if (!gl.getShaderParameter(s, gl.COMPILE_STATUS))
      throw Error(gl.getShaderInfoLog(s));
    return s;
  };

  let vs = compile(vs_source, gl.VERTEX_SHADER);

  let fs;
  try {
    fs = compile(shader, gl.FRAGMENT_SHADER);
  } catch (error) {
    console.error("GLSL Error!", error);
    state.program = null;
    return null;
  }

  state.program = gl.createProgram();
  gl.attachShader(state.program, vs);
  gl.attachShader(state.program, fs);
  gl.linkProgram(state.program);
  if (!gl.getProgramParameter(state.program, gl.LINK_STATUS))
    throw Error(gl.getProgramInfoLog(state.program));
}

// Provides: render
// Requires: state, interval
function render(currentTime) {
  let gl = state.gl;

  window.requestAnimationFrame(render);

  const delta = currentTime - state.lastTime;

  if (delta >= interval && state.program != null) {
    state.lastTime = currentTime - (delta % interval);

    gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
    gl.useProgram(state.program);
    gl.uniform2f(
      gl.getUniformLocation(state.program, "u_resolution"),
      gl.canvas.width,
      gl.canvas.height,
    );
    gl.uniform2f(
      gl.getUniformLocation(state.program, "u_mouse"),
      state.mouseX,
      state.mouseY,
    );
    gl.uniform1f(gl.getUniformLocation(state.program, "u_time"), currentTime);
    gl.drawArrays(gl.TRIANGLES, 0, 3);

    window.requestAnimationFrame(render);
  }
}

// Provides: init
// Requires: vs_source, compileAndLinkGLSL, render, state
function init() {
  const canvas = document.getElementById("gl-canvas");
  const container = canvas.parentElement;
  state.gl = canvas.getContext("webgl2");

  // Find the smallest dimension to maintain a square, resize canvas
  const resizeObserver = new window.ResizeObserver((entries) => {
    for (let entry of entries) {
      const size = Math.min(entry.contentRect.width, entry.contentRect.height);
      canvas.width = size;
      canvas.height = size;
      canvas.style.width = size + "px";
      canvas.style.height = size + "px";
    }
  });
  resizeObserver.observe(container);

  // Update mouseX and mouseY to be coords on canvas
  canvas.addEventListener("mousemove", (e) => {
    const rect = canvas.getBoundingClientRect();
    state.mouseX = e.clientX - rect.left;
    state.mouseY = rect.height - (e.clientY - rect.top);
  });

  window.requestAnimationFrame((time) => {
    state.lastTime = time;
    render(time);
  });
}
