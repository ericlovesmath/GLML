(function () {
  const VS = `#version 300 es
void main() {
  float x = -1.0 + float((gl_VertexID & 1) << 2);
  float y = -1.0 + float((gl_VertexID & 2) << 1);
  gl_Position = vec4(x, y, 0, 1);
}`;

  function fail(canvas, msg) {
    console.error("GLML:", msg);
    const div = document.createElement("div");
    div.className = "glml-error";
    div.textContent = msg;
    canvas.replaceWith(div);
  }

  function makeShader(gl, type, source) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    if (gl.getShaderParameter(shader, gl.COMPILE_STATUS)) return shader;
    const log = gl.getShaderInfoLog(shader);
    gl.deleteShader(shader);
    throw new Error(log || "shader compile failed");
  }

  function makeProgram(gl, fragmentSource) {
    const vs = makeShader(gl, gl.VERTEX_SHADER, VS);
    const fs = makeShader(gl, gl.FRAGMENT_SHADER, fragmentSource);
    const program = gl.createProgram();
    gl.attachShader(program, vs);
    gl.attachShader(program, fs);
    gl.linkProgram(program);
    if (gl.getProgramParameter(program, gl.LINK_STATUS)) return program;
    const log = gl.getProgramInfoLog(program);
    gl.deleteProgram(program);
    throw new Error(log || "program link failed");
  }

  function mount(canvas, glsl) {
    const gl = canvas.getContext("webgl2");
    if (!gl) return fail(canvas, "WebGL2 not supported");

    let program;
    try {
      program = makeProgram(gl, glsl);
    } catch (e) {
      return fail(canvas, e.message);
    }

    const uRes = gl.getUniformLocation(program, "u_resolution");
    const uMouse = gl.getUniformLocation(program, "u_mouse");
    const uTime = gl.getUniformLocation(program, "u_time");

    let mouseX = 0;
    let mouseY = 0;
    let rafId = null;

    new ResizeObserver(() => {
      const rect = canvas.getBoundingClientRect();
      const dpr = window.devicePixelRatio || 1;
      const size = Math.max(1, Math.floor(Math.min(rect.width, rect.height) * dpr));
      if (canvas.width !== size) canvas.width = canvas.height = size;
    }).observe(canvas);

    canvas.addEventListener("mousemove", (e) => {
      const rect = canvas.getBoundingClientRect();
      const dpr = window.devicePixelRatio || 1;
      mouseX = (e.clientX - rect.left) * dpr;
      mouseY = (rect.height - (e.clientY - rect.top)) * dpr;
    });

    function frame(t) {
      gl.viewport(0, 0, canvas.width, canvas.height);
      gl.useProgram(program);
      if (uRes) gl.uniform2f(uRes, canvas.width, canvas.height);
      if (uMouse) gl.uniform2f(uMouse, mouseX, mouseY);
      if (uTime) gl.uniform1f(uTime, t / 1000);
      gl.drawArrays(gl.TRIANGLES, 0, 3);
      rafId = requestAnimationFrame(frame);
    }

    new IntersectionObserver(([entry]) => {
      if (entry.isIntersecting && rafId === null) {
        rafId = requestAnimationFrame(frame);
      } else if (!entry.isIntersecting && rafId !== null) {
        cancelAnimationFrame(rafId);
        rafId = null;
      }
    }).observe(canvas);
  }

  document.addEventListener("DOMContentLoaded", () => {
    for (const canvas of document.querySelectorAll("canvas[data-glml-src]")) {
      const script = document.getElementById(canvas.dataset.glmlSrc);
      if (script) mount(canvas, script.textContent.trim());
    }
  });
})();
