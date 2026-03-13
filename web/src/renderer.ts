const VS_SOURCE = `#version 300 es
  void main() {
    float x = -1.0 + float((gl_VertexID & 1) << 2);
    float y = -1.0 + float((gl_VertexID & 2) << 1);
    gl_Position = vec4(x, y, 0, 1);
  }`;

const INTERVAL = 1000 / 120;

let gl: WebGL2RenderingContext;
let program: WebGLProgram | null = null;
let mouseX = 0;
let mouseY = 0;
let lastTime = 0;

export function compileAndLinkGLSL(glsl: string): string | null {
  const compile = (source: string, type: number): WebGLShader | string => {
    const s = gl.createShader(type)!;
    gl.shaderSource(s, source);
    gl.compileShader(s);
    if (!gl.getShaderParameter(s, gl.COMPILE_STATUS)) {
      return gl.getShaderInfoLog(s) ?? "Unknown shader error";
    }
    return s;
  };

  const vs = compile(VS_SOURCE, gl.VERTEX_SHADER);
  if (typeof vs === "string") return vs;

  const fs = compile(glsl, gl.FRAGMENT_SHADER);
  if (typeof fs === "string") {
    program = null;
    return fs;
  }

  const p = gl.createProgram()!;
  gl.attachShader(p, vs);
  gl.attachShader(p, fs);
  gl.linkProgram(p);
  if (!gl.getProgramParameter(p, gl.LINK_STATUS)) {
    const log = gl.getProgramInfoLog(p);
    program = null;
    return log;
  }

  program = p;
  return null;
}

function render(currentTime: number): void {
  requestAnimationFrame(render);

  const delta = currentTime - lastTime;
  if (delta >= INTERVAL && program != null) {
    lastTime = currentTime - (delta % INTERVAL);

    gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
    gl.useProgram(program);
    gl.uniform2f(
      gl.getUniformLocation(program, "u_resolution"),
      gl.canvas.width,
      gl.canvas.height,
    );
    gl.uniform2f(
      gl.getUniformLocation(program, "u_mouse"),
      mouseX,
      mouseY,
    );
    gl.uniform1f(
      gl.getUniformLocation(program, "u_time"),
      currentTime / 1000,
    );
    gl.drawArrays(gl.TRIANGLES, 0, 3);
  }
}

export function initRenderer(canvas: HTMLCanvasElement): void {
  const container = canvas.parentElement!;
  gl = canvas.getContext("webgl2")!;

  const resizeObserver = new ResizeObserver((entries) => {
    for (const entry of entries) {
      const size = Math.min(entry.contentRect.width, entry.contentRect.height);
      canvas.width = size;
      canvas.height = size;
      canvas.style.width = size + "px";
      canvas.style.height = size + "px";
    }
  });
  resizeObserver.observe(container);

  canvas.addEventListener("mousemove", (e) => {
    const rect = canvas.getBoundingClientRect();
    mouseX = e.clientX - rect.left;
    mouseY = rect.height - (e.clientY - rect.top);
  });

  requestAnimationFrame((time) => {
    lastTime = time;
    render(time);
  });
}
