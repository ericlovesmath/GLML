const VS_SOURCE = `#version 300 es
  void main() {
    float x = -1.0 + float((gl_VertexID & 1) << 2);
    float y = -1.0 + float((gl_VertexID & 2) << 1);
    gl_Position = vec4(x, y, 0, 1);
  }`;

const INTERVAL = 1000 / 120;

export const BUFFER_NAMES = [
  "bufferA",
  "bufferB",
  "bufferC",
  "bufferD",
] as const;

export type BufferName = (typeof BUFFER_NAMES)[number];
export type PassName = BufferName | "image";

export const PASS_ORDER: PassName[] = [...BUFFER_NAMES, "image"];

export interface PassSpec {
  name: PassName;
  glsl: string;
}

interface Pass {
  name: PassName;
  program: WebGLProgram;
}

// A double-buffered float render target
interface Target {
  tex: [WebGLTexture, WebGLTexture];
  fbo: [WebGLFramebuffer, WebGLFramebuffer];
}

let gl: WebGL2RenderingContext;
let passes: Pass[] = [];
const targets = new Map<BufferName, Target>();
let blackTexture: WebGLTexture | null = null;
let cur = 0;
let frame = 0;

let mouseX = 0;
let mouseY = 0;
let mouseDown = 0;
let lastTime = 0;

function compileShader(source: string, type: number): WebGLShader | string {
  const s = gl.createShader(type)!;
  gl.shaderSource(s, source);
  gl.compileShader(s);
  if (!gl.getShaderParameter(s, gl.COMPILE_STATUS)) {
    return gl.getShaderInfoLog(s) ?? "Unknown shader error";
  }
  return s;
}

function compileProgram(fragSource: string): WebGLProgram | string {
  const vs = compileShader(VS_SOURCE, gl.VERTEX_SHADER);
  if (typeof vs === "string") return vs;
  const fs = compileShader(fragSource, gl.FRAGMENT_SHADER);
  if (typeof fs === "string") return fs;

  const p = gl.createProgram()!;
  gl.attachShader(p, vs);
  gl.attachShader(p, fs);
  gl.linkProgram(p);
  if (!gl.getProgramParameter(p, gl.LINK_STATUS)) {
    return gl.getProgramInfoLog(p) ?? "Unknown link error";
  }
  return p;
}

function createTarget(width: number, height: number): Target {
  const makeTex = (): WebGLTexture => {
    const tex = gl.createTexture()!;
    gl.bindTexture(gl.TEXTURE_2D, tex);
    gl.texImage2D(
      gl.TEXTURE_2D,
      0,
      gl.RGBA16F,
      width,
      height,
      0,
      gl.RGBA,
      gl.HALF_FLOAT,
      null,
    );
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    return tex;
  };
  const makeFbo = (tex: WebGLTexture): WebGLFramebuffer => {
    const fbo = gl.createFramebuffer()!;
    gl.bindFramebuffer(gl.FRAMEBUFFER, fbo);
    gl.framebufferTexture2D(
      gl.FRAMEBUFFER,
      gl.COLOR_ATTACHMENT0,
      gl.TEXTURE_2D,
      tex,
      0,
    );
    return fbo;
  };
  const t0 = makeTex();
  const t1 = makeTex();
  return { tex: [t0, t1], fbo: [makeFbo(t0), makeFbo(t1)] };
}

function destroyTargets(): void {
  for (const t of targets.values()) {
    gl.deleteTexture(t.tex[0]);
    gl.deleteTexture(t.tex[1]);
    gl.deleteFramebuffer(t.fbo[0]);
    gl.deleteFramebuffer(t.fbo[1]);
  }
  targets.clear();
}

// Allocate a ping-pong target for every buffer
function allocateTargets(): void {
  destroyTargets();
  const w = gl.canvas.width;
  const h = gl.canvas.height;
  for (const pass of passes) {
    if (pass.name !== "image" && !targets.has(pass.name)) {
      targets.set(pass.name, createTarget(w, h));
    }
  }
  frame = 0;
  cur = 0;
}

/** Compile link and wire up all passes, returns an error string or null on success.
 *  Buffer passes render to ping-pong float framebuffers.
 *  Image pass renders to the screen. */
export function loadPasses(specs: PassSpec[]): string | null {
  const linked: Pass[] = [];
  for (const spec of specs) {
    const program = compileProgram(spec.glsl);
    if (typeof program === "string") {
      return `${spec.name}: ${program}`;
    }
    linked.push({ name: spec.name, program });
  }

  for (const p of passes) {
    gl.deleteProgram(p.program);
  }

  passes = linked.sort(
    (a, b) => PASS_ORDER.indexOf(a.name) - PASS_ORDER.indexOf(b.name),
  );
  allocateTargets();
  return null;
}

function setStandardUniforms(program: WebGLProgram, time: number): void {
  const loc = (name: string) => gl.getUniformLocation(program, name);
  const res = loc("u_resolution");
  if (res) gl.uniform2f(res, gl.canvas.width, gl.canvas.height);
  const mouse = loc("u_mouse");
  if (mouse) gl.uniform2f(mouse, mouseX, mouseY);
  const t = loc("u_time");
  if (t) gl.uniform1f(t, time / 1000);
  const f = loc("iFrame");
  if (f) gl.uniform1i(f, frame);
  const md = loc("u_mouse_down");
  if (md) gl.uniform1i(md, mouseDown);
}

function render(currentTime: number): void {
  requestAnimationFrame(render);

  const delta = currentTime - lastTime;
  if (delta < INTERVAL || passes.length === 0) return;
  lastTime = currentTime - (delta % INTERVAL);

  const w = gl.canvas.width;
  const h = gl.canvas.height;
  const read = cur;
  const write = 1 - cur;

  for (const pass of passes) {
    const target = targets.get(pass.name as BufferName);
    gl.bindFramebuffer(gl.FRAMEBUFFER, target ? target.fbo[write] : null);
    gl.viewport(0, 0, w, h);
    gl.useProgram(pass.program);
    setStandardUniforms(pass.program, currentTime);

    let unit = 0;
    for (const buf of BUFFER_NAMES) {
      const loc = gl.getUniformLocation(pass.program, buf);
      if (!loc) continue;
      const src = targets.get(buf);
      gl.activeTexture(gl.TEXTURE0 + unit);
      gl.bindTexture(gl.TEXTURE_2D, src ? src.tex[read] : blackTexture);
      gl.uniform1i(loc, unit);
      unit++;
    }

    gl.drawArrays(gl.TRIANGLES, 0, 3);
  }

  cur = write;
  frame++;
}

export function initRenderer(canvas: HTMLCanvasElement): void {
  const container = canvas.parentElement!;
  gl = canvas.getContext("webgl2")!;

  // Required to render into RGBA16F float framebuffers
  if (!gl.getExtension("EXT_color_buffer_float")) {
    console.error("EXT_color_buffer_float unavailable; buffers will not work.");
  }

  // A 1x1 black texture for channels that are unrouted
  blackTexture = gl.createTexture();
  gl.bindTexture(gl.TEXTURE_2D, blackTexture);
  gl.texImage2D(
    gl.TEXTURE_2D,
    0,
    gl.RGBA,
    1,
    1,
    0,
    gl.RGBA,
    gl.UNSIGNED_BYTE,
    new Uint8Array([0, 0, 0, 255]),
  );

  const resizeObserver = new ResizeObserver((entries) => {
    for (const entry of entries) {
      const size = Math.min(entry.contentRect.width, entry.contentRect.height);
      if (size === canvas.width) continue;
      canvas.width = size;
      canvas.height = size;
      canvas.style.width = size + "px";
      canvas.style.height = size + "px";
      // Feedback history is invalid at a new resolution.
      if (passes.length > 0) {
        allocateTargets();
      }
    }
  });
  resizeObserver.observe(container);

  const updateMouse = (e: MouseEvent) => {
    const rect = canvas.getBoundingClientRect();
    mouseX = e.clientX - rect.left;
    mouseY = rect.height - (e.clientY - rect.top);
  };
  canvas.addEventListener("mousemove", updateMouse);
  canvas.addEventListener("mousedown", (e) => {
    updateMouse(e);
    mouseDown = 1;
  });
  // Listen on window so releasing outside the canvas still clears the flag.
  window.addEventListener("mouseup", () => (mouseDown = 0));

  requestAnimationFrame((time) => {
    lastTime = time;
    render(time);
  });
}
