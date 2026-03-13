import { initRenderer, compileAndLinkGLSL } from "./renderer";

const GLSL_OUT = document.getElementById("glsl-output") as HTMLTextAreaElement;
const ERROR_OUT = document.getElementById("error-output") as HTMLDivElement;
const INPUT = document.getElementById("glml-input") as HTMLTextAreaElement;
const COMPILE = document.getElementById("compile-btn") as HTMLButtonElement;

const DEFAULT_EXAMPLE = `#extern vec2 u_resolution
#extern float u_time

let get_uv (coord : vec2) =
  let top = 2.0 * coord - u_resolution in
  let bot = #min(u_resolution.0, u_resolution.1) in
  top / bot

let main (coord : vec2) =
  let uv = get_uv coord in
  let wave = 5.0 * (uv.0 + uv.1) + u_time in
  let r = #sin(wave) * 0.3 + 0.7 in
  let g = #sin(wave + 2.0) * 0.3 + 0.7 in
  let b = #sin(wave + 4.0) * 0.3 + 0.7 in
  [ r, g, b ]
`;

function compile(source: string): void {
  const result = window.glml.compile(source);
  if (result.glsl !== null) {
    GLSL_OUT.value = result.glsl;
    ERROR_OUT.textContent = "";

    const glsl_error = compileAndLinkGLSL(result.glsl);
    if (glsl_error !== null) {
      ERROR_OUT.textContent = "WebGL: " + glsl_error;
    }
  } else {
    GLSL_OUT.value = "";
    ERROR_OUT.textContent = result.error ?? "Unknown error";
  }
}

function main(): void {
  const canvas = document.getElementById("gl-canvas") as HTMLCanvasElement;
  initRenderer(canvas);

  INPUT.value = DEFAULT_EXAMPLE;

  COMPILE.addEventListener("click", () => {
    compile(INPUT.value);
  });

  compile(INPUT.value);
}

main();
