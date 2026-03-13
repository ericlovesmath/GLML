import { initRenderer, compileAndLinkGLSL } from "./renderer";
import { EXAMPLES } from "./examples";

const GLSL_OUT = document.getElementById("glsl-output") as HTMLTextAreaElement;
const ERROR_OUT = document.getElementById("error-output") as HTMLDivElement;
const INPUT = document.getElementById("glml-input") as HTMLTextAreaElement;
const COMPILE = document.getElementById("compile-btn") as HTMLButtonElement;
const SELECT = document.getElementById("example-select") as HTMLSelectElement;

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

  for (const [name] of EXAMPLES) {
    const opt = document.createElement("option");
    opt.textContent = name;
    SELECT.appendChild(opt);
  }

  SELECT.addEventListener("change", () => {
    const source = EXAMPLES[SELECT.selectedIndex][1];
    INPUT.value = source;
    compile(source);
  });

  COMPILE.addEventListener("click", () => {
    compile(INPUT.value);
  });

  INPUT.value = EXAMPLES[0][1];
  compile(INPUT.value);
}

main();
