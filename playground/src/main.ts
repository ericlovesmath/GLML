import * as monaco from "monaco-editor";
import { initRenderer, compileAndLinkGLSL } from "./renderer";
import { EXAMPLES } from "./examples";

self.MonacoEnvironment = {
  getWorker: function () {
    return new Worker(
      new URL("monaco-editor/esm/vs/editor/editor.worker.js", import.meta.url),
      { type: "module" },
    );
  },
};

const ERROR_OUT = document.getElementById("error-output") as HTMLDivElement;
const COMPILE = document.getElementById("compile-btn") as HTMLButtonElement;
const SELECT = document.getElementById("example-select") as HTMLSelectElement;

const EDITOR_OPTIONS: monaco.editor.IStandaloneEditorConstructionOptions = {
  theme: "vs-dark",
  minimap: { enabled: false },
  fontSize: 13,
  lineNumbers: "on",
  scrollBeyondLastLine: false,
  automaticLayout: true,
};

const inputEditor = monaco.editor.create(
  document.getElementById("glml-input")!,
  {
    ...EDITOR_OPTIONS,
    language: "plaintext",
  },
);

const outputEditor = monaco.editor.create(
  document.getElementById("glsl-output")!,
  {
    ...EDITOR_OPTIONS,
    // Monaco has no GLSL :(
    language: "c",
    readOnly: true,
  },
);

function compile(source: string): void {
  const result = window.glml.compile(source);
  if (result.glsl !== null) {
    outputEditor.setValue(result.glsl);
    ERROR_OUT.textContent = "";

    const glsl_error = compileAndLinkGLSL(result.glsl);
    if (glsl_error !== null) {
      ERROR_OUT.textContent = "WebGL: " + glsl_error;
    }
  } else {
    outputEditor.setValue("");
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
    inputEditor.setValue(source);
    compile(source);
  });

  COMPILE.addEventListener("click", () => {
    compile(inputEditor.getValue());
  });

  inputEditor.setValue(EXAMPLES[0][1]);
  compile(EXAMPLES[0][1]);
}

main();
