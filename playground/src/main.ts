import * as monaco from "monaco-editor";
import { initVimMode } from "monaco-vim";
import { initRenderer, compileAndLinkGLSL } from "./renderer";
import { EXAMPLES } from "./examples";
import {
  registerCatppuccin,
  registerGLML,
  registerGLSL,
} from "./monaco-extensions";

registerCatppuccin();
registerGLML();
registerGLSL();

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
  theme: "catppuccin",
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
    language: "glml",
  },
);

const outputEditor = monaco.editor.create(
  document.getElementById("glsl-output")!,
  {
    ...EDITOR_OPTIONS,
    language: "glsl",
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

  inputEditor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter, () =>
    compile(inputEditor.getValue()),
  );

  const VIM_TOGGLE = document.getElementById("vim-toggle") as HTMLInputElement;
  const VIM_STATUS = document.getElementById("vim-status")!;
  const savedVim = localStorage.getItem("vimMode") === "true";
  VIM_TOGGLE.checked = savedVim;
  let vimMode: ReturnType<typeof initVimMode> | null = savedVim
    ? initVimMode(inputEditor, VIM_STATUS)
    : null;

  VIM_TOGGLE.addEventListener("change", () => {
    localStorage.setItem("vimMode", VIM_TOGGLE.checked ? "true" : "false");
    if (VIM_TOGGLE.checked) {
      vimMode = initVimMode(inputEditor, VIM_STATUS);
    } else {
      vimMode?.dispose();
      vimMode = null;
      VIM_STATUS.textContent = "";
    }
  });

  inputEditor.setValue(EXAMPLES[0][1]);
  compile(EXAMPLES[0][1]);
}

main();
