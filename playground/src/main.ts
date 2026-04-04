import { inject } from "@vercel/analytics";
import { EditorView, basicSetup } from "codemirror";
import { keymap } from "@codemirror/view";
import { Compartment } from "@codemirror/state";
import { vim, getCM } from "@replit/codemirror-vim";
import { initRenderer, compileAndLinkGLSL } from "./renderer";
import { EXAMPLES } from "./examples";

const ERROR_OUT = document.getElementById("error-output") as HTMLDivElement;
const COMPILE = document.getElementById("compile-btn") as HTMLButtonElement;
const SELECT = document.getElementById("example-select") as HTMLSelectElement;
const VIM_TOGGLE = document.getElementById("vim-toggle") as HTMLInputElement;
const VIM_STATUS = document.getElementById("vim-status")!;

inject();

const canvas = document.getElementById("gl-canvas") as HTMLCanvasElement;
initRenderer(canvas);

const darkTheme = EditorView.theme(
  {
    "&": {
      backgroundColor: "#16162a",
      color: "#cdd6f4",
      height: "100%",
    },
    ".cm-scroller": {
      overflow: "auto",
      fontFamily:
        'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace',
      fontSize: "13px",
    },
    ".cm-gutters": {
      backgroundColor: "#16162a",
      color: "#585b70",
      border: "none",
    },
    ".cm-activeLineGutter": {
      backgroundColor: "#313244",
      color: "#b4befe",
    },
    ".cm-activeLine": { backgroundColor: "#313244" },
    ".cm-selectionBackground, ::selection": {
      backgroundColor: "#45475a !important",
    },
    ".cm-cursor": { borderLeftColor: "#f5c2e7" },
  },
  { dark: true },
);

const vimCompartment = new Compartment();

const vimStatusListener = EditorView.updateListener.of((update) => {
  if (!VIM_TOGGLE.checked) return;
  const cm = getCM(update.view);
  if (cm?.state?.vim) {
    const mode: string = cm.state.vim.mode ?? "normal";
    VIM_STATUS.textContent = "-- " + mode.toUpperCase() + " --";
  }
});

const inputView = new EditorView({
  doc: EXAMPLES[0][1],
  extensions: [
    basicSetup,
    darkTheme,
    vimCompartment.of([]),
    vimStatusListener,
    keymap.of([
      {
        key: "Ctrl-Enter",
        mac: "Cmd-Enter",
        run: () => {
          compile(inputView.state.doc.toString());
          return true;
        },
      },
    ]),
  ],
  parent: document.getElementById("glml-input")!,
});

const outputView = new EditorView({
  doc: "",
  extensions: [basicSetup, darkTheme, EditorView.editable.of(false)],
  parent: document.getElementById("glsl-output")!,
});

function setContent(view: EditorView, text: string): void {
  view.dispatch({
    changes: { from: 0, to: view.state.doc.length, insert: text },
  });
}

function compile(source: string): void {
  const result = window.glml.compile(source);
  if (result.glsl !== null) {
    setContent(outputView, result.glsl);
    ERROR_OUT.textContent = "";
    const glsl_error = compileAndLinkGLSL(result.glsl);
    if (glsl_error !== null) {
      ERROR_OUT.textContent = "WebGL: " + glsl_error;
    }
  } else {
    setContent(outputView, "");
    ERROR_OUT.textContent = result.error ?? "Unknown error";
  }
}

for (const [name] of EXAMPLES) {
  const opt = document.createElement("option");
  opt.textContent = name;
  SELECT.appendChild(opt);
}

SELECT.addEventListener("change", () => {
  const source = EXAMPLES[SELECT.selectedIndex][1];
  setContent(inputView, source);
  compile(source);
});

COMPILE.addEventListener("click", () => {
  compile(inputView.state.doc.toString());
});

const savedVim = localStorage.getItem("vimMode") === "true";
VIM_TOGGLE.checked = savedVim;

if (savedVim) {
  inputView.dispatch({ effects: vimCompartment.reconfigure(vim()) });
}

VIM_TOGGLE.addEventListener("change", () => {
  localStorage.setItem("vimMode", VIM_TOGGLE.checked ? "true" : "false");
  if (VIM_TOGGLE.checked) {
    inputView.dispatch({ effects: vimCompartment.reconfigure(vim()) });
  } else {
    inputView.dispatch({ effects: vimCompartment.reconfigure([]) });
    VIM_STATUS.textContent = "";
  }
});

compile(EXAMPLES[0][1]);
