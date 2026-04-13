import LZString from "lz-string";
import { inject } from "@vercel/analytics";
import { EditorView, basicSetup } from "codemirror";
import { keymap } from "@codemirror/view";
import { Compartment, Prec } from "@codemirror/state";
import { toggleLineComment } from "@codemirror/commands";
import { vim, getCM, Vim } from "@replit/codemirror-vim";
import { initRenderer, compileAndLinkGLSL } from "./renderer";
import { EXAMPLES } from "./examples";
import { glmlExtension } from "./glml-language";
import { glslExtension } from "./glsl-language";

const ERROR_OUT = document.getElementById("error-output") as HTMLDivElement;
const COMPILE = document.getElementById("compile-btn") as HTMLButtonElement;
const EXPORT = document.getElementById("export-btn") as HTMLButtonElement;
const EXPORT_POPOVER = document.getElementById(
  "export-popover",
) as HTMLSpanElement;
const SELECT = document.getElementById("example-select") as HTMLSelectElement;
const VIM_TOGGLE = document.getElementById("vim-toggle") as HTMLInputElement;
const VIM_STATUS = document.getElementById("vim-status")!;

inject();

const canvas = document.getElementById("gl-canvas") as HTMLCanvasElement;
initRenderer(canvas);

const darkTheme = EditorView.theme(
  {
    "&": {
      backgroundColor: "#1e1e2e",
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
      backgroundColor: "#1e1e2e",
      color: "#45475a",
      border: "none",
    },
    ".cm-activeLineGutter": {
      backgroundColor: "transparent",
      color: "#b4befe",
    },
    ".cm-activeLine": { backgroundColor: "transparent" },
    ".cm-selectionBackground, ::selection": {
      backgroundColor: "#585b70 !important",
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

const hashCode = (() => {
  const hash = window.location.hash.slice(1);
  if (!hash) return null;
  return LZString.decompressFromEncodedURIComponent(hash) || null;
})();

const inputView = new EditorView({
  doc: hashCode ?? EXAMPLES[0][1],
  extensions: [
    vimCompartment.of([]),
    basicSetup,
    darkTheme,
    ...glmlExtension,
    vimStatusListener,
    Prec.highest(
      keymap.of([
        {
          key: "Ctrl-Enter",
          mac: "Cmd-Enter",
          run: () => {
            glmlReady(() => compile(inputView.state.doc.toString()));
            return true;
          },
        },
        {
          key: "Ctrl-s",
          mac: "Cmd-s",
          run: () => {
            glmlReady(() => compile(inputView.state.doc.toString()));
            return true;
          },
        },
      ]),
    ),
  ],
  parent: document.getElementById("glml-input")!,
});

const outputView = new EditorView({
  doc: "",
  extensions: [
    basicSetup,
    darkTheme,
    ...glslExtension,
    EditorView.editable.of(false),
  ],
  parent: document.getElementById("glsl-output")!,
});

function setContent(view: EditorView, text: string): void {
  view.dispatch({
    changes: { from: 0, to: view.state.doc.length, insert: text },
  });
}

function glmlReady(cb: () => void): void {
  if (window.glml) {
    cb();
  } else {
    const check = setInterval(() => {
      if (window.glml) {
        clearInterval(check);
        cb();
      }
    }, 50);
  }
}

Vim.defineEx("write", "w", () => {
  glmlReady(() => compile(inputView.state.doc.toString()));
});

// gcc in normal mode / gc in visual mode: toggle line comments
Vim.defineAction("toggleComment", (cm) => {
  toggleLineComment(cm.cm6);
  if (cm.state.vim?.visualMode) {
    Vim.exitVisualMode(cm);
  }
});
Vim.mapCommand("gcc", "action", "toggleComment", {}, { context: "normal" });
Vim.mapCommand("gc", "action", "toggleComment", {}, { context: "visual" });

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
  glmlReady(() => compile(source));
});

COMPILE.addEventListener("click", () => {
  glmlReady(() => compile(inputView.state.doc.toString()));
});

let popoverTimer: ReturnType<typeof setTimeout> | null = null;

EXPORT.addEventListener("click", () => {
  const code = inputView.state.doc.toString();
  const compressed = LZString.compressToEncodedURIComponent(code);
  window.location.hash = compressed;
  const url = window.location.href;
  navigator.clipboard.writeText(url).then(() => {
    EXPORT_POPOVER.classList.add("visible");
    if (popoverTimer !== null) clearTimeout(popoverTimer);
    popoverTimer = setTimeout(() => {
      EXPORT_POPOVER.classList.remove("visible");
      popoverTimer = null;
    }, 1500);
  });
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

glmlReady(() => compile(hashCode ?? EXAMPLES[0][1]));
