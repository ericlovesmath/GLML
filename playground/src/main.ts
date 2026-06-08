import LZString from "lz-string";
import { inject } from "@vercel/analytics";
import { EditorView, basicSetup } from "codemirror";
import { keymap } from "@codemirror/view";
import { Compartment, Prec, EditorState } from "@codemirror/state";
import { toggleLineComment, indentWithTab } from "@codemirror/commands";
import { indentUnit } from "@codemirror/language";
import { vim, getCM, Vim } from "@replit/codemirror-vim";
import {
  initRenderer,
  loadPasses,
  BUFFER_NAMES,
  PASS_ORDER,
  type PassSpec,
  type PassName,
} from "./renderer";
import {
  EXAMPLES,
  MULTIPASS_EXAMPLES,
  type MultipassExample,
} from "./examples";
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
const TAB_BAR = document.getElementById("tab-bar") as HTMLDivElement;

inject();

const canvas = document.getElementById("gl-canvas") as HTMLCanvasElement;
initRenderer(canvas);

const TAB_LABEL: Record<PassName, string> = {
  image: "Image",
  bufferA: "A",
  bufferB: "B",
  bufferC: "C",
  bufferD: "D",
};

type Sources = Record<PassName, string>;

const emptyState = (imageSource: string): Sources => ({
  image: imageSource,
  bufferA: "",
  bufferB: "",
  bufferC: "",
  bufferD: "",
});

const darkTheme = EditorView.theme(
  {
    "&": { backgroundColor: "#1e1e2e", color: "#cdd6f4", height: "100%" },
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
    ".cm-panels": { backgroundColor: "#1e1e2e", color: "#cdd6f4" },
    ".cm-panels input": {
      backgroundColor: "transparent",
      color: "#cdd6f4",
      outline: "none",
      border: "none",
    },
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

// On every edit, mirror the active tab into `sources` and re-render tab
let lastTabsSig = "";
const tabSyncListener = EditorView.updateListener.of((update) => {
  if (!update.docChanged) return;
  syncActiveSource();
  const sig = tabList()
    .map((t) => t.pass)
    .join();
  if (sig !== lastTabsSig) {
    lastTabsSig = sig;
    renderTabBar();
  }
});

const runCompile = (): boolean => {
  glmlReady(compileAll);
  return true;
};

function editorExtensions() {
  return [
    vimCompartment.of(VIM_TOGGLE.checked ? vim() : []),
    basicSetup,
    indentUnit.of("  "),
    keymap.of([indentWithTab]),
    darkTheme,
    ...glmlExtension,
    vimStatusListener,
    tabSyncListener,
    Prec.highest(
      keymap.of([
        { key: "Ctrl-Enter", mac: "Cmd-Enter", run: runCompile },
        { key: "Ctrl-s", mac: "Cmd-s", run: runCompile },
      ]),
    ),
  ];
}

const makeEditorState = (doc: string) =>
  EditorState.create({ doc, extensions: editorExtensions() });

// ======== State =========
let sources: Sources;
let activeTab: PassName = "image";
const tabStates = new Map<PassName, EditorState>();
const compiledGlsl: Partial<Record<PassName, string>> = {};

function loadInitialSources(): Sources {
  const hash = window.location.hash.slice(1);
  if (!hash) return emptyState(EXAMPLES[0][1]);
  const raw = LZString.decompressFromEncodedURIComponent(hash);
  if (!raw) return emptyState(EXAMPLES[0][1]);
  const parsed = JSON.parse(raw);
  if (parsed && typeof parsed === "object" && parsed.image !== undefined) {
    return { ...emptyState(""), ...parsed };
  }
  return emptyState(raw);
}

sources = loadInitialSources();

const inputView = new EditorView({
  state: makeEditorState(sources[activeTab]),
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

// ==== Tab strip ===

const syncActiveSource = () =>
  (sources[activeTab] = inputView.state.doc.toString());
const hasContent = (pass: PassName) => sources[pass].trim().length > 0;

const isOpen = (pass: PassName) =>
  pass === "image" || hasContent(pass) || activeTab === pass;

function tabList(): { pass: PassName; open: boolean }[] {
  const tabs = [{ pass: "image" as PassName, open: true }];
  for (const buf of BUFFER_NAMES) {
    if (isOpen(buf)) {
      tabs.push({ pass: buf, open: true });
    }
  }
  const next = BUFFER_NAMES.find((buf) => !isOpen(buf));
  if (next) {
    tabs.push({ pass: next, open: false });
  }
  return tabs;
}

function renderTabBar(): void {
  TAB_BAR.replaceChildren();
  for (const { pass, open } of tabList()) {
    const btn = document.createElement("button");
    btn.textContent = open ? TAB_LABEL[pass] : "+";
    if (pass === activeTab) btn.classList.add("active");
    if (!open) btn.classList.add("dim");
    btn.addEventListener("click", () => switchTab(pass));
    TAB_BAR.appendChild(btn);
  }
}

// Re-apply vim to the current editor state due to setState
function reapplyVim(): void {
  if (VIM_TOGGLE.checked) {
    inputView.dispatch({ effects: vimCompartment.reconfigure(vim()) });
  }
}

function switchTab(pass: PassName): void {
  if (pass === activeTab) return;
  syncActiveSource();
  tabStates.set(activeTab, inputView.state);
  activeTab = pass;
  inputView.setState(tabStates.get(pass) ?? makeEditorState(sources[pass]));
  reapplyVim();
  renderTabBar();
  setContent(outputView, compiledGlsl[pass] ?? "");
}

function setContent(view: EditorView, text: string): void {
  view.dispatch({
    changes: { from: 0, to: view.state.doc.length, insert: text },
  });
}

// ====== Compiling =====
function glmlReady(cb: () => void): void {
  if (window.glml) cb();
  else {
    const check = setInterval(() => {
      if (window.glml) {
        clearInterval(check);
        cb();
      }
    }, 50);
  }
}

function compileAll(): void {
  syncActiveSource();
  for (const key of Object.keys(compiledGlsl))
    delete compiledGlsl[key as PassName];
  const specs: PassSpec[] = [];
  for (const name of PASS_ORDER) {
    if (name !== "image" && !hasContent(name)) continue;
    const result = window.glml.compile(sources[name]);
    if (result.glsl === null) {
      ERROR_OUT.textContent = `[${TAB_LABEL[name]}] ${result.error ?? "Unknown error"}`;
      setContent(outputView, compiledGlsl[activeTab] ?? "");
      return;
    }
    compiledGlsl[name] = result.glsl;
    specs.push({ name, glsl: result.glsl });
  }
  ERROR_OUT.textContent = "";
  const glslError = loadPasses(specs);
  if (glslError !== null) ERROR_OUT.textContent = "WebGL: " + glslError;
  setContent(outputView, compiledGlsl[activeTab] ?? "");
}

// ===== Examples and Exports =====
function fromMultipass(m: MultipassExample): Sources {
  const s = emptyState(m.passes.image ?? "");
  for (const buf of BUFFER_NAMES) if (m.passes[buf]) s[buf] = m.passes[buf]!;
  return s;
}

const ALL_EXAMPLES: { name: string; load: () => Sources }[] = [
  ...EXAMPLES.map(([name, src]) => ({ name, load: () => emptyState(src) })),
  ...MULTIPASS_EXAMPLES.map((m) => ({
    name: `${m.name} (MP)`,
    load: () => fromMultipass(m),
  })),
];

for (const { name } of ALL_EXAMPLES) SELECT.add(new Option(name));

function loadPlayground(next: Sources): void {
  sources = next;
  tabStates.clear();
  activeTab = "image";
  inputView.setState(makeEditorState(sources.image));
  reapplyVim();
  renderTabBar();
  glmlReady(compileAll);
}

SELECT.addEventListener("change", () =>
  loadPlayground(ALL_EXAMPLES[SELECT.selectedIndex].load()),
);
COMPILE.addEventListener("click", () => glmlReady(compileAll));

let popoverTimer: ReturnType<typeof setTimeout> | null = null;
EXPORT.addEventListener("click", () => {
  syncActiveSource();
  window.location.hash = LZString.compressToEncodedURIComponent(
    JSON.stringify(sources),
  );
  navigator.clipboard.writeText(window.location.href).then(() => {
    EXPORT_POPOVER.classList.add("visible");
    if (popoverTimer !== null) clearTimeout(popoverTimer);
    popoverTimer = setTimeout(() => {
      EXPORT_POPOVER.classList.remove("visible");
      popoverTimer = null;
    }, 1500);
  });
});

Vim.defineEx("write", "w", () => glmlReady(compileAll));
Vim.defineAction("toggleComment", (cm) => {
  toggleLineComment(cm.cm6);
  if (cm.state.vim?.visualMode) Vim.exitVisualMode(cm);
});
Vim.mapCommand("gcc", "action", "toggleComment", {}, { context: "normal" });
Vim.mapCommand("gc", "action", "toggleComment", {}, { context: "visual" });

VIM_TOGGLE.checked = localStorage.getItem("vimMode") === "true";
reapplyVim();

VIM_TOGGLE.addEventListener("change", () => {
  localStorage.setItem("vimMode", VIM_TOGGLE.checked ? "true" : "false");
  inputView.dispatch({
    effects: vimCompartment.reconfigure(VIM_TOGGLE.checked ? vim() : []),
  });
  if (!VIM_TOGGLE.checked) VIM_STATUS.textContent = "";
});

renderTabBar();
glmlReady(compileAll);
