import * as monaco from "monaco-editor/esm/vs/editor/editor.api";

export function registerCatppuccin(): void {
  monaco.editor.defineTheme("catppuccin", {
    base: "vs-dark",
    inherit: true,
    rules: [
      { token: "comment", foreground: "6c7086", fontStyle: "italic" },
      { token: "keyword", foreground: "cba6f7" },
      { token: "type", foreground: "89dceb" },
      { token: "constant", foreground: "f38ba8" },
      { token: "number", foreground: "fab387" },
      { token: "builtin", foreground: "89b4fa" },
      { token: "typevar", foreground: "94e2d5" },
      { token: "operator", foreground: "89b4fa" },
      { token: "delimiter", foreground: "cdd6f4" },
    ],
    colors: {
      "editor.background": "#1e1e2e",
      "editor.foreground": "#cdd6f4",
      "editor.lineHighlightBackground": "#313244",
      "editor.selectionBackground": "#45475a",
      "editorLineNumber.foreground": "#585b70",
      "editorLineNumber.activeForeground": "#b4befe",
      "editorCursor.foreground": "#f5c2e7",
    },
  });
}

export function registerGLSL(): void {
  monaco.languages.register({ id: "glsl" });

  monaco.languages.setMonarchTokensProvider("glsl", {
    keywords: [
      "if",
      "else",
      "for",
      "while",
      "return",
      "continue",
      "break",
      "struct",
      "precision",
    ],
    qualifiers: ["uniform", "out", "const", "in", "inout"],
    types: [
      "float",
      "int",
      "bool",
      "void",
      "vec2",
      "vec3",
      "vec4",
      "mat2",
      "mat3",
      "mat4",
      "mat2x2",
      "mat2x3",
      "mat2x4",
      "mat3x2",
      "mat3x3",
      "mat3x4",
      "mat4x2",
      "mat4x3",
      "mat4x4",
    ],
    builtins: [
      "sin",
      "cos",
      "tan",
      "asin",
      "acos",
      "atan",
      "pow",
      "exp",
      "log",
      "exp2",
      "log2",
      "sqrt",
      "abs",
      "sign",
      "floor",
      "ceil",
      "fract",
      "min",
      "max",
      "clamp",
      "mix",
      "length",
      "distance",
      "dot",
      "cross",
      "normalize",
      "step",
      "smoothstep",
      "reflect",
      "mod",
    ],
    constants: ["true", "false"],
    tokenizer: {
      root: [
        [/\/\/.*$/, "comment"],
        [/#\w+/, "keyword"],
        [/\d+\.\d*|\d+/, "number"],
        [
          /[a-zA-Z_]\w*/,
          {
            cases: {
              "@qualifiers": "keyword",
              "@keywords": "keyword",
              "@types": "type",
              "@builtins": "builtin",
              "@constants": "constant",
              "@default": "identifier",
            },
          },
        ],
        [/==|!=|<=|>=|&&|\|\|/, "operator"],
        [/[+\-*\/%<>=!&|]/, "operator"],
        [/[(){}[\].,;:]/, "delimiter"],
      ],
    },
  });
}

export function registerGLML(): void {
  monaco.languages.register({ id: "glml" });

  monaco.languages.setMonarchTokensProvider("glml", {
    keywords: [
      "let",
      "rec",
      "fun",
      "in",
      "if",
      "then",
      "else",
      "match",
      "with",
      "type",
      "of",
    ],
    constants: ["true", "false"],
    types: [
      "bool",
      "int",
      "float",
      "vec2",
      "vec3",
      "vec4",
      "mat2",
      "mat2x2",
      "mat2x3",
      "mat2x4",
      "mat3",
      "mat3x2",
      "mat3x3",
      "mat3x4",
      "mat4",
      "mat4x2",
      "mat4x3",
      "mat4x4",
    ],
    tokenizer: {
      root: [
        [/\/\/.*$/, "comment"],
        [/#extern\b/, "keyword"],
        [/#[a-z]\w*/, "builtin"],
        [/'[a-zA-Z]\w*/, "typevar"],
        [/[A-Z]\w*/, "constant"], // Variants
        [
          /[a-z_]\w*/,
          {
            cases: {
              "@keywords": "keyword",
              "@constants": "constant",
              "@types": "type",
              "@default": "identifier",
            },
          },
        ],

        [/\d+\.?\d*/, "number"],
        [/->|<=|>=|==|&&|\|\|/, "operator"],
        [/\|/, "operator"],
        [/[+\-*/%<>=]/, "operator"],
        [/[(){}[\].,;:']/, "delimiter"],
      ],
    },
  });
}
