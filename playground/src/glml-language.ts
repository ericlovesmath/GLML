import { StreamLanguage, syntaxHighlighting, HighlightStyle } from "@codemirror/language";
import { tags as t } from "@lezer/highlight";
import type { Extension } from "@codemirror/state";
import type { StringStream } from "@codemirror/language";

// ---------------------------------------------------------------------------
// Catppuccin Mocha HighlightStyle (shared, re-exported for glsl-language.ts)
//
// CM5 token name → Lezer tag (from @codemirror/language legacy mapping):
//   "comment"    → t.comment
//   "keyword"    → t.keyword
//   "atom"       → t.atom
//   "number"     → t.number
//   "type"       → t.typeName
//   "operator"   → t.operator
//   "punctuation"→ t.punctuation
//   "meta"       → t.meta                 ← preprocessor directives (#version etc.)
//   "qualifier"  → t.modifier             ← #extern
//   "builtin"    → t.standard(t.variableName)
//   "def"        → t.definition(t.variableName)
//   "variable-2" → t.special(t.variableName)
// ---------------------------------------------------------------------------
export const catppuccinStyle = HighlightStyle.define([
  { tag: t.comment,                      color: "#6c7086", fontStyle: "italic" }, // overlay0
  { tag: t.keyword,                      color: "#cba6f7" },                       // mauve
  { tag: t.modifier,                     color: "#f5c2e7" },                       // pink   (#extern)
  { tag: t.meta,                         color: "#cba6f7" },                       // mauve  (preprocessor)
  { tag: t.atom,                         color: "#f38ba8" },                       // green  (true/false)
  { tag: t.number,                       color: "#fab387" },                       // peach
  { tag: t.typeName,                     color: "#89dceb" },                       // sky
  { tag: t.operator,                     color: "#89b4fa" },                       // blue
  { tag: t.punctuation,                  color: "#cdd6f4" },                       // text
  { tag: t.standard(t.variableName),     color: "#89b4fa" },                       // blue   (#sin, #dot…)
  { tag: t.definition(t.variableName),   color: "#f38ba8" },                       // red   (Ctor names)
  { tag: t.special(t.variableName),      color: "#94e2d5" },                       // teal   ('a 'b)
  { tag: t.variableName,                 color: "#cdd6f4" },                       // text   (plain idents)
]);

// ---------------------------------------------------------------------------
// GLML tokenizer
// ---------------------------------------------------------------------------
const KEYWORDS = new Set([
  "let", "rec", "fun", "in", "if", "then", "else",
  "match", "with", "type", "of",
]);

const PRIMITIVES = new Set([
  "bool", "int", "float",
  "vec2", "vec3", "vec4",
  "mat2", "mat3", "mat4",
  "mat2x2", "mat2x3", "mat2x4",
  "mat3x2", "mat3x3", "mat3x4",
  "mat4x2", "mat4x3", "mat4x4",
]);

const BOOLEANS = new Set(["true", "false"]);

function glmlToken(stream: StringStream, _state: number): string | null {
  // Line comment
  if (stream.match("//")) {
    stream.skipToEnd();
    return "comment";
  }

  // Type variable  'a  'b  etc.
  if (stream.match(/^'[a-zA-Z_]\w*/)) return "variable-2";

  // #extern (declaration qualifier) vs. #ident builtins
  if (stream.peek() === "#") {
    stream.next();
    const word = stream.match(/^[a-zA-Z_]\w*/);
    if (word) {
      const s = typeof word === "string" ? word : (word as RegExpMatchArray)[0];
      return s === "extern" ? "qualifier" : "builtin";
    }
    return "operator";
  }

  // Number: optional minus, digits, optional decimal
  if (stream.match(/^-?[0-9]+\.?[0-9]*/)) return "number";

  // Identifiers and keywords
  const word = stream.match(/^[a-zA-Z_]\w*/);
  if (word) {
    const s = typeof word === "string" ? word : (word as RegExpMatchArray)[0];
    if (KEYWORDS.has(s)) return "keyword";
    if (BOOLEANS.has(s)) return "atom";
    if (PRIMITIVES.has(s)) return "type";
    if (/^[A-Z]/.test(s)) return "def";   // uppercase constructor
    return null; // plain identifier — no highlight
  }

  // Two-char operators first
  if (stream.match(/^(->|==|<=|>=|&&|\|\||!=)/)) return "operator";
  // Single-char operators
  if (stream.match(/^[|=+\-*/%<>]/)) return "operator";

  // Punctuation
  if (stream.match(/^[{}()[\].,;:]/)) return "punctuation";

  // Consume one character to avoid infinite loop
  stream.next();
  return null;
}

const glmlStreamLanguage = StreamLanguage.define<number>({
  startState: () => 0,
  token: (stream, state) => glmlToken(stream, state),
  languageData: { commentTokens: { line: "//" } },
});

export const glmlExtension: Extension[] = [
  glmlStreamLanguage,
  syntaxHighlighting(catppuccinStyle),
];
