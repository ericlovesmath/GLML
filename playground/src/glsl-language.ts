import { StreamLanguage, syntaxHighlighting } from "@codemirror/language";
import { catppuccinStyle } from "./glml-language";
import type { Extension } from "@codemirror/state";
import type { StringStream } from "@codemirror/language";

// ---------------------------------------------------------------------------
// GLSL tokenizer
// ---------------------------------------------------------------------------
const KEYWORDS = new Set([
  "if", "else", "for", "while", "do",
  "return", "break", "continue", "discard", "struct",
  "uniform", "out", "in", "inout", "const", "precision",
  "layout", "attribute", "varying",
]);

const TYPES = new Set([
  "void", "bool", "int", "uint", "float", "double",
  "vec2", "vec3", "vec4",
  "ivec2", "ivec3", "ivec4",
  "uvec2", "uvec3", "uvec4",
  "bvec2", "bvec3", "bvec4",
  "dvec2", "dvec3", "dvec4",
  "mat2", "mat3", "mat4",
  "mat2x2", "mat2x3", "mat2x4",
  "mat3x2", "mat3x3", "mat3x4",
  "mat4x2", "mat4x3", "mat4x4",
  "sampler2D", "sampler3D", "samplerCube",
  "sampler2DShadow", "samplerCubeShadow",
  "sampler2DArray", "sampler2DArrayShadow",
  "isampler2D", "isampler3D", "isamplerCube", "isampler2DArray",
  "usampler2D", "usampler3D", "usamplerCube", "usampler2DArray",
]);

const BUILTINS = new Set([
  "sin", "cos", "tan", "asin", "acos", "atan",
  "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
  "pow", "exp", "log", "exp2", "log2",
  "sqrt", "inversesqrt",
  "abs", "sign", "floor", "trunc", "round", "roundEven", "ceil", "fract",
  "mod", "modf", "min", "max", "clamp", "mix", "step", "smoothstep",
  "isnan", "isinf",
  "floatBitsToInt", "floatBitsToUint", "intBitsToFloat", "uintBitsToFloat",
  "packSnorm2x16", "unpackSnorm2x16", "packUnorm2x16", "unpackUnorm2x16",
  "packHalf2x16", "unpackHalf2x16",
  "length", "distance", "dot", "cross", "normalize",
  "faceforward", "reflect", "refract",
  "matrixCompMult", "outerProduct", "transpose", "determinant", "inverse",
  "lessThan", "lessThanEqual", "greaterThan", "greaterThanEqual", "equal", "notEqual",
  "any", "all", "not",
  "texture", "textureLod", "textureOffset", "texelFetch",
  "textureSize", "textureProj", "textureProjLod",
  "dFdx", "dFdy", "fwidth",
  "emit", "endPrimitive",
]);

const BOOLEANS = new Set(["true", "false"]);

function glslToken(stream: StringStream, state: { inBlock: boolean }): string | null {
  // Inside block comment
  if (state.inBlock) {
    if (stream.match("*/")) {
      state.inBlock = false;
    } else {
      stream.next();
    }
    return "comment";
  }

  // Line comment
  if (stream.match("//")) {
    stream.skipToEnd();
    return "comment";
  }

  // Block comment start
  if (stream.match("/*")) {
    state.inBlock = true;
    while (!stream.eol()) {
      if (stream.match("*/")) { state.inBlock = false; break; }
      stream.next();
    }
    return "comment";
  }

  // Preprocessor directive: #version, #define, #ifdef, #endif, etc.
  if (stream.peek() === "#") {
    stream.next();
    stream.match(/^[a-zA-Z_]\w*/); // consume directive name if present
    return "meta";
  }

  // Numbers: hex, float, int
  if (stream.match(/^0x[0-9a-fA-F]+u?/)) return "number";
  if (stream.match(/^[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?[fF]?/)) return "number";
  if (stream.match(/^[0-9]+\.([eE][+-]?[0-9]+)?[fF]?/)) return "number";
  if (stream.match(/^[0-9]+([eE][+-]?[0-9]+)?[uU]?/)) return "number";

  // Identifiers and keywords
  const word = stream.match(/^[a-zA-Z_]\w*/);
  if (word) {
    const s = typeof word === "string" ? word : (word as RegExpMatchArray)[0];
    if (KEYWORDS.has(s)) return "keyword";
    if (BOOLEANS.has(s)) return "atom";
    if (TYPES.has(s)) return "type";
    if (BUILTINS.has(s)) return "builtin";
    return null;
  }

  // Multi-char operators
  if (stream.match(/^(\+\+|--|[+\-*/%]=|==|!=|<=|>=|&&|\|\||<<|>>|[~!&|^])/)) return "operator";
  // Single-char operators
  if (stream.match(/^[=+\-*/%<>]/)) return "operator";

  // Punctuation
  if (stream.match(/^[{}()[\].,;:]/)) return "punctuation";

  stream.next();
  return null;
}

const glslStreamLanguage = StreamLanguage.define<{ inBlock: boolean }>({
  startState: () => ({ inBlock: false }),
  token: (stream, state) => glslToken(stream, state),
  languageData: { commentTokens: { line: "//", block: { open: "/*", close: "*/" } } },
});

export const glslExtension: Extension[] = [
  glslStreamLanguage,
  syntaxHighlighting(catppuccinStyle),
];
