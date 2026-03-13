interface GlmlCompileResult {
  glsl: ?string;
  error: ?string;
}

interface GlmlCompiler {
  compile(source: string): GlmlCompileResult;
}

declare global {
  interface Window {
    glml: GlmlCompiler;
  }
  const glml: GlmlCompiler;
}

/// <reference types="vite/client" />

export {};
