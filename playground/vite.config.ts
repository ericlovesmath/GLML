import { defineConfig } from "vite";
import monacoEditorPluginModule from "vite-plugin-monaco-editor";

// CJS/ESM interop: the plugin ships as CJS, so .default may be the actual function
const monacoEditorPlugin =
  (monacoEditorPluginModule as unknown as { default: typeof monacoEditorPluginModule }).default ??
  monacoEditorPluginModule;

export default defineConfig({
  base: "/playground",
  publicDir: "public",
  build: {
    outDir: "dist",
    emptyOutDir: true,
    chunkSizeWarningLimit: 3000,
  },
  server: {
    fs: {
      allow: [".", "../examples"],
    },
  },
  plugins: [
    monacoEditorPlugin({ languageWorkers: ["editorWorkerService"] }),
  ],
});
