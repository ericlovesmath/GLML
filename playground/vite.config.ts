import { defineConfig } from "vite";

export default defineConfig({
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
});
