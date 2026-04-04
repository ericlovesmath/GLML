import { defineConfig } from "vite";

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
});
