import { defineConfig } from 'vite';

export default defineConfig({
  publicDir: 'public',
  build: {
    outDir: '../dist/web',
    emptyOutDir: true,
  },
  server: {
    fs: {
      allow: ['.', '../examples'],
    },
  },
});
