// @ts-check
import { defineConfig } from "vite";
import mpaPackage from "vite-plugin-mpa";
import elmPlugin from "vite-plugin-elm";

const { default: mpa } = mpaPackage;

export default defineConfig({
  plugins: [
    mpa({
      scanDir: "ui/pages",
      rewrite: [{ from: /^\/$/, to: "/ui/pages/index.html" }],
    }),
    elmPlugin(),
  ],
});
