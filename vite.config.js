// @ts-check
import { defineConfig } from "vite";
import mpaPackage from "vite-plugin-mpa";
import elmPlugin from "vite-plugin-elm";

const { default: mpa } = mpaPackage;

export default defineConfig({
  plugins: [mpa({ scanDir: "ui/pages" }), elmPlugin()],
});
