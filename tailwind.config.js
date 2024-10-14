const colorKeys = [10, 20, 30, 40, 50, 60, 70, 80, 90, 95];

const grey = (shade) => "oklch(" + shade + "% 0.02 91)";

/** @type {import('tailwindcss').Config} */
export default {
  content: ["./ui/**/*.{elm,html}"],
  theme: {
    colors: {
      grey: colorKeys.reduce((colors, shade) => {
        colors[shade * 10] = grey(shade);
        return colors;
      }, {}),
      borderColor: grey(80),
      borderWidth: {
        DEFAULT: "0.125rem",
      },
    },
    extend: {},
  },
  plugins: [],
};
