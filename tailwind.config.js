import type { Config } from 'tailwindcss'

export default {
  content: [
    "./index.html",
    "./src/**/*.{js,ts,vue,jsx,tsx,css,md,mdx,html,json}",
  ],
  theme: {
    extend: {},
  },
  plugins: [],
} satisfies Config