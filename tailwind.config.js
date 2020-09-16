module.exports = {
  future: {
    removeDeprecatedGapUtilities: true,
    purgeLayersByDefault: true,
  },
  purge: {
    // enabled: true,
    content: ["src/Main.elm"],
  },
  theme: {
    extend: {
      fontFamily: {
        'body': ['"Fira Sans"', 'sans-serif']
      }
    },
  },
  variants: {},
  plugins: [],
  experimental: 'all',
}
