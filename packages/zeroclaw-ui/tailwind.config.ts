import type { Config } from 'tailwindcss'

export default {
  content: ['./index.html', './src/**/*.{ts,tsx}'],
  theme: {
    fontFamily: {
      sans: ['Inter', '-apple-system', 'BlinkMacSystemFont', 'sans-serif'],
      mono: ['JetBrains Mono', 'Fira Code', 'SF Mono', 'monospace'],
    },
    extend: {
      colors: {
        bg: {
          base: '#020D31',
          raised: '#081C43',
          elevated: '#0F2A5C',
          surface: '#143C69',
        },
        fg: {
          primary: '#FFFFFF',
          secondary: '#C9EBF0',
          muted: '#8A9BB8',
          subtle: '#5A6A8A',
        },
        accent: {
          cyan: '#73FDFF',
          pink: '#FF2F93',
          green: '#73FCD6',
          yellow: '#FFBD2E',
          red: '#FF5F56',
          blue: '#76D6FF',
        },
      },
      boxShadow: {
        'glow-cyan': '0 0 15px rgba(115, 253, 255, 0.3)',
        'glow-pink': '0 0 15px rgba(255, 47, 147, 0.3)',
        'glow-green': '0 0 8px rgba(115, 252, 214, 0.6)',
      },
      borderRadius: {
        sm: '4px',
        md: '6px',
        lg: '8px',
      },
    },
  },
  plugins: [],
} satisfies Config
