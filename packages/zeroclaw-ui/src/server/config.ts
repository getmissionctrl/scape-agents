import path from 'path'
import os from 'os'

export const config = {
  port: parseInt(process.env.PORT || '5000', 10),
  gatewayUrl: process.env.GATEWAY_URL || 'ws://127.0.0.1:3000',
  botName: process.env.BOT_NAME || 'ZeroClaw',
  enableVoice: (process.env.ENABLE_VOICE || 'true').toLowerCase() === 'true',
  ttsCommand:
    process.env.TTS_COMMAND ||
    'edge-tts --text "{{TEXT}}" --write-media "{{OUTPUT}}"',
  dataDir: process.env.DATA_DIR || path.join(os.homedir(), '.zeroclaw-ui'),
  themeAccent: process.env.THEME_ACCENT || '6366f1',
} as const

/** Public config safe to send to the browser (no secrets). */
export const publicConfig = {
  botName: config.botName,
  enableVoice: config.enableVoice,
  themeAccent: config.themeAccent,
} as const
