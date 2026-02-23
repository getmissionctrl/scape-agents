import { exec } from 'child_process'
import crypto from 'crypto'
import fs from 'fs'
import path from 'path'
import { promisify } from 'util'
import { config } from './config'

const execAsync = promisify(exec)
const TTS_DIR = '/tmp/zeroclaw-tts'

// Ensure TTS directory exists
try { fs.mkdirSync(TTS_DIR, { recursive: true }) } catch { /* ignore */ }

export function cleanTextForTTS(text: string): string {
  return text
    .replace(/```[\s\S]*?```/g, ' code block ')
    .replace(/`[^`]+`/g, '')
    .replace(/\[([^\]]+)\]\([^)]+\)/g, '$1')
    .replace(/[#*_~>]/g, '')
    .replace(/https?:\/\/\S+/g, ' link ')
    .replace(/\s+/g, ' ')
    .trim()
}

export function buildTTSCommand(
  template: string,
  text: string,
  outputPath: string,
): string {
  const escaped = text.replace(/'/g, "'\\''")
  return template
    .replace(/"?\{\{TEXT\}\}"?/g, "'" + escaped + "'")
    .replace(/\{\{OUTPUT\}\}/g, outputPath)
}

export async function generateTTS(text: string): Promise<Buffer> {
  const clean = cleanTextForTTS(text).slice(0, 4000)
  if (!clean || clean.length < 3) {
    throw new Error('Text too short for TTS')
  }

  const id = crypto.randomBytes(8).toString('hex')
  const outputPath = path.join(TTS_DIR, `tts-${id}.mp3`)

  try {
    const cmd = buildTTSCommand(config.ttsCommand, clean, outputPath)
    await execAsync(cmd, { timeout: 30000 })
    const audio = fs.readFileSync(outputPath)
    return audio
  } finally {
    try { fs.unlinkSync(outputPath) } catch { /* ignore */ }
  }
}
