import { describe, it, expect, vi, beforeEach } from 'vitest'
import { cleanTextForTTS, buildTTSCommand } from '../../src/server/tts'

describe('cleanTextForTTS', () => {
  it('strips code blocks', () => {
    expect(cleanTextForTTS('hello ```code``` world')).toBe('hello code block world')
  })

  it('strips inline code', () => {
    expect(cleanTextForTTS('use `npm install`')).toBe('use')
  })

  it('extracts link text', () => {
    expect(cleanTextForTTS('see [docs](https://example.com)')).toBe('see docs')
  })

  it('strips markdown formatting', () => {
    expect(cleanTextForTTS('**bold** and *italic*')).toBe('bold and italic')
  })

  it('strips URLs', () => {
    expect(cleanTextForTTS('visit https://example.com now')).toBe('visit link now')
  })

  it('normalizes whitespace', () => {
    expect(cleanTextForTTS('  hello   world  ')).toBe('hello world')
  })
})

describe('buildTTSCommand', () => {
  it('replaces TEXT and OUTPUT placeholders', () => {
    const template = 'edge-tts --text "{{TEXT}}" --write-media "{{OUTPUT}}"'
    const cmd = buildTTSCommand(template, 'hello world', '/tmp/out.mp3')
    expect(cmd).toContain("'hello world'")
    expect(cmd).toContain('/tmp/out.mp3')
  })

  it('escapes single quotes in text', () => {
    const template = 'edge-tts --text "{{TEXT}}" --write-media "{{OUTPUT}}"'
    const cmd = buildTTSCommand(template, "it's fine", '/tmp/out.mp3')
    expect(cmd).toContain("'it'\\''s fine'")
  })
})
