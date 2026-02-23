import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { renderHook, act } from '@testing-library/react'
import { useVoice } from '../../src/client/hooks/useVoice'

const mockRecognition = {
  start: vi.fn(),
  stop: vi.fn(),
  abort: vi.fn(),
  onresult: null as any,
  onend: null as any,
  onerror: null as any,
  continuous: false,
  interimResults: false,
  lang: '',
}

beforeEach(() => {
  vi.stubGlobal('SpeechRecognition', vi.fn(() => mockRecognition))
  vi.stubGlobal('webkitSpeechRecognition', vi.fn(() => mockRecognition))
})

afterEach(() => {
  vi.unstubAllGlobals()
})

describe('useVoice', () => {
  it('starts and stops listening', () => {
    const { result } = renderHook(() => useVoice({ onTranscript: () => {} }))

    act(() => result.current.startListening())
    expect(result.current.listening).toBe(true)
    expect(mockRecognition.start).toHaveBeenCalled()

    act(() => result.current.stopListening())
    expect(mockRecognition.stop).toHaveBeenCalled()
  })

  it('reports speech availability', () => {
    const { result } = renderHook(() => useVoice({ onTranscript: () => {} }))
    expect(result.current.available).toBe(true)
  })
})
