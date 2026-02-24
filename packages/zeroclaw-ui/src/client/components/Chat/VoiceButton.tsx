import { Mic } from 'lucide-react'

export interface VoiceButtonProps {
  listening: boolean
  available: boolean
  onStart: () => void
  onStop: () => void
}

export function VoiceButton({ listening, available, onStart, onStop }: VoiceButtonProps) {
  if (!available) return null

  return (
    <button
      onClick={listening ? onStop : onStart}
      aria-label={listening ? 'Stop recording' : 'Start recording'}
      className={`w-10 h-10 rounded-full flex items-center justify-center transition-colors ${
        listening
          ? 'bg-accent-pink text-white animate-pulse shadow-glow-pink'
          : 'bg-bg-elevated text-fg-muted hover:bg-bg-surface hover:text-fg-primary'
      }`}
    >
      <Mic size={18} />
    </button>
  )
}
