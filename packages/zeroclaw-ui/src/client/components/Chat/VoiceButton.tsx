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
          ? 'bg-red-600 text-white animate-pulse'
          : 'bg-gray-800 text-gray-400 hover:bg-gray-700 hover:text-gray-200'
      }`}
    >
      <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
        <path d="M12 2a3 3 0 0 0-3 3v7a3 3 0 0 0 6 0V5a3 3 0 0 0-3-3Z" />
        <path d="M19 10v2a7 7 0 0 1-14 0v-2" />
        <line x1="12" x2="12" y1="19" y2="22" />
      </svg>
    </button>
  )
}
