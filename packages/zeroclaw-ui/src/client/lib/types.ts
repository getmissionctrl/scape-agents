export interface ChatMessage {
  id: string
  role: 'user' | 'bot'
  text: string
  timestamp: string
  images?: string[]
  widget?: WidgetData
  streaming?: boolean
}

export interface WidgetData {
  widget: string
  id: string
  [key: string]: unknown
}

export interface WidgetResponse {
  id: string
  widget: string
  value: unknown
  action: string
}

export interface GatewayMessage {
  type: string
  text?: string
  state?: 'streaming' | 'final' | 'cancelled'
  runId?: string
  error?: string
  images?: string[]
}

/** Generate a UUID, falling back to crypto.getRandomValues on non-secure contexts. */
export function uuid(): string {
  if (typeof crypto !== 'undefined' && typeof crypto.randomUUID === 'function') {
    return crypto.randomUUID()
  }
  const bytes = new Uint8Array(16)
  crypto.getRandomValues(bytes)
  bytes[6] = (bytes[6] & 0x0f) | 0x40
  bytes[8] = (bytes[8] & 0x3f) | 0x80
  const hex = [...bytes].map((b) => b.toString(16).padStart(2, '0')).join('')
  return `${hex.slice(0, 8)}-${hex.slice(8, 12)}-${hex.slice(12, 16)}-${hex.slice(16, 20)}-${hex.slice(20)}`
}

export interface PublicConfig {
  botName: string
  enableVoice: boolean
  themeAccent: string
}
