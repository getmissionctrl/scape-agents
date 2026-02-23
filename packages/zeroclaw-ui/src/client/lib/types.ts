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

export interface PublicConfig {
  botName: string
  enableVoice: boolean
  themeAccent: string
}
