# OpenRouter OAuth PKCE Integration Guide for SPAs

This document walks through integrating OpenRouter's OAuth PKCE flow into a client-side Single Page Application (SPA) with no backend. The user authenticates with their own OpenRouter account and pays for all model usage directly — the app developer never handles billing, API keys, or secrets.

## Architecture Overview

The integration follows a standard OAuth 2.0 PKCE (Proof Key for Code Exchange) flow, purpose-built for public clients like SPAs where secrets cannot be stored safely.[^1]

The data flow is:

1. **SPA → OpenRouter**: Redirect user to OpenRouter's `/auth` page with a PKCE challenge.
2. **OpenRouter → User**: User logs in and authorizes the app.
3. **OpenRouter → SPA**: Redirect back with an authorization `code`.
4. **SPA → OpenRouter**: Exchange the `code` (+ verifier) for a **user-scoped API key**.
5. **SPA → OpenRouter API**: Use that key to call models. All costs are billed to the user's OpenRouter account.[^2]

There is no backend involved. The SPA communicates directly with OpenRouter's API endpoints.

## Prerequisites

- An OpenRouter account for development/testing (to verify the flow works).
- A deployed or localhost SPA with a known callback URL.
- Familiarity with the `fetch` API and `crypto.subtle` (Web Crypto API).[^1]

No API key from the developer is required. The key is generated per-user during the OAuth flow.[^1]

## Step 1: Generate PKCE Code Verifier and Challenge

Before redirecting the user, generate a `code_verifier` (a random string) and its SHA-256 hash as the `code_challenge`. This prevents authorization code interception attacks.[^1]

```typescript
import { Buffer } from 'buffer'; // Needed in browser; use a bundler

// Generate a random code verifier
function generateCodeVerifier(length = 64): string {
  const array = new Uint8Array(length);
  crypto.getRandomValues(array);
  return Buffer.from(array).toString('base64url');
}

// Hash it with SHA-256 for the challenge
async function createSHA256CodeChallenge(codeVerifier: string): Promise<string> {
  const encoder = new TextEncoder();
  const data = encoder.encode(codeVerifier);
  const hash = await crypto.subtle.digest('SHA-256', data);
  return Buffer.from(hash).toString('base64url');
}

// Usage
const codeVerifier = generateCodeVerifier();
const codeChallenge = await createSHA256CodeChallenge(codeVerifier);

// Store codeVerifier in sessionStorage — you'll need it in Step 3
sessionStorage.setItem('openrouter_code_verifier', codeVerifier);
```

The `code_challenge_method` must be set to `S256`.[^1]

## Step 2: Redirect the User to OpenRouter

Build the authorization URL and redirect the user:

```typescript
const CALLBACK_URL = 'https://your-app.com/callback'; // Or http://localhost:3000 for dev

const authUrl = new URL('https://openrouter.ai/auth');
authUrl.searchParams.set('callback_url', CALLBACK_URL);
authUrl.searchParams.set('code_challenge', codeChallenge);
authUrl.searchParams.set('code_challenge_method', 'S256');

window.location.href = authUrl.toString();
```

The user is taken to OpenRouter where they log in (or sign up) and authorize the app. After authorization, OpenRouter redirects back to the `callback_url` with a `code` query parameter.[^1]

For local development, use `http://localhost:3000` as the callback URL. When deploying to production, replace it with the public URL or a link to the project's GitHub repository.[^1]

## Step 3: Exchange the Code for a User API Key

On the callback page, extract the `code` from the URL and exchange it:

```typescript
// Extract the authorization code from the redirect
const urlParams = new URLSearchParams(window.location.search);
const code = urlParams.get('code');

if (!code) {
  throw new Error('No authorization code received from OpenRouter');
}

// Retrieve the code verifier from sessionStorage
const codeVerifier = sessionStorage.getItem('openrouter_code_verifier');
sessionStorage.removeItem('openrouter_code_verifier'); // Clean up

// Exchange code for API key
const response = await fetch('https://openrouter.ai/api/v1/auth/keys', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({
    code,
    code_verifier: codeVerifier,
    code_challenge_method: 'S256',
  }),
});

if (!response.ok) {
  const error = await response.json();
  throw new Error(`Key exchange failed: ${error.message || response.statusText}`);
}

const { key } = await response.json();

// Store the key — this is the user's API key, billed to their account
sessionStorage.setItem('openrouter_user_key', key);
```

This key is scoped to the user. All requests made with it are billed to the user's OpenRouter credits. Users can revoke it at any time from their OpenRouter dashboard.[^2]

## Step 4: Call a Model (Non-Streaming)

With the key in hand, make a standard chat completion request:

```typescript
const userKey = sessionStorage.getItem('openrouter_user_key');

const response = await fetch('https://openrouter.ai/api/v1/chat/completions', {
  method: 'POST',
  headers: {
    'Authorization': `Bearer ${userKey}`,
    'Content-Type': 'application/json',
    'HTTP-Referer': 'https://your-app.com',        // Optional: identifies your app
    'X-OpenRouter-Title': 'My AI App',              // Optional: app name on openrouter.ai
  },
  body: JSON.stringify({
    model: 'openai/gpt-4o',  // Any model from openrouter.ai/models
    messages: [
      { role: 'system', content: 'You are a helpful assistant.' },
      { role: 'user', content: 'Explain quantum entanglement in simple terms.' },
    ],
    max_tokens: 500,
    temperature: 0.7,
  }),
});

const data = await response.json();
console.log(data.choices.message.content);
```

The request and response schema is compatible with the OpenAI Chat Completions API. OpenRouter normalizes the schema across all providers.[^3]

### Key Headers

| Header | Required | Purpose |
|---|---|---|
| `Authorization` | Yes | `Bearer <user_key>` — the key from Step 3[^2] |
| `Content-Type` | Yes | `application/json` |
| `HTTP-Referer` | No | Your app URL; used for rankings on openrouter.ai[^3] |
| `X-OpenRouter-Title` | No | Your app's display name on openrouter.ai[^3] |

## Step 5: Streaming Responses

For a chat-like UX, enable streaming to receive tokens as they are generated:[^4]

```typescript
const userKey = sessionStorage.getItem('openrouter_user_key');

const response = await fetch('https://openrouter.ai/api/v1/chat/completions', {
  method: 'POST',
  headers: {
    'Authorization': `Bearer ${userKey}`,
    'Content-Type': 'application/json',
  },
  body: JSON.stringify({
    model: 'anthropic/claude-sonnet-4',
    messages: [{ role: 'user', content: 'Write a short story about a robot.' }],
    stream: true,
  }),
});

const reader = response.body!.getReader();
const decoder = new TextDecoder();

let buffer = '';
while (true) {
  const { done, value } = await reader.read();
  if (done) break;

  buffer += decoder.decode(value, { stream: true });
  const lines = buffer.split('\n');
  buffer = lines.pop()!; // Keep incomplete line in buffer

  for (const line of lines) {
    const trimmed = line.trim();

    // Skip empty lines and SSE comments (keep-alive pings)
    if (!trimmed || trimmed.startsWith(':')) continue;

    // Remove "data: " prefix
    if (!trimmed.startsWith('data: ')) continue;
    const payload = trimmed.slice(6);

    // End of stream
    if (payload === '[DONE]') return;

    try {
      const chunk = JSON.parse(payload);

      // Check for mid-stream errors
      if (chunk.error) {
        console.error('Stream error:', chunk.error.message);
        return;
      }

      const content = chunk.choices?.?.delta?.content;
      if (content) {
        // Append content to your UI element
        document.getElementById('output')!.textContent += content;
      }

      // Final chunk includes usage stats
      if (chunk.usage) {
        console.log('Tokens used:', chunk.usage.total_tokens);
        console.log('Cost:', chunk.usage.cost);
      }
    } catch (e) {
      // Ignore non-JSON payloads (SSE comments)
    }
  }
}
```

OpenRouter occasionally sends SSE comments like `: OPENROUTER PROCESSING` to prevent connection timeouts. These should be ignored per the SSE specification.[^4]

### Stream Cancellation

To cancel an in-progress stream, use an `AbortController`:[^4]

```typescript
const controller = new AbortController();

const response = await fetch('https://openrouter.ai/api/v1/chat/completions', {
  method: 'POST',
  headers: {
    'Authorization': `Bearer ${userKey}`,
    'Content-Type': 'application/json',
  },
  body: JSON.stringify({
    model: 'openai/gpt-4o',
    messages: [{ role: 'user', content: 'Write a long essay...' }],
    stream: true,
  }),
  signal: controller.signal,
});

// To cancel at any point:
controller.abort();
```

For supported providers (OpenAI, Anthropic, Azure, and others), aborting the connection immediately stops model processing and billing.[^4]

## Step 6: Using the OpenRouter SDK (Alternative)

Instead of raw `fetch`, the `@openrouter/sdk` npm package provides a typed client:[^5]

```bash
npm install @openrouter/sdk
```

```typescript
import { OpenRouter } from '@openrouter/sdk';

const userKey = sessionStorage.getItem('openrouter_user_key')!;

const client = new OpenRouter({
  apiKey: userKey,
  defaultHeaders: {
    'HTTP-Referer': 'https://your-app.com',
    'X-OpenRouter-Title': 'My AI App',
  },
});

// Non-streaming
const completion = await client.chat.send({
  model: 'openai/gpt-4o',
  messages: [{ role: 'user', content: 'Hello!' }],
  stream: false,
});
console.log(completion.choices.message.content);

// Streaming
const stream = await client.chat.send({
  model: 'openai/gpt-4o',
  messages: [{ role: 'user', content: 'Write a poem.' }],
  stream: true,
});

for await (const chunk of stream) {
  const content = chunk.choices?.?.delta?.content;
  if (content) process.stdout.write(content);
}
```

The Vercel AI SDK provider (`@openrouter/ai-sdk-provider`) is also available for projects using the Vercel AI SDK.[^6]

## Model Selection

OpenRouter provides access to 300+ models from all major providers through a single API. The `model` parameter uses the format `provider/model-name`.[^6]

Common models include:

| Model ID | Provider | Notes |
|---|---|---|
| `openai/gpt-4o` | OpenAI | Strong general-purpose model |
| `openai/gpt-4o-mini` | OpenAI | Cheaper, faster |
| `anthropic/claude-sonnet-4` | Anthropic | Strong reasoning |
| `google/gemini-2.0-flash-001` | Google | Fast, multimodal |
| `meta-llama/llama-3.1-70b-instruct` | Meta (via providers) | Open-source |
| `openrouter/free` | Various | Routes to free-tier models automatically[^7] |

The full, live list of models is at [openrouter.ai/models](https://openrouter.ai/models). If the `model` parameter is omitted, the user's default model is used.[^3]

## Request Parameters Reference

The request body follows the OpenAI Chat Completions schema with OpenRouter extensions:[^3]

| Parameter | Type | Description |
|---|---|---|
| `messages` | `Message[]` | Required. The conversation messages |
| `model` | `string` | Model ID (e.g., `openai/gpt-4o`) |
| `stream` | `boolean` | Enable streaming (default: `false`)[^4] |
| `max_tokens` | `number` | Max tokens to generate |
| `temperature` | `number` | Sampling temperature, 0–2 (default: 1)[^3] |
| `top_p` | `number` | Nucleus sampling, 0–1[^3] |
| `stop` | `string \| string[]` | Stop sequences[^3] |
| `tools` | `Tool[]` | Function/tool calling definitions[^3] |
| `tool_choice` | `string \| object` | Tool selection strategy[^3] |
| `response_format` | `object` | Structured output / JSON mode[^8] |
| `plugins` | `Plugin[]` | OpenRouter plugins (web search, etc.)[^3] |

Non-standard parameters (e.g., `top_k` for OpenAI models) are silently ignored rather than producing errors.[^3]

## Error Handling

### OAuth Errors

| Error | Cause | Fix |
|---|---|---|
| `400 Invalid code_challenge_method` | Mismatched method between Step 1 and Step 3 | Ensure `S256` is used in both the redirect URL and the key exchange request[^1] |
| `403 Invalid code or code_verifier` | Expired code or wrong verifier | Ensure the user is logged in and `code_verifier` matches the original challenge[^1] |
| `405 Method Not Allowed` | Wrong HTTP method | Use `POST` over HTTPS[^1] |

### API Errors

| Status | Meaning |
|---|---|
| `400` | Bad request — check parameters[^9] |
| `401` | Unauthorized — invalid or revoked API key[^9] |
| `402` | Payment required — user has insufficient credits[^9] |
| `429` | Rate limited — back off and retry[^9] |
| `502` | Provider error — retry or try a different model[^9] |

For streaming, errors that occur after tokens have already been sent arrive as SSE events with `finish_reason: "error"` instead of an HTTP error code.[^4]

## Security Considerations

- **The user's API key never leaves the browser.** Store it in `sessionStorage` (cleared on tab close) or `localStorage` (persists) depending on UX preference.
- **Never log the key** to analytics, error tracking services, or console in production.
- **All billing goes directly to the user's OpenRouter account.** The app developer has zero visibility into the user's costs or usage beyond what the API response returns.[^2]
- **Users can revoke the key at any time** from their OpenRouter dashboard, and they can set per-app credit limits for additional control.[^2]
- **The PKCE flow prevents code interception** — even if someone captures the authorization code, they cannot exchange it without the `code_verifier` that only exists in the user's browser session.[^1]
- **Open-source the SPA** if possible, so users can verify the code does not exfiltrate their key.

## Full Minimal Example

A complete, copy-pasteable example putting all the steps together:

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>OpenRouter OAuth PKCE Demo</title>
</head>
<body>
  <div id="app">
    <button id="connectBtn">Connect with OpenRouter</button>
    <div id="chat" style="display:none">
      <textarea id="prompt" rows="3" cols="50" placeholder="Ask anything..."></textarea>
      <br>
      <button id="sendBtn">Send</button>
      <pre id="output"></pre>
    </div>
  </div>

  <script type="module">
    // --- PKCE Helpers ---
    function generateCodeVerifier(length = 64) {
      const array = new Uint8Array(length);
      crypto.getRandomValues(array);
      return btoa(String.fromCharCode(...array))
        .replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
    }

    async function sha256Challenge(verifier) {
      const data = new TextEncoder().encode(verifier);
      const hash = await crypto.subtle.digest('SHA-256', data);
      return btoa(String.fromCharCode(...new Uint8Array(hash)))
        .replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
    }

    const CALLBACK = window.location.origin + window.location.pathname;

    // --- On page load: check for callback code ---
    const params = new URLSearchParams(window.location.search);
    const code = params.get('code');

    if (code) {
      // Step 3: Exchange code for key
      const verifier = sessionStorage.getItem('pkce_verifier');
      sessionStorage.removeItem('pkce_verifier');

      const res = await fetch('https://openrouter.ai/api/v1/auth/keys', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          code,
          code_verifier: verifier,
          code_challenge_method: 'S256',
        }),
      });

      const { key } = await res.json();
      sessionStorage.setItem('or_key', key);

      // Clean URL
      window.history.replaceState({}, '', CALLBACK);
    }

    // --- UI Logic ---
    const userKey = sessionStorage.getItem('or_key');

    if (userKey) {
      document.getElementById('connectBtn').style.display = 'none';
      document.getElementById('chat').style.display = 'block';
    }

    // Step 1 & 2: Start OAuth
    document.getElementById('connectBtn').addEventListener('click', async () => {
      const verifier = generateCodeVerifier();
      const challenge = await sha256Challenge(verifier);
      sessionStorage.setItem('pkce_verifier', verifier);

      const url = new URL('https://openrouter.ai/auth');
      url.searchParams.set('callback_url', CALLBACK);
      url.searchParams.set('code_challenge', challenge);
      url.searchParams.set('code_challenge_method', 'S256');
      window.location.href = url.toString();
    });

    // Step 4 & 5: Send prompt (streaming)
    document.getElementById('sendBtn').addEventListener('click', async () => {
      const prompt = document.getElementById('prompt').value;
      const output = document.getElementById('output');
      output.textContent = '';

      const res = await fetch('https://openrouter.ai/api/v1/chat/completions', {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${sessionStorage.getItem('or_key')}`,
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          model: 'openai/gpt-4o-mini',
          messages: [{ role: 'user', content: prompt }],
          stream: true,
        }),
      });

      const reader = res.body.getReader();
      const decoder = new TextDecoder();
      let buf = '';

      while (true) {
        const { done, value } = await reader.read();
        if (done) break;
        buf += decoder.decode(value, { stream: true });
        const lines = buf.split('\n');
        buf = lines.pop();
        for (const line of lines) {
          const trimmed = line.trim();
          if (!trimmed || trimmed.startsWith(':') || !trimmed.startsWith('data: ')) continue;
          const payload = trimmed.slice(6);
          if (payload === '[DONE]') return;
          try {
            const chunk = JSON.parse(payload);
            const content = chunk.choices?.?.delta?.content;
            if (content) output.textContent += content;
          } catch {}
        }
      }
    });
  </script>
</body>
</html>
```

This single HTML file implements the full flow: OAuth connect → key exchange → streaming chat. No backend, no build step required for prototyping.

---

## References

1. [OAuth PKCE - Secure User Authentication](https://openrouter.ai/docs/guides/overview/auth/oauth) - Step 3: Use the API key. Store the API key securely within the user's browser or in your own databas...

2. [API Authentication | OpenRouter OAuth and API Keys](https://openrouter.ai/docs/api/reference/authentication) - To use an API key, first create your key. Give it a name and you can optionally set a credit limit. ...

3. [OpenRouter API Reference | Complete API Documentation](https://openrouter.ai/docs/api/reference/overview) - Here is the request schema as a TypeScript type. This will be the body of your POST request to the /...

4. [API Streaming | Real-time Model Responses in OpenRouter](https://openrouter.ai/docs/api/reference/streaming) - The OpenRouter API allows streaming responses from any model. This is useful for building chat inter...

5. [OpenRouter TypeScript SDK | Complete Documentation](https://openrouter.ai/docs/sdks/typescript) - The OpenRouter TypeScript SDK is a type-safe toolkit for building AI applications with access to 300...

6. [openrouter/ai-sdk-provider - NPM](https://www.npmjs.com/package/@openrouter/ai-sdk-provider) - The [OpenRouter](https://openrouter.ai/) provider for the [Vercel AI SDK](https://sdk.vercel.ai/docs...

7. [Free Models Router - API, Providers, Stats - OpenRouter](https://openrouter.ai/openrouter/free) - The router smartly filters for models that support features needed for your request such as image un...

8. [Structured Outputs | Enforce JSON Schema in OpenRouter API ...](https://openrouter.ai/docs/guides/features/structured-outputs) - Enforce JSON Schema validation on AI model responses. Get consistent, type-safe outputs and avoid pa...

9. [Create a chat completion | OpenRouter | Documentation](https://openrouter.ai/docs/api/api-reference/chat/send-chat-completion-request) - Sends a request for a model response for the given chat conversation. Supports both streaming and no...

