import { Elm } from "./Main.elm";

// =========================================================
// PKCE Helpers
// =========================================================

function generateCodeVerifier(length = 64) {
  const array = new Uint8Array(length);
  crypto.getRandomValues(array);
  return btoa(String.fromCharCode(...array))
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=+$/, "");
}

async function sha256Challenge(verifier) {
  const data = new TextEncoder().encode(verifier);
  const hash = await crypto.subtle.digest("SHA-256", data);
  return btoa(String.fromCharCode(...new Uint8Array(hash)))
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=+$/, "");
}

// =========================================================
// OAuth Callback Handling (before Elm init)
// =========================================================

const CALLBACK_URL = window.location.origin + window.location.pathname;

async function handleOAuthCallback() {
  const params = new URLSearchParams(window.location.search);
  const code = params.get("code");

  if (!code) return;

  const verifier = sessionStorage.getItem("pkce_verifier");
  sessionStorage.removeItem("pkce_verifier");

  if (!verifier) {
    console.error("No PKCE verifier found in session storage");
    window.history.replaceState({}, "", CALLBACK_URL);
    return;
  }

  try {
    const res = await fetch("https://openrouter.ai/api/v1/auth/keys", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        code,
        code_verifier: verifier,
        code_challenge_method: "S256",
      }),
    });

    if (!res.ok) {
      const error = await res.json().catch(() => ({}));
      throw new Error(error.message || `Key exchange failed: ${res.status}`);
    }

    const { key } = await res.json();
    sessionStorage.setItem("openrouter_key", key);
  } catch (err) {
    console.error("OAuth key exchange failed:", err);
  }

  // Clean URL
  window.history.replaceState({}, "", CALLBACK_URL);
}

// =========================================================
// Init (wrapped in async IIFE for compatibility)
// =========================================================

const SYSTEM_PROMPT = `You are a language analysis assistant. The user will provide a short narrative written in a non-English language (such as Spanish, French, Japanese, etc.).

Your task:
1. Split the narrative into individual sentences.
2. For each sentence, determine the primary English verb tense that would be used to translate it.
3. Provide a brief explanation (1 sentence) of why that tense applies.

The verb tense MUST be one of these exact values:
SimplePast, PastContinuous, PastPerfect, PastPerfectContinuous,
SimplePresent, PresentContinuous, PresentPerfect, PresentPerfectContinuous,
SimpleFuture, FutureContinuous, FuturePerfect, FuturePerfectContinuous

Respond with a JSON object containing a "sentences" array.`;

const RESPONSE_SCHEMA = {
  type: "json_schema",
  json_schema: {
    name: "tense_analysis",
    strict: true,
    schema: {
      type: "object",
      properties: {
        sentences: {
          type: "array",
          items: {
            type: "object",
            properties: {
              original: { type: "string" },
              verbTense: {
                type: "string",
                enum: [
                  "SimplePast",
                  "PastContinuous",
                  "PastPerfect",
                  "PastPerfectContinuous",
                  "SimplePresent",
                  "PresentContinuous",
                  "PresentPerfect",
                  "PresentPerfectContinuous",
                  "SimpleFuture",
                  "FutureContinuous",
                  "FuturePerfect",
                  "FuturePerfectContinuous",
                ],
              },
              explanation: { type: "string" },
            },
            required: ["original", "verbTense", "explanation"],
            additionalProperties: false,
          },
        },
      },
      required: ["sentences"],
      additionalProperties: false,
    },
  },
};

(async function main() {
  await handleOAuthCallback();

  const userKey = sessionStorage.getItem("openrouter_key");

  const app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: { loggedIn: !!userKey },
  });

  // =========================================================
  // Port: startOAuthLogin
  // =========================================================

  app.ports.startOAuthLogin.subscribe(async () => {
    const verifier = generateCodeVerifier();
    const challenge = await sha256Challenge(verifier);
    sessionStorage.setItem("pkce_verifier", verifier);

    const url = new URL("https://openrouter.ai/auth");
    url.searchParams.set("callback_url", CALLBACK_URL);
    url.searchParams.set("code_challenge", challenge);
    url.searchParams.set("code_challenge_method", "S256");
    window.location.href = url.toString();
  });

  // =========================================================
  // Port: analyzeNarrative
  // =========================================================

  app.ports.analyzeNarrative.subscribe(async (narrative) => {
    const key = sessionStorage.getItem("openrouter_key");

    if (!key) {
      app.ports.llmResponseReceived.send({
        error: "Not connected to OpenRouter. Please connect first.",
      });
      return;
    }

    try {
      const res = await fetch(
        "https://openrouter.ai/api/v1/chat/completions",
        {
          method: "POST",
          headers: {
            Authorization: `Bearer ${key}`,
            "Content-Type": "application/json",
            "HTTP-Referer": window.location.origin,
            "X-OpenRouter-Title": "Grammar Space",
          },
          body: JSON.stringify({
            model: "openai/gpt-4o-mini",
            messages: [
              { role: "system", content: SYSTEM_PROMPT },
              { role: "user", content: narrative },
            ],
            response_format: RESPONSE_SCHEMA,
            temperature: 0.3,
          }),
        },
      );

      if (!res.ok) {
        const status = res.status;
        let msg;
        if (status === 401) {
          sessionStorage.removeItem("openrouter_key");
          app.ports.oauthStatusChanged.send(false);
          msg = "API key is invalid or revoked. Please reconnect.";
        } else if (status === 402) {
          msg = "Insufficient credits on your OpenRouter account.";
        } else if (status === 429) {
          msg = "Rate limited. Please wait a moment and try again.";
        } else {
          const body = await res.json().catch(() => ({}));
          msg = body.error?.message || `API error: ${status}`;
        }
        app.ports.llmResponseReceived.send({ error: msg });
        return;
      }

      const data = await res.json();
      const content = data.choices?.[0]?.message?.content;

      if (!content) {
        app.ports.llmResponseReceived.send({
          error: "No content in API response.",
        });
        return;
      }

      const parsed = JSON.parse(content);
      app.ports.llmResponseReceived.send(parsed);
    } catch (err) {
      app.ports.llmResponseReceived.send({
        error: err.message || "Network error. Please check your connection.",
      });
    }
  });
})();
