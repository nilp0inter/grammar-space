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

  if (!code) return null;

  const verifier = sessionStorage.getItem("pkce_verifier");
  sessionStorage.removeItem("pkce_verifier");

  if (!verifier) {
    console.error("No PKCE verifier found in session storage");
    window.history.replaceState({}, "", CALLBACK_URL);
    return null;
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
    window.history.replaceState({}, "", CALLBACK_URL);
    return key;
  } catch (err) {
    console.error("OAuth key exchange failed:", err);
    window.history.replaceState({}, "", CALLBACK_URL);
    return null;
  }
}

// =========================================================
// Init
// =========================================================

(async function main() {
  const oauthKey = await handleOAuthCallback();
  const existingKey = sessionStorage.getItem("openrouter_key");
  const apiKey = oauthKey || existingKey || null;

  let savedStories = [];
  try {
    const raw = localStorage.getItem("grammar_space_stories");
    if (raw) savedStories = JSON.parse(raw);
  } catch (_) {}

  const app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: { apiKey, savedStories },
  });

  // If we just got a key from OAuth callback, notify Elm
  if (oauthKey) {
    app.ports.oauthKeyReceived.send(oauthKey);
  }

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
  // Port: saveApiKey / clearApiKey
  // =========================================================

  app.ports.saveApiKey.subscribe((key) => {
    sessionStorage.setItem("openrouter_key", key);
  });

  app.ports.clearApiKey.subscribe(() => {
    sessionStorage.removeItem("openrouter_key");
  });

  // =========================================================
  // Port: saveStories
  // =========================================================

  app.ports.saveStories.subscribe((stories) => {
    try {
      localStorage.setItem("grammar_space_stories", JSON.stringify(stories));
    } catch (err) {
      console.error("Failed to save stories:", err);
    }
  });
})();
