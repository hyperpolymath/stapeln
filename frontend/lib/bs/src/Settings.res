// SPDX-License-Identifier: PMPL-1.0-or-later
// Settings.res - Settings and preferences page (Page 3)

type runtimeEngine = Podman | Docker | Nerdctl
type authMethod = OAuth2 | APIKey | MTLS
type ipcMode = ZeroCopyWASM | JsonHttp

type settings = {
  // Default Component Settings
  defaultRuntime: runtimeEngine,
  defaultRegistry: string,
  autoVerifySignatures: bool,
  requireSBOM: bool,
  enforceNetworkPolicies: bool,
  defaultCpuLimit: float,
  defaultMemoryLimit: int, // MB
  defaultStorageLimit: int, // GB
  // Cerro Torre
  ctCliPath: string,
  ctSigningKey: string,
  ctTransparencyLog: string,
  ctBaseImage: string,
  ctCompression: string,
  ctAttestationFormat: string,
  // Svalinn
  svalinnEndpoint: string,
  svalinnAuth: authMethod,
  svalinnRequireVerified: bool,
  svalinnBlockPrivileged: bool,
  svalinnEnforceQuotas: bool,
  svalinnAuditLogging: bool,
  // selur
  selurIpcMode: ipcMode,
  selurSharedMemory: int, // MB
  selurMaxThroughput: int, // req/s
  selurLatencyTarget: int, // ms
  // Vörðr
  vordrEndpoint: string,
  vordrProtocol: string,
  vordrAutoRestart: bool,
  vordrMaxRetries: int,
  vordrBackoff: string,
  // UI
  theme: string, // "system", "light", "dark"
  fontSize: int,
  highContrast: bool,
  reducedMotion: bool,
  screenReaderMode: bool,
  gridSnapping: bool,
  gridSize: int,
  autoArrange: bool,
}

let defaultSettings: settings = {
  defaultRuntime: Podman,
  defaultRegistry: "ghcr.io/hyperpolymath",
  autoVerifySignatures: true,
  requireSBOM: true,
  enforceNetworkPolicies: true,
  defaultCpuLimit: 1.0,
  defaultMemoryLimit: 512,
  defaultStorageLimit: 10,
  ctCliPath: "/usr/local/bin/ct",
  ctSigningKey: "~/.ct/keys/default.key",
  ctTransparencyLog: "https://rekor.sigstore.dev",
  ctBaseImage: "ghcr.io/hyperpolymath/base:latest",
  ctCompression: "zstd",
  ctAttestationFormat: "in-toto",
  svalinnEndpoint: "http://localhost:8000",
  svalinnAuth: OAuth2,
  svalinnRequireVerified: true,
  svalinnBlockPrivileged: true,
  svalinnEnforceQuotas: true,
  svalinnAuditLogging: true,
  selurIpcMode: ZeroCopyWASM,
  selurSharedMemory: 256,
  selurMaxThroughput: 10000,
  selurLatencyTarget: 1,
  vordrEndpoint: "http://localhost:8081",
  vordrProtocol: "JSON-RPC 2.0",
  vordrAutoRestart: true,
  vordrMaxRetries: 3,
  vordrBackoff: "exponential",
  theme: "system",
  fontSize: 16,
  highContrast: false,
  reducedMotion: false,
  screenReaderMode: false,
  gridSnapping: true,
  gridSize: 20,
  autoArrange: true,
}

// Settings view
let view = (settings: settings, isDark: bool) => {
  <main
    role="main"
    ariaLabel="Settings and preferences"
    style={ReactDOM.Style.make(
      ~padding="2rem",
      ~backgroundColor=isDark ? "#000000" : "#FFFFFF",
      ~color=isDark ? "#FFFFFF" : "#000000",
      ~minHeight="100vh",
      ~fontFamily="-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif",
      (),
    )}
  >
    <h1
      style={{
        fontSize: "2rem",
        fontWeight: "700",
        marginBottom: "2rem",
      }}
    >
      {"Settings"->React.string}
    </h1>

    // Section 1: Default Component Settings
    <section
      role="region"
      ariaLabelledby="defaults-title"
      style={ReactDOM.Style.make(
        ~marginBottom="2rem",
        ~padding="1.5rem",
        ~border=isDark ? "2px solid #CCCCCC" : "2px solid #333333",
        ~borderRadius="8px",
        (),
      )}
    >
      <h2 id="defaults-title" style={{fontSize: "1.5rem", fontWeight: "600", marginBottom: "1rem"}}>
        {"Default Component Settings"->React.string}
      </h2>

      <fieldset style={{border: "none", padding: "0", marginBottom: "1rem"}}>
        <legend style={{fontWeight: "600", marginBottom: "0.5rem"}}>
          {"Default Container Runtime"->React.string}
        </legend>
        <label style={{display: "inline-block", marginRight: "1rem"}}>
          <input
            type_="radio"
            name="runtime"
            value="podman"
            defaultChecked={settings.defaultRuntime === Podman}
          />
          {" Podman"->React.string}
        </label>
        <label style={{display: "inline-block", marginRight: "1rem"}}>
          <input
            type_="radio"
            name="runtime"
            value="docker"
            defaultChecked={settings.defaultRuntime === Docker}
          />
          {" Docker"->React.string}
        </label>
        <label style={{display: "inline-block"}}>
          <input
            type_="radio"
            name="runtime"
            value="nerdctl"
            defaultChecked={settings.defaultRuntime === Nerdctl}
          />
          {" nerdctl"->React.string}
        </label>
      </fieldset>

      <label style={{display: "block", marginBottom: "1rem"}}>
        <span style={{display: "block", fontWeight: "600", marginBottom: "0.5rem"}}>
          {"Default Registry"->React.string}
        </span>
        <input
          type_="text"
          defaultValue=settings.defaultRegistry
          ariaLabel="Default container registry"
          style={{
            width: "100%",
            maxWidth: "500px",
            padding: "0.75rem",
            backgroundColor: isDark ? "#1A1A1A" : "#F5F5F5",
            color: isDark ? "#FFFFFF" : "#000000",
            border: isDark ? "1px solid #CCCCCC" : "1px solid #333333",
            borderRadius: "4px",
          }}
        />
      </label>

      <label style={{display: "block", marginBottom: "0.5rem"}}>
        <input
          type_="checkbox"
          defaultChecked=settings.autoVerifySignatures
          ariaLabel="Automatically verify container signatures"
        />
        {" Auto-verify signatures"->React.string}
      </label>

      <label style={{display: "block", marginBottom: "0.5rem"}}>
        <input
          type_="checkbox"
          defaultChecked=settings.requireSBOM
          ariaLabel="Require SBOM for all containers"
        />
        {" Require SBOM"->React.string}
      </label>

      <label style={{display: "block", marginBottom: "1rem"}}>
        <input
          type_="checkbox"
          defaultChecked=settings.enforceNetworkPolicies
          ariaLabel="Enforce network policies"
        />
        {" Enforce network policies"->React.string}
      </label>

      <h3 style={{fontSize: "1.2rem", fontWeight: "600", margin: "1.5rem 0 1rem"}}>
        {"Default Resource Limits"->React.string}
      </h3>

      <div
        style={ReactDOM.Style.make(
          ~display="grid",
          ~gridTemplateColumns="repeat(3, 1fr)",
          ~gap="1rem",
          ~maxWidth="600px",
          (),
        )}
      >
        <label>
          <span style={{display: "block", fontWeight: "600", marginBottom: "0.5rem"}}>
            {"CPU (cores)"->React.string}
          </span>
          <input
            type_="number"
            defaultValue={Float.toString(settings.defaultCpuLimit)}
            step=0.1
            min="0.1"
            ariaLabel="Default CPU limit in cores"
            style={ReactDOM.Style.make(
              ~width="100%",
              ~padding="0.75rem",
              ~backgroundColor=isDark ? "#1A1A1A" : "#F5F5F5",
              ~color=isDark ? "#FFFFFF" : "#000000",
              ~border=isDark ? "1px solid #CCCCCC" : "1px solid #333333",
              ~borderRadius="4px",
              (),
            )}
          />
        </label>

        <label>
          <span style={{display: "block", fontWeight: "600", marginBottom: "0.5rem"}}>
            {"Memory (MB)"->React.string}
          </span>
          <input
            type_="number"
            defaultValue={Int.toString(settings.defaultMemoryLimit)}
            step=128.0
            min="128"
            ariaLabel="Default memory limit in megabytes"
            style={ReactDOM.Style.make(
              ~width="100%",
              ~padding="0.75rem",
              ~backgroundColor=isDark ? "#1A1A1A" : "#F5F5F5",
              ~color=isDark ? "#FFFFFF" : "#000000",
              ~border=isDark ? "1px solid #CCCCCC" : "1px solid #333333",
              ~borderRadius="4px",
              (),
            )}
          />
        </label>

        <label>
          <span style={{display: "block", fontWeight: "600", marginBottom: "0.5rem"}}>
            {"Storage (GB)"->React.string}
          </span>
          <input
            type_="number"
            defaultValue={Int.toString(settings.defaultStorageLimit)}
            step=1.0
            min="1"
            ariaLabel="Default storage limit in gigabytes"
            style={ReactDOM.Style.make(
              ~width="100%",
              ~padding="0.75rem",
              ~backgroundColor=isDark ? "#1A1A1A" : "#F5F5F5",
              ~color=isDark ? "#FFFFFF" : "#000000",
              ~border=isDark ? "1px solid #CCCCCC" : "1px solid #333333",
              ~borderRadius="4px",
              (),
            )}
          />
        </label>
      </div>
    </section>

    // Section 2: Cerro Torre Integration
    <section
      role="region"
      ariaLabelledby="cerro-torre-title"
      style={ReactDOM.Style.make(
        ~marginBottom="2rem",
        ~padding="1.5rem",
        ~border=isDark ? "2px solid #CCCCCC" : "2px solid #333333",
        ~borderRadius="8px",
        (),
      )}
    >
      <h2
        id="cerro-torre-title" style={{fontSize: "1.5rem", fontWeight: "600", marginBottom: "1rem"}}
      >
        {"🏔️ Cerro Torre Integration"->React.string}
      </h2>

      <label style={{display: "block", marginBottom: "1rem"}}>
        <span style={{display: "block", fontWeight: "600", marginBottom: "0.5rem"}}>
          {"CLI Path"->React.string}
        </span>
        <input
          type_="text"
          defaultValue=settings.ctCliPath
          ariaLabel="Path to Cerro Torre CLI"
          style={ReactDOM.Style.make(
            ~width="100%",
            ~maxWidth="500px",
            ~padding="0.75rem",
            ~backgroundColor=isDark ? "#1A1A1A" : "#F5F5F5",
            ~color=isDark ? "#FFFFFF" : "#000000",
            ~border=isDark ? "1px solid #CCCCCC" : "1px solid #333333",
            ~borderRadius="4px",
            ~fontFamily="monospace",
            (),
          )}
        />
      </label>

      <label style={{display: "block", marginBottom: "1rem"}}>
        <span style={{display: "block", fontWeight: "600", marginBottom: "0.5rem"}}>
          {"Default Signing Key"->React.string}
        </span>
        <input
          type_="text"
          defaultValue=settings.ctSigningKey
          ariaLabel="Path to default signing key"
          style={ReactDOM.Style.make(
            ~width="100%",
            ~maxWidth="500px",
            ~padding="0.75rem",
            ~backgroundColor=isDark ? "#1A1A1A" : "#F5F5F5",
            ~color=isDark ? "#FFFFFF" : "#000000",
            ~border=isDark ? "1px solid #CCCCCC" : "1px solid #333333",
            ~borderRadius="4px",
            ~fontFamily="monospace",
            (),
          )}
        />
      </label>

      <label style={{display: "block", marginBottom: "1rem"}}>
        <span style={{display: "block", fontWeight: "600", marginBottom: "0.5rem"}}>
          {"Transparency Log"->React.string}
        </span>
        <input
          type_="url"
          defaultValue=settings.ctTransparencyLog
          ariaLabel="Transparency log URL"
          style={ReactDOM.Style.make(
            ~width="100%",
            ~maxWidth="500px",
            ~padding="0.75rem",
            ~backgroundColor=isDark ? "#1A1A1A" : "#F5F5F5",
            ~color=isDark ? "#FFFFFF" : "#000000",
            ~border=isDark ? "1px solid #CCCCCC" : "1px solid #333333",
            ~borderRadius="4px",
            (),
          )}
        />
      </label>
    </section>

    // Section 3: UI Preferences
    <section
      role="region"
      ariaLabelledby="ui-title"
      style={ReactDOM.Style.make(
        ~marginBottom="2rem",
        ~padding="1.5rem",
        ~border=isDark ? "2px solid #CCCCCC" : "2px solid #333333",
        ~borderRadius="8px",
        (),
      )}
    >
      <h2 id="ui-title" style={{fontSize: "1.5rem", fontWeight: "600", marginBottom: "1rem"}}>
        {"UI Preferences"->React.string}
      </h2>

      <fieldset style={{border: "none", padding: "0", marginBottom: "1rem"}}>
        <legend style={{fontWeight: "600", marginBottom: "0.5rem"}}>
          {"Theme"->React.string}
        </legend>
        <label style={{display: "inline-block", marginRight: "1rem"}}>
          <input
            type_="radio" name="theme" value="system" defaultChecked={settings.theme === "system"}
          />
          {" System"->React.string}
        </label>
        <label style={{display: "inline-block", marginRight: "1rem"}}>
          <input
            type_="radio" name="theme" value="light" defaultChecked={settings.theme === "light"}
          />
          {" Light"->React.string}
        </label>
        <label style={{display: "inline-block"}}>
          <input
            type_="radio" name="theme" value="dark" defaultChecked={settings.theme === "dark"}
          />
          {" Dark"->React.string}
        </label>
      </fieldset>

      <h3 style={{fontSize: "1.2rem", fontWeight: "600", margin: "1.5rem 0 1rem"}}>
        {"Accessibility"->React.string}
      </h3>

      <label style={{display: "block", marginBottom: "1rem"}}>
        <span style={{display: "block", fontWeight: "600", marginBottom: "0.5rem"}}>
          {"Font Size (px)"->React.string}
        </span>
        <input
          type_="number"
          defaultValue={Int.toString(settings.fontSize)}
          min="12"
          max="24"
          ariaLabel="Font size in pixels"
          style={ReactDOM.Style.make(
            ~width="150px",
            ~padding="0.75rem",
            ~backgroundColor=isDark ? "#1A1A1A" : "#F5F5F5",
            ~color=isDark ? "#FFFFFF" : "#000000",
            ~border=isDark ? "1px solid #CCCCCC" : "1px solid #333333",
            ~borderRadius="4px",
            (),
          )}
        />
      </label>

      <label style={{display: "block", marginBottom: "0.5rem"}}>
        <input type_="checkbox" defaultChecked=settings.highContrast />
        {" High contrast mode"->React.string}
      </label>

      <label style={{display: "block", marginBottom: "0.5rem"}}>
        <input type_="checkbox" defaultChecked=settings.reducedMotion />
        {" Reduced motion"->React.string}
      </label>

      <label style={{display: "block", marginBottom: "1rem"}}>
        <input type_="checkbox" defaultChecked=settings.screenReaderMode />
        {" Screen reader mode (extra announcements)"->React.string}
      </label>
    </section>

    // Action buttons
    <div style={ReactDOM.Style.make(~display="flex", ~gap="1rem", ~marginTop="2rem", ())}>
      <button
        ariaLabel="Reset all settings to default values"
        style={ReactDOM.Style.make(
          ~padding="0.75rem 1.5rem",
          ~backgroundColor=isDark ? "#1A1A1A" : "#F5F5F5",
          ~color=isDark ? "#FFFFFF" : "#000000",
          ~border=isDark ? "2px solid #CCCCCC" : "2px solid #333333",
          ~borderRadius="6px",
          ~fontWeight="600",
          ~cursor="pointer",
          (),
        )}
      >
        {"Reset to Defaults"->React.string}
      </button>

      <button
        ariaLabel="Save settings"
        style={ReactDOM.Style.make(
          ~padding="0.75rem 1.5rem",
          ~backgroundColor=isDark ? "#66B2FF" : "#0052CC",
          ~color="white",
          ~border="none",
          ~borderRadius="6px",
          ~fontWeight="600",
          ~cursor="pointer",
          (),
        )}
      >
        {"Save"->React.string}
      </button>

      <button
        ariaLabel="Cancel changes"
        style={ReactDOM.Style.make(
          ~padding="0.75rem 1.5rem",
          ~backgroundColor=isDark ? "#1A1A1A" : "#F5F5F5",
          ~color=isDark ? "#FFFFFF" : "#000000",
          ~border=isDark ? "2px solid #CCCCCC" : "2px solid #333333",
          ~borderRadius="6px",
          ~fontWeight="600",
          ~cursor="pointer",
          (),
        )}
      >
        {"Cancel"->React.string}
      </button>
    </div>
  </main>
}
