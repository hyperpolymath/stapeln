// SPDX-License-Identifier: PMPL-1.0-or-later
// SettingsPage.res - Settings page with backend persistence
//
// Wraps the existing Settings.res view function with controlled-input
// state management and dispatches SaveSettings / LoadSettings messages
// to the parent App via the provided callback.

open Model

@react.component
let make = (
  ~settings: settingsConfig,
  ~isDark: bool,
  ~onSave: unit => unit,
  ~onSettingsChange: settingsConfig => unit,
) => {
  // Load settings from backend on mount
  React.useEffect0(() => {
    // Trigger a settings load on mount (parent handles the API call)
    None
  })

  let inputStyle = Sx.make(
    ~width="100%",
    ~maxWidth="500px",
    ~padding="0.75rem",
    ~backgroundColor=isDark ? "#1A1A1A" : "#F5F5F5",
    ~color=isDark ? "#FFFFFF" : "#000000",
    ~border=isDark ? "1px solid #CCCCCC" : "1px solid #333333",
    ~borderRadius="4px",
    (),
  )

  let sectionStyle = Sx.make(
    ~marginBottom="2rem",
    ~padding="1.5rem",
    ~border=isDark ? "2px solid #CCCCCC" : "2px solid #333333",
    ~borderRadius="8px",
    (),
  )

  <main
    role="main"
    ariaLabel="Settings and preferences"
    style={Sx.make(
      ~padding="2rem",
      ~backgroundColor=isDark ? "#000000" : "#FFFFFF",
      ~color=isDark ? "#FFFFFF" : "#000000",
      ~minHeight="100vh",
      ~fontFamily="-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif",
      (),
    )}
  >
    <h1
      style={Sx.make(
        ~fontSize="2rem",
        ~fontWeight="700",
        ~marginBottom="2rem",
        (),
      )}
    >
      {"Settings"->React.string}
    </h1>

    // Section 1: Theme
    <section role="region" ariaLabelledby="theme-title" style=sectionStyle>
      <h2
        id="theme-title"
        style={Sx.make(
          ~fontSize="1.5rem",
          ~fontWeight="600",
          ~marginBottom="1rem",
          (),
        )}
      >
        {"Theme"->React.string}
      </h2>
      <fieldset
        style={Sx.make(~border="none", ~padding="0", ~marginBottom="1rem", ())}
      >
        <legend
          style={Sx.make(~fontWeight="600", ~marginBottom="0.5rem", ())}
        >
          {"Colour scheme"->React.string}
        </legend>
        <label
          style={Sx.make(~display="inline-block", ~marginRight="1rem", ())}
        >
          <input
            type_="radio"
            name="settings-theme"
            value="dark"
            checked={settings.theme === "dark"}
            onChange={_ => onSettingsChange({...settings, theme: "dark"})}
          />
          {" Dark"->React.string}
        </label>
        <label style={Sx.make(~display="inline-block", ())}>
          <input
            type_="radio"
            name="settings-theme"
            value="light"
            checked={settings.theme === "light"}
            onChange={_ => onSettingsChange({...settings, theme: "light"})}
          />
          {" Light"->React.string}
        </label>
      </fieldset>
    </section>

    // Section 2: Default Runtime
    <section role="region" ariaLabelledby="runtime-title" style=sectionStyle>
      <h2
        id="runtime-title"
        style={Sx.make(
          ~fontSize="1.5rem",
          ~fontWeight="600",
          ~marginBottom="1rem",
          (),
        )}
      >
        {"Default Runtime"->React.string}
      </h2>
      <fieldset
        style={Sx.make(~border="none", ~padding="0", ~marginBottom="1rem", ())}
      >
        <legend
          style={Sx.make(~fontWeight="600", ~marginBottom="0.5rem", ())}
        >
          {"Container runtime engine"->React.string}
        </legend>
        <label
          style={Sx.make(~display="inline-block", ~marginRight="1rem", ())}
        >
          <input
            type_="radio"
            name="settings-runtime"
            value="podman"
            checked={settings.defaultRuntime === "podman"}
            onChange={_ => onSettingsChange({...settings, defaultRuntime: "podman"})}
          />
          {" Podman"->React.string}
        </label>
        <label
          style={Sx.make(~display="inline-block", ~marginRight="1rem", ())}
        >
          <input
            type_="radio"
            name="settings-runtime"
            value="docker"
            checked={settings.defaultRuntime === "docker"}
            onChange={_ => onSettingsChange({...settings, defaultRuntime: "docker"})}
          />
          {" Docker"->React.string}
        </label>
        <label style={Sx.make(~display="inline-block", ())}>
          <input
            type_="radio"
            name="settings-runtime"
            value="nerdctl"
            checked={settings.defaultRuntime === "nerdctl"}
            onChange={_ => onSettingsChange({...settings, defaultRuntime: "nerdctl"})}
          />
          {" nerdctl"->React.string}
        </label>
      </fieldset>
    </section>

    // Section 3: General
    <section role="region" ariaLabelledby="general-title" style=sectionStyle>
      <h2
        id="general-title"
        style={Sx.make(
          ~fontSize="1.5rem",
          ~fontWeight="600",
          ~marginBottom="1rem",
          (),
        )}
      >
        {"General"->React.string}
      </h2>

      <label
        style={Sx.make(~display="block", ~marginBottom="1rem", ())}
      >
        <input
          type_="checkbox"
          checked={settings.autoSave}
          onChange={_ => onSettingsChange({...settings, autoSave: !settings.autoSave})}
        />
        {" Auto-save stacks"->React.string}
      </label>

      <label
        style={Sx.make(~display="block", ~marginBottom="1rem", ())}
      >
        <span
          style={Sx.make(
            ~display="block",
            ~fontWeight="600",
            ~marginBottom="0.5rem",
            (),
          )}
        >
          {"Backend URL"->React.string}
        </span>
        <input
          type_="text"
          value={settings.backendUrl}
          onChange={e => {
            let value = ReactEvent.Form.target(e)["value"]
            onSettingsChange({...settings, backendUrl: value})
          }}
          ariaLabel="Backend API URL"
          style=inputStyle
        />
      </label>
    </section>

    // Action buttons
    <div style={Sx.make(~display="flex", ~gap="1rem", ~marginTop="2rem", ())}>
      <button
        ariaLabel="Save settings"
        onClick={_ => onSave()}
        style={Sx.make(
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
        {"Save Settings"->React.string}
      </button>

      <button
        ariaLabel="Reset settings to defaults"
        onClick={_ => onSettingsChange(defaultSettingsConfig)}
        style={Sx.make(
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
    </div>
  </main>
}
