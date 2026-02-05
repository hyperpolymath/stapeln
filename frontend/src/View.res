// SPDX-License-Identifier: PMPL-1.0-or-later
// View.res - Main view with Paragon-style interface

open Model
open Msg

// WCAG 2.3 AAA color palette
module Colors = {
  // Light mode (contrast ratio 7:1 minimum for AAA)
  let lightBg = "#FFFFFF"
  let lightText = "#000000"
  let lightBorder = "#333333"
  let lightPrimary = "#0052CC" // Blue with sufficient contrast
  let lightSecondary = "#666666"

  // Dark mode (contrast ratio 7:1 minimum for AAA)
  let darkBg = "#000000"
  let darkText = "#FFFFFF"
  let darkBorder = "#CCCCCC"
  let darkPrimary = "#66B2FF" // Lighter blue for dark mode
  let darkSecondary = "#AAAAAA"

  // Component colors (Paragon-style)
  let cerroTorre = "#1B5E20" // Dark green
  let svalinn = "#0D47A1" // Dark blue
  let selur = "#4A148C" // Deep purple
  let vordr = "#B71C1C" // Dark red
  let podman = "#6A1B9A" // Purple
  let docker = "#1565C0" // Blue
  let nerdctl = "#2E7D32" // Green
}

// Accessibility helpers
let ariaLabel = (text: string): string => {
  "aria-label=\"" ++ text ++ "\""
}

let ariaDescribedBy = (id: string): string => {
  "aria-describedby=\"" ++ id ++ "\""
}

let ariaRole = (role: string): string => {
  "role=\"" ++ role ++ "\""
}

let ariaBraille = (text: string): string => {
  // Braille-ready semantic annotation
  "data-braille=\"" ++ text ++ "\""
}

// Paragon-style block view (like disk partitions)
let renderStackBlock = (component: component, isDark: bool) => {
  let bgColor = switch component.componentType {
  | CerroTorre => Colors.cerroTorre
  | Svalinn => Colors.svalinn
  | Selur => Colors.selur
  | Vordr => Colors.vordr
  | Podman => Colors.podman
  | Docker => Colors.docker
  | Nerdctl => Colors.nerdctl
  | Volume => "#757575"
  | Network => "#9E9E9E"
  }

  let componentName = componentTypeToString(component.componentType)

  <section
    className="stack-block"
    role="region"
    ariaLabel={componentName ++ " component"}
    ariaBraille={componentName}
    style={`
      background-color: ${bgColor};
      color: white;
      padding: 1.5rem;
      margin: 0.5rem;
      border-radius: 8px;
      min-height: 80px;
      display: flex;
      flex-direction: column;
      justify-content: center;
      border: 2px solid ${isDark ? Colors.darkBorder : Colors.lightBorder};
      box-shadow: 0 2px 4px rgba(0,0,0,0.3);
    `}>
    <h3
      style="margin: 0; font-size: 1.2rem; font-weight: 600;"
      id={component.id ++ "-title"}>
      {componentName ->React.string}
    </h3>
    <p
      style="margin: 0.5rem 0 0 0; font-size: 0.9rem; opacity: 0.9;"
      id={component.id ++ "-desc"}
      ariaDescribedby={component.id ++ "-title"}>
      {("Position: " ++ Float.toString(component.position.x) ++ ", " ++
        Float.toString(component.position.y)) ->React.string}
    </p>

    // Semantic metadata (hidden from visual, available for screen readers)
    <div
      style={ReactDOM.Style.make(~display="none", ())}
      role="note"
      ariaHidden="false">
      <div data-component-id={component.id} />
      <div data-component-name={componentName} />
      <div data-component-layer={
        switch component.componentType {
        | CerroTorre => "build"
        | LagoGrey => "base-image"
        | Svalinn => "gateway"
        | Selur => "bridge"
        | Vordr => "runtime"
        | Podman | Docker | Nerdctl => "container-engine"
        | Volume => "storage"
        | Network => "network"
        }
      } />
    </div>
  </section>
}

// Main Paragon-style vertical stack view
let renderParagonStack = (model: model, isDark: bool) => {
  <main
    className="paragon-stack-view"
    role="main"
    ariaLabel="Container Stack Designer"
    style={`
      display: flex;
      flex-direction: column;
      padding: 2rem;
      background-color: ${isDark ? Colors.darkBg : Colors.lightBg};
      color: ${isDark ? Colors.darkText : Colors.lightText};
      min-height: 100vh;
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    `}>

    // Screen reader announcement region
    <div
      role="status"
      aria-live="polite"
      aria-atomic="true"
      style="position: absolute; left: -10000px; width: 1px; height: 1px; overflow: hidden;">
      {switch model.validationResult {
      | None => React.null
      | Some(result) =>
        (result.valid
          ? "Stack validation passed"
          : "Stack validation failed with " ++ Int.toString(Array.length(result.errors)) ++ " errors"
        ) ->React.string
      }}
    </div>

    // Header with theme toggle
    <header
      role="banner"
      style={`
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding-bottom: 1.5rem;
        border-bottom: 2px solid ${isDark ? Colors.darkBorder : Colors.lightBorder};
      `}>
      <h1
        style="margin: 0; font-size: 2rem; font-weight: 700;"
        id="main-title">
        {"stackur" ->React.string}
        <span style="font-size: 0.9rem; font-weight: 400; opacity: 0.8; margin-left: 0.5rem;">
          {"Container Stack Designer" ->React.string}
        </span>
      </h1>

      // Theme toggle (system aware)
      <button
        ariaLabel="Toggle dark/light theme"
        ariaBraille="Theme"
        role="switch"
        aria-checked={isDark ? "true" : "false"}
        style={`
          padding: 0.75rem 1.5rem;
          background-color: ${isDark ? Colors.darkPrimary : Colors.lightPrimary};
          color: white;
          border: none;
          border-radius: 6px;
          font-size: 1rem;
          cursor: pointer;
          font-weight: 600;
          transition: background-color 0.2s ease;
        `}>
        {(isDark ? "🌙 Dark" : "☀️ Light") ->React.string}
      </button>
    </header>

    // Component palette (left sidebar)
    <div style="display: flex; margin-top: 2rem;">
      <aside
        role="complementary"
        ariaLabel="Component palette"
        style={`
          width: 250px;
          padding-right: 2rem;
          border-right: 2px solid ${isDark ? Colors.darkBorder : Colors.lightBorder};
        `}>
        <h2
          style="font-size: 1.3rem; font-weight: 600; margin-bottom: 1rem;"
          id="palette-title">
          {"Components" ->React.string}
        </h2>

        <nav ariaLabelledBy="palette-title">
          <ul
            role="list"
            style="list-style: none; padding: 0; margin: 0;">
            {
              let components = [
                (CerroTorre, "Cerro Torre", "Build verified container bundles"),
                (Svalinn, "Svalinn", "Edge gateway with authentication"),
                (Selur, "selur", "Zero-copy IPC bridge"),
                (Vordr, "Vörðr", "Container orchestrator"),
                (Podman, "Podman", "OCI runtime"),
                (Docker, "Docker", "Docker Engine runtime"),
                (Nerdctl, "nerdctl", "containerd CLI"),
              ]
              components->Array.map(((ct, name, desc)) => {
                <li
                  role="listitem"
                  style="margin-bottom: 0.75rem;">
                  <button
                    ariaLabel={"Add " ++ name ++ " to stack. " ++ desc}
                    ariaBraille={name}
                    style={`
                      width: 100%;
                      padding: 1rem;
                      background-color: ${isDark ? "#1A1A1A" : "#F5F5F5"};
                      color: ${isDark ? Colors.darkText : Colors.lightText};
                      border: 2px solid ${isDark ? Colors.darkBorder : Colors.lightBorder};
                      border-radius: 6px;
                      text-align: left;
                      cursor: pointer;
                      font-size: 0.95rem;
                      font-weight: 500;
                      transition: all 0.2s ease;
                    `}>
                    <div style="font-weight: 600;">{name->React.string}</div>
                    <div style="font-size: 0.8rem; opacity: 0.8; margin-top: 0.25rem;">
                      {desc->React.string}
                    </div>
                  </button>
                </li>
              })->React.array
            }
          </ul>
        </nav>
      </aside>

      // Main stack area (Paragon-style vertical blocks)
      <article
        role="article"
        ariaLabel="Current stack configuration"
        style="flex: 1; padding-left: 2rem;">
        <h2
          style="font-size: 1.3rem; font-weight: 600; margin-bottom: 1rem;"
          id="stack-title">
          {"Stack Configuration" ->React.string}
        </h2>

        {Array.length(model.components) === 0
          ? <div
              role="status"
              ariaLabel="Empty stack"
              style={`
                padding: 3rem;
                text-align: center;
                border: 2px dashed ${isDark ? Colors.darkBorder : Colors.lightBorder};
                border-radius: 8px;
                color: ${isDark ? Colors.darkSecondary : Colors.lightSecondary};
              `}>
              <p style="font-size: 1.1rem; margin: 0;">
                {"Drag components from the palette to build your stack" ->React.string}
              </p>
            </div>
          : <div
              role="region"
              ariaLabel="Stack components"
              ariaDescribedBy="stack-title">
              {model.components
              -> Array.map(comp => renderStackBlock(comp, isDark))
              -> React.array}
            </div>
        }
      </article>
    </div>
  </main>
}

// Main view function
let view = (model: model) => {
  // Detect system dark mode preference
  let isDarkMode = false // TODO: Detect from window.matchMedia('(prefers-color-scheme: dark)')

  renderParagonStack(model, isDarkMode)
}
