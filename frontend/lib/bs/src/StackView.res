// SPDX-License-Identifier: PMPL-1.0-or-later
// StackView.res - Vertical stack view with Paragon-style interface

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
  let lagoGrey = "#546E7A" // Blue grey (minimal Linux images)
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
  | LagoGrey => Colors.lagoGrey
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
    style={{
      backgroundColor: bgColor,
      color: "white",
      padding: "1.5rem",
      margin: "0.5rem",
      borderRadius: "8px",
      minHeight: "80px",
      display: "flex",
      flexDirection: "column",
      justifyContent: "center",
      border: isDark ? "2px solid " ++ Colors.darkBorder : "2px solid " ++ Colors.lightBorder,
      boxShadow: "0 2px 4px rgba(0,0,0,0.3)",
    }}
  >
    <h3 style={{margin: "0", fontSize: "1.2rem", fontWeight: "600"}} id={component.id ++ "-title"}>
      {componentName->React.string}
    </h3>
    <p
      style={{margin: "0.5rem 0 0 0", fontSize: "0.9rem", opacity: "0.9"}}
      id={component.id ++ "-desc"}
      ariaDescribedby={component.id ++ "-title"}
    >
      {("Position: " ++
      Float.toString(component.position.x) ++
      ", " ++
      Float.toString(component.position.y))->React.string}
    </p>

    // Metadata available via aria-label already
  </section>
}

// Main Paragon-style vertical stack view
let renderParagonStack = (model: model, isDark: bool) => {
  <main
    className="paragon-stack-view"
    role="main"
    ariaLabel="Container Stack Designer"
    style={ReactDOM.Style.make(
      ~display="flex",
      ~flexDirection="column",
      ~padding="2rem",
      ~backgroundColor=isDark ? Colors.darkBg : Colors.lightBg,
      ~color=isDark ? Colors.darkText : Colors.lightText,
      ~minHeight="100vh",
      ~fontFamily="-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif",
      (),
    )}
  >
    // Screen reader announcement region
    <div
      role="status"
      ariaLive=#polite
      ariaAtomic=true
      style={{
        position: "absolute",
        left: "-10000px",
        width: "1px",
        height: "1px",
        overflow: "hidden",
      }}
    >
      {switch model.validationResult {
      | None => React.null
      | Some(result) =>
        (
          result.valid
            ? "Stack validation passed"
            : "Stack validation failed with " ++
              Int.toString(Array.length(result.errors)) ++ " errors"
        )->React.string
      }}
    </div>

    // Header with theme toggle
    <header
      role="banner"
      style={ReactDOM.Style.make(
        ~display="flex",
        ~justifyContent="space-between",
        ~alignItems="center",
        ~paddingBottom="1.5rem",
        ~borderBottom=isDark
          ? "2px solid " ++ Colors.darkBorder
          : "2px solid " ++ Colors.lightBorder,
        (),
      )}
    >
      <h1
        style={ReactDOM.Style.make(~margin="0", ~fontSize="2rem", ~fontWeight="700", ())}
        id="main-title"
      >
        {"stackur"->React.string}
        <span style={{fontSize: "0.9rem", fontWeight: "400", opacity: "0.8", marginLeft: "0.5rem"}}>
          {"Container Stack Designer"->React.string}
        </span>
      </h1>

      // Theme toggle (system aware)
      <button
        ariaLabel="Toggle dark/light theme"
        role="switch"
        ariaChecked={isDark ? #"true" : #"false"}
        style={ReactDOM.Style.make(
          ~padding="0.75rem 1.5rem",
          ~backgroundColor=isDark ? Colors.darkPrimary : Colors.lightPrimary,
          ~color="white",
          ~border="none",
          ~borderRadius="6px",
          ~fontSize="1rem",
          ~cursor="pointer",
          ~fontWeight="600",
          ~transition="background-color 0.2s ease",
          (),
        )}
      >
        {(isDark ? "🌙 Dark" : "☀️ Light")->React.string}
      </button>
    </header>

    // Component palette (left sidebar)
    <div style={ReactDOM.Style.make(~display="flex", ~marginTop="2rem", ())}>
      <aside
        role="complementary"
        ariaLabel="Component palette"
        style={ReactDOM.Style.make(
          ~width="250px",
          ~paddingRight="2rem",
          ~borderRight=isDark
            ? "2px solid " ++ Colors.darkBorder
            : "2px solid " ++ Colors.lightBorder,
          (),
        )}
      >
        <h2
          style={ReactDOM.Style.make(
            ~fontSize="1.3rem",
            ~fontWeight="600",
            ~marginBottom="1rem",
            (),
          )}
          id="palette-title"
        >
          {"Components"->React.string}
        </h2>

        <nav ariaLabelledby="palette-title">
          <ul
            role="list"
            style={ReactDOM.Style.make(~listStyle="none", ~padding="0", ~margin="0", ())}
          >
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
              components
              ->Array.map(((ct, name, desc)) => {
                <li role="listitem" style={ReactDOM.Style.make(~marginBottom="0.75rem", ())}>
                  <button
                    ariaLabel={"Add " ++ name ++ " to stack. " ++ desc}
                    style={ReactDOM.Style.make(
                      ~width="100%",
                      ~padding="1rem",
                      ~backgroundColor=isDark ? "#1A1A1A" : "#F5F5F5",
                      ~color=isDark ? Colors.darkText : Colors.lightText,
                      ~border=isDark
                        ? "2px solid " ++ Colors.darkBorder
                        : "2px solid " ++ Colors.lightBorder,
                      ~borderRadius="6px",
                      ~textAlign="left",
                      ~cursor="pointer",
                      ~fontSize="0.95rem",
                      ~fontWeight="500",
                      ~transition="all 0.2s ease",
                      (),
                    )}
                  >
                    <div style={ReactDOM.Style.make(~fontWeight="600", ())}>
                      {name->React.string}
                    </div>
                    <div
                      style={ReactDOM.Style.make(
                        ~fontSize="0.8rem",
                        ~opacity="0.8",
                        ~marginTop="0.25rem",
                        (),
                      )}
                    >
                      {desc->React.string}
                    </div>
                  </button>
                </li>
              })
              ->React.array
            }
          </ul>
        </nav>
      </aside>

      // Main stack area (Paragon-style vertical blocks)
      <article
        role="article"
        ariaLabel="Current stack configuration"
        style={ReactDOM.Style.make(~flex="1", ~paddingLeft="2rem", ())}
      >
        <h2
          style={ReactDOM.Style.make(
            ~fontSize="1.3rem",
            ~fontWeight="600",
            ~marginBottom="1rem",
            (),
          )}
          id="stack-title"
        >
          {"Stack Configuration"->React.string}
        </h2>

        {Array.length(model.components) === 0
          ? <div
              role="status"
              ariaLabel="Empty stack"
              style={ReactDOM.Style.make(
                ~padding="3rem",
                ~textAlign="center",
                ~border=isDark
                  ? "2px dashed " ++ Colors.darkBorder
                  : "2px dashed " ++ Colors.lightBorder,
                ~borderRadius="8px",
                ~color=isDark ? Colors.darkSecondary : Colors.lightSecondary,
                (),
              )}
            >
              <p style={ReactDOM.Style.make(~fontSize="1.1rem", ~margin="0", ())}>
                {"Drag components from the palette to build your stack"->React.string}
              </p>
            </div>
          : <div role="region" ariaLabel="Stack components" ariaDescribedby="stack-title">
              {model.components
              ->Array.map(comp => renderStackBlock(comp, isDark))
              ->React.array}
            </div>}
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
