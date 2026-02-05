// SPDX-License-Identifier: PMPL-1.0-or-later
// LagoGreyImageDesigner.res - Interactive visual designer for Lago Grey images

// Ice formation types
type iceFormation =
  | Floe(string, int)        // < 1MB
  | Iceberg(string, int)     // 1-75MB
  | Glacier(string, int)     // 75MB+

type baseImage =
  | Distroless  // ~10MB
  | Alpine      // ~60MB
  | Scratch     // 0MB

type canvasFormation = {
  formation: iceFormation,
  selected: bool,
}

type state = {
  baseImage: baseImage,
  formations: array<canvasFormation>,
  totalSize: int,
}

let formationName = (formation: iceFormation): string => {
  switch formation {
  | Floe(name, _) => name
  | Iceberg(name, _) => name
  | Glacier(name, _) => name
  }
}

let formationSize = (formation: iceFormation): int => {
  switch formation {
  | Floe(_, size) => size
  | Iceberg(_, size) => size
  | Glacier(_, size) => size
  }
}

let formationIcon = (formation: iceFormation): string => {
  switch formation {
  | Floe(_, _) => "🧊"
  | Iceberg(_, _) => "🏔️"
  | Glacier(_, _) => "🌊"
  }
}

let formatBytes = (bytes: int): string => {
  if bytes < 1024 {
    Int.toString(bytes) ++ " B"
  } else if bytes < 1024 * 1024 {
    Float.toString(Float.fromInt(bytes) /. 1024.0) ++ " KB"
  } else {
    Float.toString(Float.fromInt(bytes) /. 1024.0 /. 1024.0) ++ " MB"
  }
}

// Available components (catalog)
let availableFloes = [
  Floe("hello", 29_000),
  Floe("libsodium", 506_000),
  Floe("libargon2", 42_000),
  Floe("obli-pkg", 56_000),
]

let availableIcebergs = [
  Iceberg("liboqs", 11_000_000),
]

@react.component
let make = () => {
  let (state, setState) = React.useState(() => {
    baseImage: Distroless,
    formations: [],
    totalSize: 10_000_000, // Distroless base
  })

  let toggleFormation = (formation: iceFormation) => {
    setState(prev => {
      let exists = Array.some(prev.formations, cf =>
        formationName(cf.formation) == formationName(formation)
      )

      if exists {
        // Remove it
        {
          ...prev,
          formations: Array.filter(prev.formations, cf =>
            formationName(cf.formation) != formationName(formation)
          ),
          totalSize: prev.totalSize - formationSize(formation),
        }
      } else {
        // Add it
        {
          ...prev,
          formations: Array.concat(prev.formations, [{formation, selected: false}]),
          totalSize: prev.totalSize + formationSize(formation),
        }
      }
    })
  }

  let setBaseImage = (base: baseImage) => {
    setState(prev => {
      let baseSize = switch base {
      | Distroless => 10_000_000
      | Alpine => 60_000_000
      | Scratch => 0
      }

      let formationsSize = Array.reduce(prev.formations, 0, (acc, cf) =>
        acc + formationSize(cf.formation)
      )

      {
        ...prev,
        baseImage: base,
        totalSize: baseSize + formationsSize,
      }
    })
  }

  let isFormationSelected = (formation: iceFormation): bool => {
    Array.some(state.formations, cf =>
      formationName(cf.formation) == formationName(formation)
    )
  }

  <div className="lago-grey-designer">
    <header className="designer-header">
      <h1> {"🌊 Lago Grey Image Designer"->React.string} </h1>
      <p className="tagline">
        {"Build minimal Linux distributions by assembling ice formations"->React.string}
      </p>
    </header>

    <div className="designer-layout">
      {/* Left sidebar - Component catalog */}
      <div className="sidebar">
        <div className="section">
          <h2> {"Base Image"->React.string} </h2>
          <div className="base-options">
            <button
              className={state.baseImage == Distroless ? "base-btn active" : "base-btn"}
              onClick={_ => setBaseImage(Distroless)}>
              <div className="base-name"> {"Distroless"->React.string} </div>
              <div className="base-size"> {"~10 MB, ~20 files"->React.string} </div>
            </button>
            <button
              className={state.baseImage == Alpine ? "base-btn active" : "base-btn"}
              onClick={_ => setBaseImage(Alpine)}>
              <div className="base-name"> {"Alpine"->React.string} </div>
              <div className="base-size"> {"~60 MB, ~5,000 files"->React.string} </div>
            </button>
            <button
              className={state.baseImage == Scratch ? "base-btn active" : "base-btn"}
              onClick={_ => setBaseImage(Scratch)}>
              <div className="base-name"> {"Scratch"->React.string} </div>
              <div className="base-size"> {"0 MB (empty)"->React.string} </div>
            </button>
          </div>
        </div>

        <div className="section">
          <h2> {"🧊 Floes (< 1MB)"->React.string} </h2>
          <div className="component-list">
            {Array.map(availableFloes, floe => {
              let selected = isFormationSelected(floe)
              <button
                key={formationName(floe)}
                className={selected ? "component-btn selected" : "component-btn"}
                onClick={_ => toggleFormation(floe)}>
                <span className="component-icon"> {formationIcon(floe)->React.string} </span>
                <span className="component-name"> {formationName(floe)->React.string} </span>
                <span className="component-size"> {formatBytes(formationSize(floe))->React.string} </span>
              </button>
            })->React.array}
          </div>
        </div>

        <div className="section">
          <h2> {"🏔️ Icebergs (1-75MB)"->React.string} </h2>
          <div className="component-list">
            {Array.map(availableIcebergs, iceberg => {
              let selected = isFormationSelected(iceberg)
              <button
                key={formationName(iceberg)}
                className={selected ? "component-btn selected" : "component-btn"}
                onClick={_ => toggleFormation(iceberg)}>
                <span className="component-icon"> {formationIcon(iceberg)->React.string} </span>
                <span className="component-name"> {formationName(iceberg)->React.string} </span>
                <span className="component-size"> {formatBytes(formationSize(iceberg))->React.string} </span>
              </button>
            })->React.array}
          </div>
        </div>
      </div>

      {/* Center - Canvas/Preview */}
      <div className="canvas">
        <div className="canvas-header">
          <h2> {"Your Lago Grey Image"->React.string} </h2>
          <div className="size-display">
            <span className="size-label"> {"Total Size:"->React.string} </span>
            <span className="size-value"> {formatBytes(state.totalSize)->React.string} </span>
            <span className={
              state.totalSize <= 17_500_000 ? "classification success" : "classification warning"
            }>
              {
                if state.totalSize < 1_000_000 {
                  "🧊 Floe"
                } else if state.totalSize <= 75_000_000 {
                  "🏔️ Small Iceberg"
                } else {
                  "🌊 Glacier"
                }->React.string
              }
            </span>
          </div>
        </div>

        <div className="canvas-content">
          {/* Base layer */}
          <div className="layer base-layer">
            <div className="layer-icon"> {"📦"->React.string} </div>
            <div className="layer-info">
              <div className="layer-name">
                {switch state.baseImage {
                | Distroless => "Distroless Base"
                | Alpine => "Alpine Base"
                | Scratch => "Scratch (Empty)"
                }->React.string}
              </div>
              <div className="layer-size">
                {switch state.baseImage {
                | Distroless => "10 MB"
                | Alpine => "60 MB"
                | Scratch => "0 MB"
                }->React.string}
              </div>
            </div>
          </div>

          {/* Ice formations */}
          {Array.map(state.formations, cf => {
            <div key={formationName(cf.formation)} className="layer formation-layer">
              <div className="layer-icon"> {formationIcon(cf.formation)->React.string} </div>
              <div className="layer-info">
                <div className="layer-name"> {formationName(cf.formation)->React.string} </div>
                <div className="layer-size"> {formatBytes(formationSize(cf.formation))->React.string} </div>
              </div>
            </div>
          })->React.array}

          {/* Empty state */}
          {Array.length(state.formations) == 0
            ? <div className="empty-state">
                <p> {"👈 Select components from the sidebar to add them"->React.string} </p>
                <p className="hint"> {"Click on Floes or Icebergs to build your image"->React.string} </p>
              </div>
            : React.null}
        </div>

        <div className="canvas-footer">
          <div className="comparison">
            <h3> {"Competitive Position"->React.string} </h3>
            <div className="comparison-grid">
              <div className="comparison-item">
                <span className="label"> {"vs Alpine (60 MB):"->React.string} </span>
                <span className="value">
                  {(state.totalSize <= 60_000_000
                    ? Float.toString((1.0 -. Float.fromInt(state.totalSize) /. 60_000_000) *. 100.0) ++ "% smaller ✅"
                    : Float.toString((Float.fromInt(state.totalSize) /. 60_000_000 -. 1.0) *. 100.0) ++ "% larger ⚠️"
                  )->React.string}
                </span>
              </div>
              <div className="comparison-item">
                <span className="label"> {"Target (17.5 MB):"->React.string} </span>
                <span className={state.totalSize <= 17_500_000 ? "value success" : "value warning"}>
                  {(state.totalSize <= 17_500_000
                    ? "✅ Under target!"
                    : "⚠️ " ++ formatBytes(state.totalSize - 17_500_000) ++ " over"
                  )->React.string}
                </span>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Right sidebar - Info & Export */}
      <div className="info-panel">
        <div className="section">
          <h2> {"Ice Formation Guide"->React.string} </h2>
          <div className="guide">
            <div className="guide-item">
              <span className="guide-icon"> {"🧊"->React.string} </span>
              <div>
                <div className="guide-title"> {"Floe"->React.string} </div>
                <div className="guide-desc"> {"< 1MB - Small components"->React.string} </div>
              </div>
            </div>
            <div className="guide-item">
              <span className="guide-icon"> {"🏔️"->React.string} </span>
              <div>
                <div className="guide-title"> {"Iceberg"->React.string} </div>
                <div className="guide-desc"> {"1-75MB - Medium chunks"->React.string} </div>
              </div>
            </div>
            <div className="guide-item">
              <span className="guide-icon"> {"🌊"->React.string} </span>
              <div>
                <div className="guide-title"> {"Glacier"->React.string} </div>
                <div className="guide-desc"> {"75MB+ - Large masses"->React.string} </div>
              </div>
            </div>
          </div>
        </div>

        <div className="section">
          <h2> {"Export"->React.string} </h2>
          <div className="export-buttons">
            <button className="export-btn" disabled=true>
              {"📄 Dockerfile"->React.string}
            </button>
            <button className="export-btn" disabled=true>
              {"📦 .zpkg Package"->React.string}
            </button>
            <button className="export-btn" disabled=true>
              {"⚙️ Nickel Config"->React.string}
            </button>
          </div>
          <p className="export-note"> {"(Export coming soon)"->React.string} </p>
        </div>

        <div className="section">
          <h2> {"Security"->React.string} </h2>
          <div className="security-info">
            {isFormationSelected(Iceberg("liboqs", 11_000_000))
              ? <div className="security-item enabled">
                  {"✅ Post-quantum crypto"->React.string}
                </div>
              : <div className="security-item disabled">
                  {"⚠️ No PQ crypto (add liboqs)"->React.string}
                </div>}
            {isFormationSelected(Floe("libsodium", 506_000))
              ? <div className="security-item enabled">
                  {"✅ Classical crypto"->React.string}
                </div>
              : <div className="security-item disabled">
                  {"⚠️ No classical crypto"->React.string}
                </div>}
          </div>
        </div>
      </div>
    </div>
  </div>
}
