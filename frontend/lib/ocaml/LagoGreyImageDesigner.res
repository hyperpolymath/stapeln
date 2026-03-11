// SPDX-License-Identifier: PMPL-1.0-or-later
// LagoGreyImageDesigner.res - Interactive visual designer for Lago Grey images
// Features: base image selection, formation catalog, drag reorder, custom formations,
// security indicators, multiple export formats, size visualization

// Ice formation types
type iceFormation =
  | Floe(string, int) // < 1MB
  | Iceberg(string, int) // 1-75MB
  | Glacier(string, int) // 75MB+

type baseImage =
  | Distroless // ~10MB
  | Alpine // ~60MB
  | Scratch // 0MB

type canvasFormation = {
  formation: iceFormation,
  selected: bool,
}

type designerView =
  | Catalog // Browse and add formations
  | LayerEditor // Reorder and edit layers
  | CustomFormation // Add a custom formation

type state = {
  baseImage: baseImage,
  formations: array<canvasFormation>,
  totalSize: int,
  activeView: designerView,
  customName: string,
  customSize: int,
  customType: string, // "floe", "iceberg", "glacier"
  dragIndex: option<int>,
  hoveredLayer: option<int>,
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
  | Floe(_, _) => "F"
  | Iceberg(_, _) => "I"
  | Glacier(_, _) => "G"
  }
}

let formationTypeLabel = (formation: iceFormation): string => {
  switch formation {
  | Floe(_, _) => "Floe"
  | Iceberg(_, _) => "Iceberg"
  | Glacier(_, _) => "Glacier"
  }
}

let formatBytes = (bytes: int): string => {
  if bytes < 1024 {
    Int.toString(bytes) ++ " B"
  } else if bytes < 1024 * 1024 {
    let kb = Float.fromInt(bytes) /. 1024.0
    let rounded = Float.toInt(kb *. 10.0)->Float.fromInt /. 10.0
    Float.toString(rounded) ++ " KB"
  } else {
    let mb = Float.fromInt(bytes) /. 1024.0 /. 1024.0
    let rounded = Float.toInt(mb *. 100.0)->Float.fromInt /. 100.0
    Float.toString(rounded) ++ " MB"
  }
}

// Available components (catalog)
let availableFloes = [
  Floe("hello", 29_000),
  Floe("libsodium", 506_000),
  Floe("libargon2", 42_000),
  Floe("obli-pkg", 56_000),
  Floe("curl-minimal", 280_000),
  Floe("ca-certificates", 200_000),
]

let availableIcebergs = [
  Iceberg("liboqs", 11_000_000),
  Iceberg("openssl", 5_000_000),
  Iceberg("busybox", 2_000_000),
]

let availableGlaciers = [
  Glacier("python-runtime", 120_000_000),
  Glacier("nodejs-runtime", 95_000_000),
]

// Recalculate total size from base + formations
let recalcSize = (base: baseImage, formations: array<canvasFormation>): int => {
  let baseSize = switch base {
  | Distroless => 10_000_000
  | Alpine => 60_000_000
  | Scratch => 0
  }
  let formationsSize = Array.reduce(formations, 0, (acc, cf) =>
    acc + formationSize(cf.formation)
  )
  baseSize + formationsSize
}

@react.component
let make = () => {
  let (state, setState) = React.useState(() => {
    baseImage: Distroless,
    formations: [],
    totalSize: 10_000_000, // Distroless base
    activeView: Catalog,
    customName: "",
    customSize: 100_000,
    customType: "floe",
    dragIndex: None,
    hoveredLayer: None,
  })

  let toggleFormation = (formation: iceFormation) => {
    setState(prev => {
      let exists = Array.some(prev.formations, cf =>
        formationName(cf.formation) == formationName(formation)
      )

      let newFormations = if exists {
        Belt.Array.keep(prev.formations, cf =>
          formationName(cf.formation) != formationName(formation)
        )
      } else {
        Array.concat(prev.formations, [{formation, selected: false}])
      }

      {
        ...prev,
        formations: newFormations,
        totalSize: recalcSize(prev.baseImage, newFormations),
      }
    })
  }

  let removeFormation = (idx: int) => {
    setState(prev => {
      let newFormations = Array.keepWithIndex(prev.formations, (_cf, i) => i !== idx)
      {
        ...prev,
        formations: newFormations,
        totalSize: recalcSize(prev.baseImage, newFormations),
      }
    })
  }

  let moveFormation = (fromIdx: int, toIdx: int) => {
    setState(prev => {
      if fromIdx === toIdx || fromIdx < 0 || toIdx < 0 {
        prev
      } else {
        let arr = Array.copy(prev.formations)
        let item = arr[fromIdx]
        switch item {
        | Some(formation) =>
          // Remove from old position
          let without = Array.keepWithIndex(arr, (_cf, i) => i !== fromIdx)
          // Insert at new position
          let before = Array.slice(without, ~offset=0, ~len=toIdx)
          let after = Array.slice(without, ~offset=toIdx, ~len=Array.length(without) - toIdx)
          let newFormations = Array.concatMany([before, [formation], after])
          {
            ...prev,
            formations: newFormations,
            dragIndex: None,
            hoveredLayer: None,
          }
        | None => prev
        }
      }
    })
  }

  let addCustomFormation = () => {
    setState(prev => {
      if prev.customName === "" {
        prev
      } else {
        let formation = switch prev.customType {
        | "iceberg" => Iceberg(prev.customName, prev.customSize)
        | "glacier" => Glacier(prev.customName, prev.customSize)
        | _ => Floe(prev.customName, prev.customSize)
        }
        let newFormations = Array.concat(prev.formations, [{formation, selected: false}])
        {
          ...prev,
          formations: newFormations,
          totalSize: recalcSize(prev.baseImage, newFormations),
          customName: "",
          customSize: 100_000,
          activeView: Catalog,
        }
      }
    })
  }

  let setBaseImage = (base: baseImage) => {
    setState(prev => {
      {
        ...prev,
        baseImage: base,
        totalSize: recalcSize(base, prev.formations),
      }
    })
  }

  let isFormationSelected = (formation: iceFormation): bool => {
    Array.some(state.formations, cf => formationName(cf.formation) == formationName(formation))
  }

  let sizeBarWidth = (size: int, maxSize: int): string => {
    let pct = Float.fromInt(size) /. Float.fromInt(maxSize) *. 100.0
    let clamped = Math.min(pct, 100.0)
    Float.toString(clamped) ++ "%"
  }

  <div className="lago-grey-designer">
    <header className="designer-header">
      <h1> {"Lago Grey Image Designer"->React.string} </h1>
      <p className="tagline">
        {"Build minimal Linux distributions by assembling ice formations"->React.string}
      </p>
    </header>

    <div className="designer-layout">
      // Left sidebar - catalog and controls
      <div className="sidebar">
        // View tabs
        <div className="view-tabs" style={{display: "flex", gap: "0.25rem", marginBottom: "1rem"}}>
          <button
            className={state.activeView == Catalog ? "tab-btn active" : "tab-btn"}
            onClick={_ => setState(prev => {...prev, activeView: Catalog})}
          >
            {"Catalog"->React.string}
          </button>
          <button
            className={state.activeView == LayerEditor ? "tab-btn active" : "tab-btn"}
            onClick={_ => setState(prev => {...prev, activeView: LayerEditor})}
          >
            {"Layers"->React.string}
          </button>
          <button
            className={state.activeView == CustomFormation ? "tab-btn active" : "tab-btn"}
            onClick={_ => setState(prev => {...prev, activeView: CustomFormation})}
          >
            {"+ Custom"->React.string}
          </button>
        </div>

        {switch state.activeView {
        | Catalog =>
          <>
            <div className="section">
              <h2> {"Base Image"->React.string} </h2>
              <div className="base-options">
                <button
                  className={state.baseImage == Distroless ? "base-btn active" : "base-btn"}
                  onClick={_ => setBaseImage(Distroless)}
                >
                  <div className="base-name"> {"Distroless"->React.string} </div>
                  <div className="base-size"> {"~10 MB, ~20 files"->React.string} </div>
                </button>
                <button
                  className={state.baseImage == Alpine ? "base-btn active" : "base-btn"}
                  onClick={_ => setBaseImage(Alpine)}
                >
                  <div className="base-name"> {"Alpine"->React.string} </div>
                  <div className="base-size"> {"~60 MB, ~5,000 files"->React.string} </div>
                </button>
                <button
                  className={state.baseImage == Scratch ? "base-btn active" : "base-btn"}
                  onClick={_ => setBaseImage(Scratch)}
                >
                  <div className="base-name"> {"Scratch"->React.string} </div>
                  <div className="base-size"> {"0 MB (empty)"->React.string} </div>
                </button>
              </div>
            </div>

            <div className="section">
              <h2> {"Floes (< 1MB)"->React.string} </h2>
              <div className="component-list">
                {Array.map(availableFloes, floe => {
                  let selected = isFormationSelected(floe)
                  <button
                    key={formationName(floe)}
                    className={selected ? "component-btn selected" : "component-btn"}
                    onClick={_ => toggleFormation(floe)}
                  >
                    <span className="component-icon"> {formationIcon(floe)->React.string} </span>
                    <span className="component-name"> {formationName(floe)->React.string} </span>
                    <span className="component-size">
                      {formatBytes(formationSize(floe))->React.string}
                    </span>
                  </button>
                })->React.array}
              </div>
            </div>

            <div className="section">
              <h2> {"Icebergs (1-75MB)"->React.string} </h2>
              <div className="component-list">
                {Array.map(availableIcebergs, iceberg => {
                  let selected = isFormationSelected(iceberg)
                  <button
                    key={formationName(iceberg)}
                    className={selected ? "component-btn selected" : "component-btn"}
                    onClick={_ => toggleFormation(iceberg)}
                  >
                    <span className="component-icon"> {formationIcon(iceberg)->React.string} </span>
                    <span className="component-name"> {formationName(iceberg)->React.string} </span>
                    <span className="component-size">
                      {formatBytes(formationSize(iceberg))->React.string}
                    </span>
                  </button>
                })->React.array}
              </div>
            </div>

            <div className="section">
              <h2> {"Glaciers (75MB+)"->React.string} </h2>
              <div className="component-list">
                {Array.map(availableGlaciers, glacier => {
                  let selected = isFormationSelected(glacier)
                  <button
                    key={formationName(glacier)}
                    className={selected ? "component-btn selected" : "component-btn"}
                    onClick={_ => toggleFormation(glacier)}
                  >
                    <span className="component-icon"> {formationIcon(glacier)->React.string} </span>
                    <span className="component-name"> {formationName(glacier)->React.string} </span>
                    <span className="component-size">
                      {formatBytes(formationSize(glacier))->React.string}
                    </span>
                  </button>
                })->React.array}
              </div>
            </div>
          </>

        | LayerEditor =>
          <div className="section">
            <h2> {"Layer Order"->React.string} </h2>
            <p className="hint" style={{marginBottom: "0.5rem", fontSize: "0.85rem", opacity: "0.7"}}>
              {"Click arrows to reorder. Click X to remove."->React.string}
            </p>
            {Array.length(state.formations) === 0
              ? <p className="empty-state"> {"No formations added yet."->React.string} </p>
              : <div className="layer-editor-list">
                  {Array.mapWithIndex(state.formations, (idx, cf) => {
                    let isHovered = state.hoveredLayer === Some(idx)
                    <div
                      key={formationName(cf.formation) ++ Int.toString(idx)}
                      className={isHovered ? "layer-editor-item hovered" : "layer-editor-item"}
                      onMouseEnter={_ => setState(prev => {...prev, hoveredLayer: Some(idx)})}
                      onMouseLeave={_ => setState(prev => {...prev, hoveredLayer: None})}
                      style={{
                        display: "flex",
                        alignItems: "center",
                        gap: "0.5rem",
                        padding: "0.5rem",
                        borderBottom: "1px solid rgba(128,128,128,0.3)",
                      }}
                    >
                      <div style={{display: "flex", flexDirection: "column", gap: "2px"}}>
                        <button
                          disabled={idx === 0}
                          onClick={_ => moveFormation(idx, idx - 1)}
                          style={{
                            padding: "0 4px",
                            cursor: idx === 0 ? "default" : "pointer",
                            opacity: idx === 0 ? "0.3" : "1",
                            border: "none",
                            background: "transparent",
                            color: "inherit",
                          }}
                          ariaLabel="Move layer up"
                        >
                          {"^"->React.string}
                        </button>
                        <button
                          disabled={idx === Array.length(state.formations) - 1}
                          onClick={_ => moveFormation(idx, idx + 1)}
                          style={{
                            padding: "0 4px",
                            cursor: idx === Array.length(state.formations) - 1
                              ? "default"
                              : "pointer",
                            opacity: idx === Array.length(state.formations) - 1 ? "0.3" : "1",
                            border: "none",
                            background: "transparent",
                            color: "inherit",
                          }}
                          ariaLabel="Move layer down"
                        >
                          {"v"->React.string}
                        </button>
                      </div>
                      <span style={{fontWeight: "600", minWidth: "20px"}}>
                        {formationIcon(cf.formation)->React.string}
                      </span>
                      <span style={{flex: "1"}}>
                        {formationName(cf.formation)->React.string}
                      </span>
                      <span style={{opacity: "0.6", fontSize: "0.85rem"}}>
                        {formatBytes(formationSize(cf.formation))->React.string}
                      </span>
                      <button
                        onClick={_ => removeFormation(idx)}
                        ariaLabel={"Remove " ++ formationName(cf.formation)}
                        style={{
                          padding: "2px 6px",
                          cursor: "pointer",
                          border: "none",
                          background: "transparent",
                          color: "#CC3333",
                          fontWeight: "bold",
                        }}
                      >
                        {"X"->React.string}
                      </button>
                    </div>
                  })->React.array}
                </div>}
          </div>

        | CustomFormation =>
          <div className="section">
            <h2> {"Add Custom Formation"->React.string} </h2>
            <div style={{display: "flex", flexDirection: "column", gap: "0.75rem"}}>
              <label>
                {"Name"->React.string}
                <input
                  type_="text"
                  value=state.customName
                  onChange={e => {
                    let value = ReactEvent.Form.target(e)["value"]
                    setState(prev => {...prev, customName: value})
                  }}
                  placeholder="my-package"
                  style={{
                    width: "100%",
                    padding: "0.5rem",
                    marginTop: "0.25rem",
                    borderRadius: "4px",
                    border: "1px solid rgba(128,128,128,0.5)",
                    background: "transparent",
                    color: "inherit",
                  }}
                />
              </label>
              <label>
                {"Size (bytes)"->React.string}
                <input
                  type_="number"
                  value={Int.toString(state.customSize)}
                  onChange={e => {
                    let value = ReactEvent.Form.target(e)["value"]
                    let size = switch Int.fromString(value) {
                    | Some(n) => n
                    | None => 0
                    }
                    setState(prev => {...prev, customSize: size})
                  }}
                  min="1"
                  style={{
                    width: "100%",
                    padding: "0.5rem",
                    marginTop: "0.25rem",
                    borderRadius: "4px",
                    border: "1px solid rgba(128,128,128,0.5)",
                    background: "transparent",
                    color: "inherit",
                  }}
                />
              </label>
              <label>
                {"Type"->React.string}
                <select
                  value=state.customType
                  onChange={e => {
                    let value = ReactEvent.Form.target(e)["value"]
                    setState(prev => {...prev, customType: value})
                  }}
                  style={{
                    width: "100%",
                    padding: "0.5rem",
                    marginTop: "0.25rem",
                    borderRadius: "4px",
                    border: "1px solid rgba(128,128,128,0.5)",
                    background: "transparent",
                    color: "inherit",
                  }}
                >
                  <option value="floe"> {"Floe (< 1MB)"->React.string} </option>
                  <option value="iceberg"> {"Iceberg (1-75MB)"->React.string} </option>
                  <option value="glacier"> {"Glacier (75MB+)"->React.string} </option>
                </select>
              </label>
              <button
                onClick={_ => addCustomFormation()}
                className="export-btn"
                disabled={state.customName === ""}
              >
                {"Add Formation"->React.string}
              </button>
            </div>
          </div>
        }}
      </div>

      // Main canvas - layer visualization
      <div className="canvas">
        <div className="canvas-header">
          <h2> {"Your Lago Grey Image"->React.string} </h2>
          <div className="size-display">
            <span className="size-label"> {"Total Size:"->React.string} </span>
            <span className="size-value"> {formatBytes(state.totalSize)->React.string} </span>
            <span
              className={state.totalSize <= 17_500_000
                ? "classification success"
                : "classification warning"}
            >
              {if state.totalSize < 1_000_000 {
                "Floe"
              } else if state.totalSize <= 75_000_000 {
                "Small Iceberg"
              } else {
                "Glacier"
              }->React.string}
            </span>
          </div>
        </div>

        <div className="canvas-content">
          // Base layer
          <div className="layer base-layer">
            <div className="layer-icon"> {"B"->React.string} </div>
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
            // Size bar
            <div
              style={{
                height: "4px",
                backgroundColor: "rgba(128,128,128,0.2)",
                borderRadius: "2px",
                marginTop: "4px",
                width: "100%",
              }}
            >
              <div
                style={{
                  height: "4px",
                  backgroundColor: "#4CAF50",
                  borderRadius: "2px",
                  width: sizeBarWidth(
                    switch state.baseImage {
                    | Distroless => 10_000_000
                    | Alpine => 60_000_000
                    | Scratch => 0
                    },
                    Math.Int.max(state.totalSize, 1),
                  ),
                }}
              />
            </div>
          </div>

          // Formation layers
          {Array.mapWithIndex(state.formations, (idx, cf) => {
            <div key={formationName(cf.formation) ++ Int.toString(idx)} className="layer formation-layer">
              <div className="layer-icon"> {formationIcon(cf.formation)->React.string} </div>
              <div className="layer-info">
                <div className="layer-name">
                  {(formationName(cf.formation) ++ " (" ++ formationTypeLabel(cf.formation) ++ ")")->React.string}
                </div>
                <div className="layer-size">
                  {formatBytes(formationSize(cf.formation))->React.string}
                </div>
              </div>
              // Size bar
              <div
                style={{
                  height: "4px",
                  backgroundColor: "rgba(128,128,128,0.2)",
                  borderRadius: "2px",
                  marginTop: "4px",
                  width: "100%",
                }}
              >
                <div
                  style={{
                    height: "4px",
                    backgroundColor: switch cf.formation {
                    | Floe(_, _) => "#2196F3"
                    | Iceberg(_, _) => "#FF9800"
                    | Glacier(_, _) => "#F44336"
                    },
                    borderRadius: "2px",
                    width: sizeBarWidth(formationSize(cf.formation), Math.Int.max(state.totalSize, 1)),
                  }}
                />
              </div>
            </div>
          })->React.array}

          {Array.length(state.formations) == 0
            ? <div className="empty-state">
                <p> {"Select components from the sidebar to add them"->React.string} </p>
                <p className="hint">
                  {"Click on Floes, Icebergs, or Glaciers to build your image"->React.string}
                </p>
              </div>
            : React.null}
        </div>

        // Footer with comparisons
        <div className="canvas-footer">
          <div className="comparison">
            <h3> {"Competitive Position"->React.string} </h3>
            <div className="comparison-grid">
              <div className="comparison-item">
                <span className="label"> {"vs Alpine (60 MB):"->React.string} </span>
                <span className="value">
                  {(
                    state.totalSize <= 60_000_000
                      ? Float.toString(
                          (1.0 -. Float.fromInt(state.totalSize) /. 60_000_000.) *. 100.0,
                        ) ++ "% smaller"
                      : Float.toString(
                          (Float.fromInt(state.totalSize) /. 60_000_000. -. 1.0) *. 100.0,
                        ) ++ "% larger"
                  )->React.string}
                </span>
              </div>
              <div className="comparison-item">
                <span className="label"> {"Target (17.5 MB):"->React.string} </span>
                <span className={state.totalSize <= 17_500_000 ? "value success" : "value warning"}>
                  {(
                    state.totalSize <= 17_500_000
                      ? "Under target!"
                      : formatBytes(state.totalSize - 17_500_000) ++ " over"
                  )->React.string}
                </span>
              </div>
            </div>
          </div>
        </div>
      </div>

      // Right info panel
      <div className="info-panel">
        <div className="section">
          <h2> {"Ice Formation Guide"->React.string} </h2>
          <div className="guide">
            <div className="guide-item">
              <span className="guide-icon"> {"F"->React.string} </span>
              <div>
                <div className="guide-title"> {"Floe"->React.string} </div>
                <div className="guide-desc"> {"< 1MB - Small components"->React.string} </div>
              </div>
            </div>
            <div className="guide-item">
              <span className="guide-icon"> {"I"->React.string} </span>
              <div>
                <div className="guide-title"> {"Iceberg"->React.string} </div>
                <div className="guide-desc"> {"1-75MB - Medium chunks"->React.string} </div>
              </div>
            </div>
            <div className="guide-item">
              <span className="guide-icon"> {"G"->React.string} </span>
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
            <button
              className="export-btn"
              onClick={_ => {
                let exportState: LagoGreyExport.lagoGreyState = {
                  baseImage: switch state.baseImage {
                  | Distroless => LagoGreyExport.Distroless
                  | Alpine => LagoGreyExport.Alpine
                  | Scratch => LagoGreyExport.Scratch
                  },
                  formations: (Array.map(state.formations, cf => cf.formation) :> array<
                    LagoGreyExport.iceFormation,
                  >),
                  totalSize: state.totalSize,
                }
                LagoGreyExport.exportDockerfile(exportState)
              }}
            >
              {"Containerfile"->React.string}
            </button>
            <button
              className="export-btn"
              onClick={_ => {
                let exportState: LagoGreyExport.lagoGreyState = {
                  baseImage: switch state.baseImage {
                  | Distroless => LagoGreyExport.Distroless
                  | Alpine => LagoGreyExport.Alpine
                  | Scratch => LagoGreyExport.Scratch
                  },
                  formations: (Array.map(state.formations, cf => cf.formation) :> array<
                    LagoGreyExport.iceFormation,
                  >),
                  totalSize: state.totalSize,
                }
                LagoGreyExport.exportCompletePackage(exportState)
              }}
            >
              {"Complete Package"->React.string}
            </button>
            <button
              className="export-btn"
              onClick={_ => {
                let exportState: LagoGreyExport.lagoGreyState = {
                  baseImage: switch state.baseImage {
                  | Distroless => LagoGreyExport.Distroless
                  | Alpine => LagoGreyExport.Alpine
                  | Scratch => LagoGreyExport.Scratch
                  },
                  formations: (Array.map(state.formations, cf => cf.formation) :> array<
                    LagoGreyExport.iceFormation,
                  >),
                  totalSize: state.totalSize,
                }
                LagoGreyExport.exportManifest(exportState)
              }}
            >
              {"Manifest JSON"->React.string}
            </button>
          </div>
          <p className="export-note">
            {"Click to download. Run 'podman build' to create image."->React.string}
          </p>
        </div>

        <div className="section">
          <h2> {"Security"->React.string} </h2>
          <div className="security-info">
            {isFormationSelected(Iceberg("liboqs", 11_000_000))
              ? <div className="security-item enabled">
                  {"[OK] Post-quantum crypto"->React.string}
                </div>
              : <div className="security-item disabled">
                  {"[--] No PQ crypto (add liboqs)"->React.string}
                </div>}
            {isFormationSelected(Floe("libsodium", 506_000))
              ? <div className="security-item enabled">
                  {"[OK] Classical crypto"->React.string}
                </div>
              : <div className="security-item disabled">
                  {"[--] No classical crypto"->React.string}
                </div>}
            {isFormationSelected(Floe("ca-certificates", 200_000))
              ? <div className="security-item enabled">
                  {"[OK] CA certificates"->React.string}
                </div>
              : <div className="security-item disabled">
                  {"[--] No CA certs (add ca-certificates)"->React.string}
                </div>}
          </div>
        </div>

        <div className="section">
          <h2> {"Layer Summary"->React.string} </h2>
          <div style={{fontSize: "0.85rem"}}>
            <p>
              {("Layers: " ++ Int.toString(Array.length(state.formations) + 1))->React.string}
            </p>
            <p>
              {("Formations: " ++ Int.toString(Array.length(state.formations)))->React.string}
            </p>
            <p>
              {("Floes: " ++
              Int.toString(
                Array.reduce(state.formations, 0, (acc, cf) =>
                  switch cf.formation {
                  | Floe(_, _) => acc + 1
                  | _ => acc
                  }
                ),
              ))->React.string}
            </p>
            <p>
              {("Icebergs: " ++
              Int.toString(
                Array.reduce(state.formations, 0, (acc, cf) =>
                  switch cf.formation {
                  | Iceberg(_, _) => acc + 1
                  | _ => acc
                  }
                ),
              ))->React.string}
            </p>
            <p>
              {("Glaciers: " ++
              Int.toString(
                Array.reduce(state.formations, 0, (acc, cf) =>
                  switch cf.formation {
                  | Glacier(_, _) => acc + 1
                  | _ => acc
                  }
                ),
              ))->React.string}
            </p>
          </div>
        </div>
      </div>
    </div>
  </div>
}
