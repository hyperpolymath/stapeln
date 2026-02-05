// SPDX-License-Identifier: PMPL-1.0-or-later
// CiscoView.res - Cisco Packet Tracer-style topology designer (Page 2)

open Model
open Msg

type containerShape =
  | Box
  | Oval
  | Gateway
  | Nested

type ciscoComponent = {
  id: string,
  shape: containerShape,
  label: string,
  position: position,
  size: (float, float), // width, height
  ports: array<string>,
  config: Js.Dict.t<string>,
  children: array<ciscoComponent>, // For nested containers
}

// Convert between Model.component and ciscoComponent
let toCiscoComponent = (comp: component): ciscoComponent => {
  let shape = switch comp.componentType {
  | CerroTorre | Svalinn => Gateway
  | Selur | Vordr => Box
  | Podman | Docker | Nerdctl => Box
  | Volume => Oval
  | Network => Box
  }

  {
    id: comp.id,
    shape: shape,
    label: componentTypeToString(comp.componentType),
    position: comp.position,
    size: (150.0, 100.0), // Default size
    ports: [],
    config: comp.config,
    children: [],
  }
}

// Render SVG shape based on type
let renderShape = (comp: ciscoComponent, isSelected: bool, isDark: bool) => {
  let (width, height) = comp.size
  let x = comp.position.x
  let y = comp.position.y

  let borderColor = isDark ? "#CCCCCC" : "#333333"
  let fillColor = isDark ? "#1A1A1A" : "#F5F5F5"
  let selectedBorder = isDark ? "#66B2FF" : "#0052CC"

  let strokeWidth = isSelected ? "3" : "2"
  let stroke = isSelected ? selectedBorder : borderColor

  switch comp.shape {
  | Box =>
    <rect
      x={Float.toString(x)}
      y={Float.toString(y)}
      width={Float.toString(width)}
      height={Float.toString(height)}
      fill=fillColor
      stroke=stroke
      strokeWidth=strokeWidth
      rx="8"
      role="graphics-symbol"
      ariaLabel={comp.label ++ " container (box shape)"}
    />

  | Oval =>
    <ellipse
      cx={Float.toString(x +. width /. 2.0)}
      cy={Float.toString(y +. height /. 2.0)}
      rx={Float.toString(width /. 2.0)}
      ry={Float.toString(height /. 2.0)}
      fill=fillColor
      stroke=stroke
      strokeWidth=strokeWidth
      role="graphics-symbol"
      ariaLabel={comp.label ++ " container (oval shape)"}
    />

  | Gateway =>
    <rect
      x={Float.toString(x)}
      y={Float.toString(y)}
      width={Float.toString(width)}
      height={Float.toString(height)}
      fill=fillColor
      stroke=stroke
      strokeWidth="4"
      rx="8"
      strokeDasharray="10,5"
      role="graphics-symbol"
      ariaLabel={comp.label ++ " security gateway"}
    />

  | Nested =>
    <>
      <rect
        x={Float.toString(x)}
        y={Float.toString(y)}
        width={Float.toString(width)}
        height={Float.toString(height)}
        fill=fillColor
        stroke=stroke
        strokeWidth=strokeWidth
        rx="8"
        role="graphics-symbol"
        ariaLabel={comp.label ++ " nested container group"}
      />
      // Render nested children
      {Array.mapWithIndex(comp.children, (idx, child) => {
        let childX = x +. 20.0 +. Float.fromInt(idx) *. 30.0
        let childY = y +. 40.0
        renderShape({...child, position: {x: childX, y: childY}}, false, isDark)
      })
      ->React.array}
    </>
  }
}

// Render connection line between components
let renderConnection = (conn: connection, components: array<component>, isDark: bool) => {
  let fromComp = Array.getBy(components, c => c.id === conn.from)
  let toComp = Array.getBy(components, c => c.id === conn.to)

  switch (fromComp, toComp) {
  | (Some(from), Some(to)) => {
      let x1 = from.position.x +. 75.0 // Center of component
      let y1 = from.position.y +. 50.0
      let x2 = to.position.x +. 75.0
      let y2 = to.position.y +. 50.0

      // Bezier curve path
      let midX = (x1 +. x2) /. 2.0
      let path = `M ${Float.toString(x1)} ${Float.toString(y1)} Q ${Float.toString(
          midX,
        )} ${Float.toString(y1)} ${Float.toString(midX)} ${Float.toString(
          (y1 +. y2) /. 2.0,
        )} T ${Float.toString(x2)} ${Float.toString(y2)}`

      <path
        d=path
        fill="none"
        stroke={isDark ? "#66B2FF" : "#0052CC"}
        strokeWidth="2"
        markerEnd="url(#arrowhead)"
        role="graphics-symbol"
        ariaLabel="Connection from component to component"
      />
    }
  | _ => React.null
  }
}

// Configuration panel (right sidebar)
let renderConfigPanel = (component: option<component>, isDark: bool, dispatch) => {
  switch component {
  | None =>
    <aside
      role="complementary"
      ariaLabel="Configuration panel"
      style={`
        width: 300px;
        padding: 1.5rem;
        background-color: ${isDark ? "#000000" : "#FFFFFF"};
        border-left: 2px solid ${isDark ? "#CCCCCC" : "#333333"};
        color: ${isDark ? "#FFFFFF" : "#000000"};
      `}>
      <h3 style="font-size: 1.2rem; margin-bottom: 1rem;">
        {"Configuration" ->React.string}
      </h3>
      <p style="opacity: 0.7;">
        {"Click a component to configure it" ->React.string}
      </p>
    </aside>

  | Some(comp) =>
    <aside
      role="complementary"
      ariaLabel={"Configuration for " ++ componentTypeToString(comp.componentType)}
      style={`
        width: 300px;
        padding: 1.5rem;
        background-color: ${isDark ? "#000000" : "#FFFFFF"};
        border-left: 2px solid ${isDark ? "#CCCCCC" : "#333333"};
        color: ${isDark ? "#FFFFFF" : "#000000"};
      `}>
      <h3 style="font-size: 1.2rem; margin-bottom: 1rem;">
        {componentTypeToString(comp.componentType) ->React.string}
      </h3>

      <form onSubmit={e => e->ReactEvent.Form.preventDefault}>
        // Shape selector
        <fieldset style="margin-bottom: 1rem; border: none; padding: 0;">
          <legend style="font-weight: 600; margin-bottom: 0.5rem;">
            {"Shape" ->React.string}
          </legend>
          <label style="display: block; margin-bottom: 0.5rem;">
            <input type_="radio" name="shape" value="box" defaultChecked=true />
            {" Box" ->React.string}
          </label>
          <label style="display: block; margin-bottom: 0.5rem;">
            <input type_="radio" name="shape" value="oval" />
            {" Oval" ->React.string}
          </label>
          <label style="display: block;">
            <input type_="radio" name="shape" value="gateway" />
            {" Gateway" ->React.string}
          </label>
        </fieldset>

        // Ports
        <fieldset style="margin-bottom: 1rem; border: none; padding: 0;">
          <legend style="font-weight: 600; margin-bottom: 0.5rem;">
            {"Ports" ->React.string}
          </legend>
          <input
            type_="text"
            placeholder="8080:80"
            ariaLabel="Port mapping"
            style={`
              width: 100%;
              padding: 0.5rem;
              background-color: ${isDark ? "#1A1A1A" : "#F5F5F5"};
              color: ${isDark ? "#FFFFFF" : "#000000"};
              border: 1px solid ${isDark ? "#CCCCCC" : "#333333"};
              border-radius: 4px;
            `}
          />
        </fieldset>

        // Resources
        <fieldset style="margin-bottom: 1rem; border: none; padding: 0;">
          <legend style="font-weight: 600; margin-bottom: 0.5rem;">
            {"Resources" ->React.string}
          </legend>
          <label style="display: block; margin-bottom: 0.5rem;">
            {"CPU (cores)" ->React.string}
            <input
              type_="number"
              defaultValue="1.0"
              step="0.1"
              min="0.1"
              ariaLabel="CPU cores"
              style={`
                width: 100%;
                padding: 0.5rem;
                margin-top: 0.25rem;
                background-color: ${isDark ? "#1A1A1A" : "#F5F5F5"};
                color: ${isDark ? "#FFFFFF" : "#000000"};
                border: 1px solid ${isDark ? "#CCCCCC" : "#333333"};
                border-radius: 4px;
              `}
            />
          </label>
          <label style="display: block;">
            {"Memory (MB)" ->React.string}
            <input
              type_="number"
              defaultValue="512"
              step="128"
              min="128"
              ariaLabel="Memory in megabytes"
              style={`
                width: 100%;
                padding: 0.5rem;
                margin-top: 0.25rem;
                background-color: ${isDark ? "#1A1A1A" : "#F5F5F5"};
                color: ${isDark ? "#FFFFFF" : "#000000"};
                border: 1px solid ${isDark ? "#CCCCCC" : "#333333"};
                border-radius: 4px;
              `}
            />
          </label>
        </fieldset>

        // Actions
        <div style="display: flex; gap: 0.5rem; margin-top: 1rem;">
          <button
            type_="submit"
            ariaLabel="Apply configuration changes"
            style={`
              flex: 1;
              padding: 0.75rem;
              background-color: ${isDark ? "#66B2FF" : "#0052CC"};
              color: white;
              border: none;
              border-radius: 6px;
              font-weight: 600;
              cursor: pointer;
            `}>
            {"Apply" ->React.string}
          </button>
          <button
            type_="button"
            onClick={_ => dispatch(SelectComponent(None))}
            ariaLabel="Cancel configuration"
            style={`
              flex: 1;
              padding: 0.75rem;
              background-color: ${isDark ? "#1A1A1A" : "#F5F5F5"};
              color: ${isDark ? "#FFFFFF" : "#000000"};
              border: 1px solid ${isDark ? "#CCCCCC" : "#333333"};
              border-radius: 6px;
              font-weight: 600;
              cursor: pointer;
            `}>
            {"Cancel" ->React.string}
          </button>
        </div>
      </form>
    </aside>
  }
}

// Main Cisco view
let view = (model: model, isDark: bool, dispatch) => {
  let ciscoComponents = Array.map(model.components, toCiscoComponent)

  <div
    role="main"
    ariaLabel="Cisco-style network topology designer"
    style={`
      display: flex;
      height: 100vh;
      background-color: ${isDark ? "#000000" : "#FFFFFF"};
      color: ${isDark ? "#FFFFFF" : "#000000"};
    `}>
    // Main SVG canvas
    <svg
      width="100%"
      height="100%"
      role="graphics-document"
      ariaLabel="Container network topology">
      // Define arrowhead marker for connections
      <defs>
        <marker
          id="arrowhead"
          markerWidth="10"
          markerHeight="10"
          refX="9"
          refY="3"
          orient="auto">
          <polygon points="0 0, 10 3, 0 6" fill={isDark ? "#66B2FF" : "#0052CC"} />
        </marker>
      </defs>

      // Grid background
      <pattern id="grid" width="20" height="20" patternUnits="userSpaceOnUse">
        <rect width="20" height="20" fill="transparent" />
        <path
          d="M 20 0 L 0 0 0 20"
          fill="none"
          stroke={isDark ? "#1A1A1A" : "#E0E0E0"}
          strokeWidth="0.5"
        />
      </pattern>
      <rect width="100%" height="100%" fill="url(#grid)" />

      // Render connections first (below components)
      {Array.map(model.connections, conn =>
        renderConnection(conn, model.components, isDark)
      ) |> React.array}

      // Render components
      {Array.map(ciscoComponents, comp => {
        let isSelected = switch model.selectedComponent {
        | Some(id) => id === comp.id
        | None => false
        }

        <g
          key=comp.id
          onClick={_ => dispatch(SelectComponent(Some(comp.id)))}
          style="cursor: pointer;">
          {renderShape(comp, isSelected, isDark)}
          <text
            x={Float.toString(comp.position.x +. fst(comp.size) /. 2.0)}
            y={Float.toString(comp.position.y +. snd(comp.size) /. 2.0)}
            textAnchor="middle"
            dominantBaseline="middle"
            fill={isDark ? "#FFFFFF" : "#000000"}
            style="font-size: 14px; font-weight: 600; pointer-events: none;">
            {comp.label ->React.string}
          </text>
        </g>
      }) |> React.array}
    </svg>

    // Configuration panel
    {renderConfigPanel(
      switch model.selectedComponent {
      | Some(id) => Array.getBy(model.components, c => c.id === id)
      | None => None
      },
      isDark,
      dispatch,
    )}
  </div>
}
