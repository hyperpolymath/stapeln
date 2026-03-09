// SPDX-License-Identifier: PMPL-1.0-or-later
// TopologyView.res - Network topology designer with Cisco Packet Tracer-style layout
// Features: snap-to-grid, connection drawing, delete confirmation, undo support

open Model
open Msg

// ---------------------------------------------------------------------------
// Snap-to-grid configuration
// ---------------------------------------------------------------------------

let gridSize = 20.0

let snapToGrid = (pos: position): position => {
  {
    x: Math.round(pos.x /. gridSize) *. gridSize,
    y: Math.round(pos.y /. gridSize) *. gridSize,
  }
}

// ---------------------------------------------------------------------------
// Undo history (local to this view — tracks component positions and deletions)
// ---------------------------------------------------------------------------

type undoEntry =
  | MovedComponent(string, position, position) // id, oldPos, newPos
  | DeletedComponent(component)
  | DeletedConnection(connection)
  | AddedConnection(string) // connection id

type canvasState = {
  undoStack: array<undoEntry>,
  connectionDrawing: option<(string, position)>, // (fromComponentId, currentMousePos)
  deleteConfirm: option<string>, // component id pending delete
}

let initialCanvasState: canvasState = {
  undoStack: [],
  connectionDrawing: None,
  deleteConfirm: None,
}

type containerShape =
  | Box
  | Oval
  | Gateway
  | Nested

type rec ciscoComponent = {
  id: string,
  shape: containerShape,
  label: string,
  position: position,
  size: (float, float), // width, height
  ports: array<string>,
  config: dict<string>,
  children: array<ciscoComponent>, // For nested containers
}

// Convert between Model.component and ciscoComponent
let toCiscoComponent = (comp: component): ciscoComponent => {
  let shape = switch comp.componentType {
  | CerroTorre | Svalinn => Gateway
  | LagoGrey => Box
  | Selur | Vordr => Box
  | Podman | Docker | Nerdctl => Box
  | Volume => Oval
  | Network => Box
  }

  {
    id: comp.id,
    shape,
    label: componentTypeToString(comp.componentType),
    position: comp.position,
    size: (150.0, 100.0), // Default size
    ports: [],
    config: comp.config,
    children: [],
  }
}

// ---------------------------------------------------------------------------
// SVG shape rendering
// ---------------------------------------------------------------------------

let rec renderShape = (comp: ciscoComponent, isSelected: bool, isDark: bool) => {
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
      })->React.array}
    </>
  }
}

// ---------------------------------------------------------------------------
// Connection rendering (existing connections + live drawing preview)
// ---------------------------------------------------------------------------

let renderConnection = (conn: connection, components: array<component>, isDark: bool) => {
  let fromComp = Array.getBy(components, c => c.id === conn.from)
  let toComp = Array.getBy(components, c => c.id === conn.to)

  switch (fromComp, toComp) {
  | (Some(from), Some(to)) => {
      let x1 = from.position.x +. 75.0
      let y1 = from.position.y +. 50.0
      let x2 = to.position.x +. 75.0
      let y2 = to.position.y +. 50.0

      let midX = (x1 +. x2) /. 2.0
      let path = `M ${Float.toString(x1)} ${Float.toString(y1)} Q ${Float.toString(
          midX,
        )} ${Float.toString(y1)} ${Float.toString(midX)} ${Float.toString(
          (y1 +. y2) /. 2.0,
        )} T ${Float.toString(x2)} ${Float.toString(y2)}`

      <path
        key={conn.id}
        d=path
        fill="none"
        stroke={isDark ? "#66B2FF" : "#0052CC"}
        strokeWidth="2"
        markerEnd="url(#arrowhead)"
        role="graphics-symbol"
        ariaLabel={`Connection from ${conn.from} to ${conn.to}`}
      />
    }
  | _ => React.null
  }
}

// Render the live connection-drawing preview line
let renderConnectionPreview = (
  fromId: string,
  mousePos: position,
  components: array<component>,
  isDark: bool,
) => {
  let fromComp = Array.getBy(components, c => c.id === fromId)

  switch fromComp {
  | Some(from) => {
      let x1 = from.position.x +. 75.0
      let y1 = from.position.y +. 50.0
      <line
        x1={Float.toString(x1)}
        y1={Float.toString(y1)}
        x2={Float.toString(mousePos.x)}
        y2={Float.toString(mousePos.y)}
        stroke={isDark ? "#66B2FF80" : "#0052CC80"}
        strokeWidth="2"
        strokeDasharray="6,4"
      />
    }
  | None => React.null
  }
}

// ---------------------------------------------------------------------------
// Connection port indicators (small circles on component edges)
// ---------------------------------------------------------------------------

let renderPortIndicator = (comp: ciscoComponent, isDark: bool) => {
  let (width, _height) = comp.size
  let cx = comp.position.x +. width
  let cy = comp.position.y +. 50.0

  <circle
    cx={Float.toString(cx)}
    cy={Float.toString(cy)}
    r="6"
    fill={isDark ? "#66B2FF" : "#0052CC"}
    stroke={isDark ? "#FFFFFF" : "#000000"}
    strokeWidth="1"
    style={Sx.make(~cursor="crosshair", ())}
    role="graphics-symbol"
    ariaLabel={comp.label ++ " connection port"}
  />
}

// ---------------------------------------------------------------------------
// Delete confirmation overlay
// ---------------------------------------------------------------------------

let renderDeleteConfirmation = (
  compId: string,
  components: array<component>,
  isDark: bool,
  onConfirm: unit => unit,
  onCancel: unit => unit,
) => {
  let comp = Array.getBy(components, c => c.id === compId)

  switch comp {
  | Some(c) =>
    <div
      role="alertdialog"
      ariaLabel="Confirm component deletion"
      style={Sx.make(
        ~position="fixed",
        ~top="0",
        ~left="0",
        ~width="100%",
        ~height="100%",
        ~display="flex",
        ~justifyContent="center",
        ~alignItems="center",
        ~zIndex="1000",
        ~backgroundColor="rgba(0,0,0,0.5)",
        (),
      )}
    >
      <div
        style={Sx.make(
          ~backgroundColor=isDark ? "#1A1A1A" : "#FFFFFF",
          ~color=isDark ? "#FFFFFF" : "#000000",
          ~padding="2rem",
          ~borderRadius="8px",
          ~maxWidth="400px",
          ~border=isDark ? "2px solid #CCCCCC" : "2px solid #333333",
          (),
        )}
      >
        <h3 style={{marginBottom: "1rem"}}>
          {"Delete Component?"->React.string}
        </h3>
        <p style={{marginBottom: "1.5rem"}}>
          {("Remove \"" ++ componentTypeToString(c.componentType) ++ "\" and all its connections?")->React.string}
        </p>
        <div style={Sx.make(~display="flex", ~gap="1rem", ~justifyContent="flex-end", ())}>
          <button
            onClick={_ => onCancel()}
            ariaLabel="Cancel deletion"
            style={Sx.make(
              ~padding="0.5rem 1.5rem",
              ~backgroundColor=isDark ? "#333333" : "#E0E0E0",
              ~color=isDark ? "#FFFFFF" : "#000000",
              ~border="none",
              ~borderRadius="4px",
              ~cursor="pointer",
              (),
            )}
          >
            {"Cancel"->React.string}
          </button>
          <button
            onClick={_ => onConfirm()}
            ariaLabel="Confirm deletion"
            style={Sx.make(
              ~padding="0.5rem 1.5rem",
              ~backgroundColor="#CC3333",
              ~color="white",
              ~border="none",
              ~borderRadius="4px",
              ~cursor="pointer",
              ~fontWeight="600",
              (),
            )}
          >
            {"Delete"->React.string}
          </button>
        </div>
      </div>
    </div>
  | None => React.null
  }
}

// ---------------------------------------------------------------------------
// Configuration panel (right sidebar)
// ---------------------------------------------------------------------------

let renderConfigPanel = (component: option<component>, isDark: bool, dispatch) => {
  switch component {
  | None =>
    <aside
      role="complementary"
      ariaLabel="Configuration panel"
      style={{
        width: "300px",
        padding: "1.5rem",
        backgroundColor: isDark ? "#000000" : "#FFFFFF",
        borderLeft: isDark ? "2px solid #CCCCCC" : "2px solid #333333",
        color: isDark ? "#FFFFFF" : "#000000",
      }}
    >
      <h3 style={{fontSize: "1.2rem", marginBottom: "1rem"}}> {"Configuration"->React.string} </h3>
      <p style={{opacity: "0.7"}}> {"Click a component to configure it"->React.string} </p>
      <div style={{marginTop: "2rem", opacity: "0.5", fontSize: "0.85rem"}}>
        <h4 style={{marginBottom: "0.5rem"}}> {"Keyboard Shortcuts"->React.string} </h4>
        <p> {"Ctrl+Z: Undo last action"->React.string} </p>
        <p> {"Delete: Remove selected component"->React.string} </p>
        <p> {"Shift+Click port: Draw connection"->React.string} </p>
      </div>
    </aside>

  | Some(comp) =>
    <aside
      role="complementary"
      ariaLabel={"Configuration for " ++ componentTypeToString(comp.componentType)}
      style={Sx.make(
        ~width="300px",
        ~padding="1.5rem",
        ~backgroundColor=isDark ? "#000000" : "#FFFFFF",
        ~borderLeft=isDark ? "2px solid #CCCCCC" : "2px solid #333333",
        ~color=isDark ? "#FFFFFF" : "#000000",
        (),
      )}
    >
      <h3 style={{fontSize: "1.2rem", marginBottom: "1rem"}}>
        {componentTypeToString(comp.componentType)->React.string}
      </h3>

      <form onSubmit={e => e->ReactEvent.Form.preventDefault}>
        // Shape selector
        <fieldset style={{marginBottom: "1rem", border: "none", padding: "0"}}>
          <legend style={{fontWeight: "600", marginBottom: "0.5rem"}}>
            {"Shape"->React.string}
          </legend>
          <label style={{display: "block", marginBottom: "0.5rem"}}>
            <input type_="radio" name="shape" value="box" defaultChecked=true />
            {" Box"->React.string}
          </label>
          <label style={{display: "block", marginBottom: "0.5rem"}}>
            <input type_="radio" name="shape" value="oval" />
            {" Oval"->React.string}
          </label>
          <label style={{display: "block"}}>
            <input type_="radio" name="shape" value="gateway" />
            {" Gateway"->React.string}
          </label>
        </fieldset>

        // Ports
        <fieldset style={{marginBottom: "1rem", border: "none", padding: "0"}}>
          <legend style={{fontWeight: "600", marginBottom: "0.5rem"}}>
            {"Ports"->React.string}
          </legend>
          <input
            type_="text"
            placeholder="8080:80"
            ariaLabel="Port mapping"
            style={{
              width: "100%",
              padding: "0.5rem",
              backgroundColor: isDark ? "#1A1A1A" : "#F5F5F5",
              color: isDark ? "#FFFFFF" : "#000000",
              border: isDark ? "1px solid #CCCCCC" : "1px solid #333333",
              borderRadius: "4px",
            }}
          />
        </fieldset>

        // Resources
        <fieldset style={{marginBottom: "1rem", border: "none", padding: "0"}}>
          <legend style={{fontWeight: "600", marginBottom: "0.5rem"}}>
            {"Resources"->React.string}
          </legend>
          <label style={{display: "block", marginBottom: "0.5rem"}}>
            {"CPU (cores)"->React.string}
            <input
              type_="number"
              defaultValue="1.0"
              step=0.1
              min="0.1"
              ariaLabel="CPU cores"
              style={Sx.make(
                ~width="100%",
                ~padding="0.5rem",
                ~marginTop="0.25rem",
                ~backgroundColor=isDark ? "#1A1A1A" : "#F5F5F5",
                ~color=isDark ? "#FFFFFF" : "#000000",
                ~border=isDark ? "1px solid #CCCCCC" : "1px solid #333333",
                ~borderRadius="4px",
                (),
              )}
            />
          </label>
          <label style={{display: "block"}}>
            {"Memory (MB)"->React.string}
            <input
              type_="number"
              defaultValue="512"
              step=128.0
              min="128"
              ariaLabel="Memory in megabytes"
              style={Sx.make(
                ~width="100%",
                ~padding="0.5rem",
                ~marginTop="0.25rem",
                ~backgroundColor=isDark ? "#1A1A1A" : "#F5F5F5",
                ~color=isDark ? "#FFFFFF" : "#000000",
                ~border=isDark ? "1px solid #CCCCCC" : "1px solid #333333",
                ~borderRadius="4px",
                (),
              )}
            />
          </label>
        </fieldset>

        // Actions
        <div style={Sx.make(~display="flex", ~gap="0.5rem", ~marginTop="1rem", ())}>
          <button
            type_="submit"
            ariaLabel="Apply configuration changes"
            style={Sx.make(
              ~flex="1",
              ~padding="0.75rem",
              ~backgroundColor=isDark ? "#66B2FF" : "#0052CC",
              ~color="white",
              ~border="none",
              ~borderRadius="6px",
              ~fontWeight="600",
              ~cursor="pointer",
              (),
            )}
          >
            {"Apply"->React.string}
          </button>
          <button
            type_="button"
            onClick={_ => dispatch(SelectComponent(None))}
            ariaLabel="Cancel configuration"
            style={Sx.make(
              ~flex="1",
              ~padding="0.75rem",
              ~backgroundColor=isDark ? "#1A1A1A" : "#F5F5F5",
              ~color=isDark ? "#FFFFFF" : "#000000",
              ~border=isDark ? "1px solid #CCCCCC" : "1px solid #333333",
              ~borderRadius="6px",
              ~fontWeight="600",
              ~cursor="pointer",
              (),
            )}
          >
            {"Cancel"->React.string}
          </button>
        </div>
      </form>
    </aside>
  }
}

// ---------------------------------------------------------------------------
// Toolbar with undo, zoom, and connection mode controls
// ---------------------------------------------------------------------------

let renderToolbar = (
  isDark: bool,
  canvasState: canvasState,
  dispatch,
  onUndo: unit => unit,
  onToggleConnMode: unit => unit,
) => {
  let undoDisabled = Array.length(canvasState.undoStack) === 0
  let isDrawingConn = canvasState.connectionDrawing !== None

  <div
    role="toolbar"
    ariaLabel="Canvas toolbar"
    style={Sx.make(
      ~display="flex",
      ~gap="0.5rem",
      ~padding="0.5rem",
      ~position="absolute",
      ~top="10px",
      ~left="10px",
      ~zIndex="100",
      ~backgroundColor=isDark ? "#1A1A1A" : "#F5F5F5",
      ~borderRadius="8px",
      ~border=isDark ? "1px solid #444" : "1px solid #CCC",
      (),
    )}
  >
    <button
      onClick={_ => dispatch(ZoomIn)}
      ariaLabel="Zoom in"
      title="Zoom in"
      style={Sx.make(~padding="0.4rem 0.8rem", ~cursor="pointer", ~border="none", ~borderRadius="4px", ~backgroundColor=isDark ? "#333" : "#DDD", ~color=isDark ? "#FFF" : "#000", ())}
    >
      {"+"->React.string}
    </button>
    <button
      onClick={_ => dispatch(ZoomOut)}
      ariaLabel="Zoom out"
      title="Zoom out"
      style={Sx.make(~padding="0.4rem 0.8rem", ~cursor="pointer", ~border="none", ~borderRadius="4px", ~backgroundColor=isDark ? "#333" : "#DDD", ~color=isDark ? "#FFF" : "#000", ())}
    >
      {"-"->React.string}
    </button>
    <button
      onClick={_ => dispatch(ResetZoom)}
      ariaLabel="Reset zoom"
      title="Reset zoom"
      style={Sx.make(~padding="0.4rem 0.8rem", ~cursor="pointer", ~border="none", ~borderRadius="4px", ~backgroundColor=isDark ? "#333" : "#DDD", ~color=isDark ? "#FFF" : "#000", ())}
    >
      {"1:1"->React.string}
    </button>
    <div style={{width: "1px", backgroundColor: isDark ? "#444" : "#CCC"}} />
    <button
      onClick={_ => onUndo()}
      disabled=undoDisabled
      ariaLabel="Undo last action"
      title="Undo (Ctrl+Z)"
      style={Sx.make(~padding="0.4rem 0.8rem", ~cursor=undoDisabled ? "default" : "pointer", ~border="none", ~borderRadius="4px", ~backgroundColor=isDark ? "#333" : "#DDD", ~color=isDark ? "#FFF" : "#000", ~opacity=undoDisabled ? "0.4" : "1", ())}
    >
      {"Undo"->React.string}
    </button>
    <div style={{width: "1px", backgroundColor: isDark ? "#444" : "#CCC"}} />
    <button
      onClick={_ => onToggleConnMode()}
      ariaLabel={isDrawingConn ? "Cancel connection drawing" : "Start connection drawing mode"}
      title="Draw connection"
      style={Sx.make(~padding="0.4rem 0.8rem", ~cursor="pointer", ~border="none", ~borderRadius="4px", ~backgroundColor=isDrawingConn ? (isDark ? "#66B2FF" : "#0052CC") : (isDark ? "#333" : "#DDD"), ~color=isDrawingConn ? "#FFF" : (isDark ? "#FFF" : "#000"), ())}
    >
      {"Connect"->React.string}
    </button>
  </div>
}

// ---------------------------------------------------------------------------
// Main topology view
// ---------------------------------------------------------------------------

let view = (model: model, isDark: bool, dispatch) => {
  let ciscoComponents = Array.map(model.components, toCiscoComponent)

  // Local canvas state management via React.useState
  let (canvasState, setCanvasState) = React.useState(() => initialCanvasState)

  // Undo handler
  let handleUndo = () => {
    let stack = canvasState.undoStack
    if Array.length(stack) > 0 {
      let lastEntry = stack[Array.length(stack) - 1]
      let newStack = Array.slice(stack, ~offset=0, ~len=Array.length(stack) - 1)

      switch lastEntry {
      | Some(MovedComponent(id, oldPos, _newPos)) =>
        dispatch(UpdateComponentPosition(id, snapToGrid(oldPos)))
      | Some(DeletedComponent(comp)) =>
        dispatch(AddComponent(comp.componentType, comp.position))
      | Some(DeletedConnection(conn)) =>
        dispatch(AddConnection(conn.from, conn.to))
      | Some(AddedConnection(connId)) =>
        dispatch(RemoveConnection(connId))
      | None => ()
      }

      setCanvasState(prev => {...prev, undoStack: newStack})
    }
  }

  // Toggle connection drawing mode
  let toggleConnMode = () => {
    setCanvasState(prev => {
      ...prev,
      connectionDrawing: switch prev.connectionDrawing {
      | Some(_) => None
      | None => None // Connection mode is initiated by clicking a port indicator
      },
    })
  }

  // Handle mouse move on SVG (for drag and connection drawing)
  let handleMouseMove = (e: ReactEvent.Mouse.t) => {
    let x = Float.fromInt(ReactEvent.Mouse.clientX(e))
    let y = Float.fromInt(ReactEvent.Mouse.clientY(e))

    // Update connection drawing preview
    switch canvasState.connectionDrawing {
    | Some((fromId, _)) =>
      setCanvasState(prev => {...prev, connectionDrawing: Some((fromId, {x, y}))})
    | None => ()
    }

    // Pass through to regular drag
    dispatch(DragMove(snapToGrid({x, y})))
  }

  // Handle component click for connection drawing
  let handleComponentClick = (compId: string) => {
    switch canvasState.connectionDrawing {
    | Some((fromId, _)) =>
      if fromId !== compId {
        // Complete the connection
        dispatch(AddConnection(fromId, compId))
        // Record for undo
        let connId = generateId()
        setCanvasState(prev => {
          ...prev,
          connectionDrawing: None,
          undoStack: Array.concat(prev.undoStack, [AddedConnection(connId)]),
        })
      }
    | None => dispatch(SelectComponent(Some(compId)))
    }
  }

  // Handle port indicator click to start connection drawing
  let handlePortClick = (compId: string, e: ReactEvent.Mouse.t) => {
    e->ReactEvent.Mouse.stopPropagation
    let x = Float.fromInt(ReactEvent.Mouse.clientX(e))
    let y = Float.fromInt(ReactEvent.Mouse.clientY(e))
    setCanvasState(prev => {...prev, connectionDrawing: Some((compId, {x, y}))})
  }

  // Handle delete with confirmation
  let handleDeleteRequest = (compId: string) => {
    setCanvasState(prev => {...prev, deleteConfirm: Some(compId)})
  }

  let handleDeleteConfirm = () => {
    switch canvasState.deleteConfirm {
    | Some(compId) =>
      let comp = Array.getBy(model.components, c => c.id === compId)
      switch comp {
      | Some(c) =>
        setCanvasState(prev => {
          ...prev,
          deleteConfirm: None,
          undoStack: Array.concat(prev.undoStack, [DeletedComponent(c)]),
        })
        dispatch(RemoveComponent(compId))
      | None => setCanvasState(prev => {...prev, deleteConfirm: None})
      }
    | None => ()
    }
  }

  let handleDeleteCancel = () => {
    setCanvasState(prev => {...prev, deleteConfirm: None})
  }

  // Handle drag end with snap-to-grid and undo recording
  let handleDragEnd = () => {
    switch model.dragState {
    | DraggingComponent(comp) =>
      let snappedPos = snapToGrid(comp.position)
      dispatch(UpdateComponentPosition(comp.id, snappedPos))
      // Record the move for undo (we use the component's current position as "new")
      setCanvasState(prev => {
        ...prev,
        undoStack: Array.concat(prev.undoStack, [
          MovedComponent(comp.id, comp.position, snappedPos),
        ]),
      })
    | _ => ()
    }
    dispatch(DragEnd)
  }

  <div
    role="main"
    ariaLabel="Cisco-style network topology designer"
    style={Sx.make(
      ~display="flex",
      ~height="100vh",
      ~backgroundColor=isDark ? "#000000" : "#FFFFFF",
      ~color=isDark ? "#FFFFFF" : "#000000",
      ~position="relative",
      (),
    )}
  >
    // Toolbar
    {renderToolbar(isDark, canvasState, dispatch, handleUndo, toggleConnMode)}

    // Main SVG canvas
    <svg
      width="100%"
      height="100%"
      role="graphics-document"
      ariaLabel="Container network topology"
      onMouseMove={handleMouseMove}
      onMouseUp={_ => handleDragEnd()}
    >
      // Define arrowhead marker for connections
      <defs>
        <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
          <polygon points="0 0, 10 3, 0 6" fill={isDark ? "#66B2FF" : "#0052CC"} />
        </marker>
      </defs>

      // Grid background (snap-to-grid visual guide)
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

      // Canvas transform group (zoom + pan)
      <g
        transform={`translate(${Float.toString(model.canvasOffset.x)}, ${Float.toString(
            model.canvasOffset.y,
          )}) scale(${Float.toString(model.zoomLevel)})`}
      >
        // Render connections first (below components)
        {Array.map(model.connections, conn =>
          renderConnection(conn, model.components, isDark)
        )->React.array}

        // Render connection drawing preview
        {switch canvasState.connectionDrawing {
        | Some((fromId, mousePos)) =>
          renderConnectionPreview(fromId, mousePos, model.components, isDark)
        | None => React.null
        }}

        // Render components
        {Array.map(ciscoComponents, comp => {
          let isSelected = switch model.selectedComponent {
          | Some(id) => id === comp.id
          | None => false
          }

          <g
            key=comp.id
            onClick={_ => handleComponentClick(comp.id)}
            onMouseDown={e => {
              let mousePos = {
                x: Float.fromInt(ReactEvent.Mouse.clientX(e)),
                y: Float.fromInt(ReactEvent.Mouse.clientY(e)),
              }
              switch Array.getBy(model.components, c => c.id === comp.id) {
              | Some(c) => dispatch(StartDragComponent(c, mousePos))
              | None => ()
              }
            }}
            onDoubleClick={_ => handleDeleteRequest(comp.id)}
            style={Sx.make(~cursor="pointer", ())}
          >
            {renderShape(comp, isSelected, isDark)}

            // Port indicator for connection drawing
            {renderPortIndicator(comp, isDark)}

            // Port indicator click target (invisible, larger hit area)
            <circle
              cx={Float.toString(comp.position.x +. Pair.first(comp.size))}
              cy={Float.toString(comp.position.y +. 50.0)}
              r="12"
              fill="transparent"
              onClick={e => handlePortClick(comp.id, e)}
              style={Sx.make(~cursor="crosshair", ())}
            />

            <text
              x={Float.toString(comp.position.x +. Pair.first(comp.size) /. 2.0)}
              y={Float.toString(comp.position.y +. Pair.second(comp.size) /. 2.0)}
              textAnchor="middle"
              dominantBaseline="middle"
              fill={isDark ? "#FFFFFF" : "#000000"}
              style={Sx.make(
                ~fontSize="14px",
                ~fontWeight="600",
                ~pointerEvents="none",
                (),
              )}
            >
              {comp.label->React.string}
            </text>

            // Snap position indicator when selected
            {isSelected
              ? <text
                  x={Float.toString(comp.position.x)}
                  y={Float.toString(comp.position.y -. 8.0)}
                  fill={isDark ? "#888" : "#999"}
                  style={Sx.make(~fontSize="10px", ~pointerEvents="none", ())}
                >
                  {(`${Float.toString(comp.position.x)}, ${Float.toString(comp.position.y)}`)->React.string}
                </text>
              : React.null}
          </g>
        })->React.array}
      </g>
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

    // Delete confirmation dialog
    {switch canvasState.deleteConfirm {
    | Some(compId) =>
      renderDeleteConfirmation(compId, model.components, isDark, handleDeleteConfirm, handleDeleteCancel)
    | None => React.null
    }}
  </div>
}
