// SPDX-License-Identifier: PMPL-1.0-or-later
// Update.res - TEA Update (state transitions)

open Model
open Msg

type effect<'msg> = Tea.Cmd.t<'msg>

let update = (model: model, msg: msg): (model, effect<msg>) => {
  switch msg {
  // Component management
  | AddComponent(componentType, position) => {
      let newComponent: component = {
        id: generateId(),
        componentType: componentType,
        position: position,
        config: Js.Dict.empty(),
      }
      let newModel = {
        ...model,
        components: Array.concat(model.components, [newComponent]),
      }
      (newModel, Tea.Cmd.none)
    }

  | RemoveComponent(id) => {
      let newComponents = Array.keep(model.components, c => c.id !== id)
      let newConnections = Array.keep(
        model.connections,
        conn => conn.from !== id && conn.to !== id,
      )
      let newModel = {
        ...model,
        components: newComponents,
        connections: newConnections,
        selectedComponent: model.selectedComponent === Some(id) ? None : model.selectedComponent,
      }
      (newModel, Tea.Cmd.none)
    }

  | UpdateComponentPosition(id, position) => {
      let newComponents = Array.map(model.components, comp =>
        comp.id === id ? {...comp, position: position} : comp
      )
      let newModel = {...model, components: newComponents}
      (newModel, Tea.Cmd.none)
    }

  | UpdateComponentConfig(id, config) => {
      let newComponents = Array.map(model.components, comp =>
        comp.id === id ? {...comp, config: config} : comp
      )
      let newModel = {...model, components: newComponents}
      (newModel, Tea.Cmd.none)
    }

  | SelectComponent(componentId) => {
      let newModel = {...model, selectedComponent: componentId}
      (newModel, Tea.Cmd.none)
    }

  // Connection management
  | AddConnection(fromId, toId) => {
      // Validate connection: check if components exist
      let fromExists = Array.some(model.components, c => c.id === fromId)
      let toExists = Array.some(model.components, c => c.id === toId)

      if fromExists && toExists {
        let newConnection: connection = {
          id: generateId(),
          from: fromId,
          to: toId,
        }
        let newModel = {
          ...model,
          connections: Array.concat(model.connections, [newConnection]),
        }
        (newModel, Tea.Cmd.none)
      } else {
        // Invalid connection, don't add
        (model, Tea.Cmd.none)
      }
    }

  | RemoveConnection(id) => {
      let newConnections = Array.keep(model.connections, conn => conn.id !== id)
      let newModel = {...model, connections: newConnections}
      (newModel, Tea.Cmd.none)
    }

  // Drag and drop
  | StartDragComponent(component, mousePos) => {
      let newModel = {...model, dragState: DraggingComponent(component)}
      (newModel, Tea.Cmd.none)
    }

  | StartDragCanvas(mousePos) => {
      let newModel = {...model, dragState: DraggingCanvas(mousePos)}
      (newModel, Tea.Cmd.none)
    }

  | DragMove(mousePos) => {
      switch model.dragState {
      | DraggingComponent(component) => {
          // Update component position during drag
          let newComponents = Array.map(model.components, comp =>
            comp.id === component.id ? {...comp, position: mousePos} : comp
          )
          let newModel = {
            ...model,
            components: newComponents,
            dragState: DraggingComponent({...component, position: mousePos}),
          }
          (newModel, Tea.Cmd.none)
        }

      | DraggingCanvas(startPos) => {
          // Pan the canvas
          let deltaX = mousePos.x -. startPos.x
          let deltaY = mousePos.y -. startPos.y
          let newOffset = {
            x: model.canvasOffset.x +. deltaX,
            y: model.canvasOffset.y +. deltaY,
          }
          let newModel = {
            ...model,
            canvasOffset: newOffset,
            dragState: DraggingCanvas(mousePos),
          }
          (newModel, Tea.Cmd.none)
        }

      | NotDragging => (model, Tea.Cmd.none)
      }
    }

  | DragEnd => {
      let newModel = {...model, dragState: NotDragging}
      (newModel, Tea.Cmd.none)
    }

  // Canvas operations
  | ZoomIn => {
      let newZoom = Float.min(model.zoomLevel *. 1.2, 3.0) // Max 3x zoom
      let newModel = {...model, zoomLevel: newZoom}
      (newModel, Tea.Cmd.none)
    }

  | ZoomOut => {
      let newZoom = Float.max(model.zoomLevel /. 1.2, 0.5) // Min 0.5x zoom
      let newModel = {...model, zoomLevel: newZoom}
      (newModel, Tea.Cmd.none)
    }

  | ResetZoom => {
      let newModel = {...model, zoomLevel: 1.0, canvasOffset: {x: 0.0, y: 0.0}}
      (newModel, Tea.Cmd.none)
    }

  | PanCanvas(offset) => {
      let newModel = {...model, canvasOffset: offset}
      (newModel, Tea.Cmd.none)
    }

  // Validation
  | ValidateStack => {
      // TODO: Send validation request to backend
      // For now, return a mock validation
      let cmd = Tea.Cmd.call(() => {
        let result: validationResult = {
          valid: Array.length(model.components) > 0,
          errors: [],
          warnings: Array.length(model.components) === 0
            ? ["Stack is empty"]
            : [],
        }
        ValidationResult(result)
      })
      (model, cmd)
    }

  | ValidationResult(result) => {
      let newModel = {...model, validationResult: Some(result)}
      (newModel, Tea.Cmd.none)
    }

  // Export
  | ExportToSelurCompose => {
      // TODO: Generate compose.toml and download
      Js.Console.log("Exporting to selur-compose format...")
      (model, Tea.Cmd.none)
    }

  | ExportToDockerCompose => {
      // TODO: Generate docker-compose.yml and download
      Js.Console.log("Exporting to docker-compose format...")
      (model, Tea.Cmd.none)
    }

  | ExportToPodmanCompose => {
      // TODO: Generate podman-compose.yml and download
      Js.Console.log("Exporting to podman-compose format...")
      (model, Tea.Cmd.none)
    }

  // API communication
  | SaveStack => {
      // TODO: Send stack to backend API
      Js.Console.log("Saving stack...")
      (model, Tea.Cmd.none)
    }

  | LoadStack(stackId) => {
      // TODO: Load stack from backend API
      Js.Console.log2("Loading stack:", stackId)
      (model, Tea.Cmd.none)
    }

  | StackSaved(result) => {
      switch result {
      | Ok(stackId) => {
          Js.Console.log2("Stack saved with ID:", stackId)
          (model, Tea.Cmd.none)
        }
      | Error(err) => {
          Js.Console.error2("Failed to save stack:", err)
          (model, Tea.Cmd.none)
        }
      }
    }

  | StackLoaded(result) => {
      switch result {
      | Ok(loadedModel) => {
          Js.Console.log("Stack loaded successfully")
          (loadedModel, Tea.Cmd.none)
        }
      | Error(err) => {
          Js.Console.error2("Failed to load stack:", err)
          (model, Tea.Cmd.none)
        }
      }
    }
  }
}

// Subscriptions (for animations, WebSocket, etc.)
let subscriptions = (_model: model): Tea.Sub.t<msg> => {
  Tea.Sub.none
}
