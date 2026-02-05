// SPDX-License-Identifier: PMPL-1.0-or-later
// Update.res - State transitions (pure functions)

open Model
open Msg

// No effects - pure state updates only
let update = (model: model, msg: msg): model => {
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
      newModel
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
      newModel
    }

  | UpdateComponentPosition(id, position) => {
      let newComponents = Array.map(model.components, comp =>
        comp.id === id ? {...comp, position: position} : comp
      )
      let newModel = {...model, components: newComponents}
      newModel
    }

  | UpdateComponentConfig(id, config) => {
      let newComponents = Array.map(model.components, comp =>
        comp.id === id ? {...comp, config: config} : comp
      )
      let newModel = {...model, components: newComponents}
      newModel
    }

  | SelectComponent(componentId) => {
      let newModel = {...model, selectedComponent: componentId}
      newModel
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
        newModel
      } else {
        // Invalid connection, don't add
        model
      }
    }

  | RemoveConnection(id) => {
      let newConnections = Array.keep(model.connections, conn => conn.id !== id)
      let newModel = {...model, connections: newConnections}
      newModel
    }

  // Drag and drop
  | StartDragComponent(component, mousePos) => {
      let newModel = {...model, dragState: DraggingComponent(component)}
      newModel
    }

  | StartDragCanvas(mousePos) => {
      let newModel = {...model, dragState: DraggingCanvas(mousePos)}
      newModel
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
          newModel
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
          newModel
        }

      | NotDragging => model
      }
    }

  | DragEnd => {
      let newModel = {...model, dragState: NotDragging}
      newModel
    }

  // Canvas operations
  | ZoomIn => {
      let newZoom = Float.min(model.zoomLevel *. 1.2, 3.0) // Max 3x zoom
      let newModel = {...model, zoomLevel: newZoom}
      newModel
    }

  | ZoomOut => {
      let newZoom = Float.max(model.zoomLevel /. 1.2, 0.5) // Min 0.5x zoom
      let newModel = {...model, zoomLevel: newZoom}
      newModel
    }

  | ResetZoom => {
      let newModel = {...model, zoomLevel: 1.0, canvasOffset: {x: 0.0, y: 0.0}}
      newModel
    }

  | PanCanvas(offset) => {
      let newModel = {...model, canvasOffset: offset}
      newModel
    }

  // Validation
  | ValidateStack => {
      // Run validation synchronously
      let result: validationResult = {
        valid: Array.length(model.components) > 0,
        errors: [],
        warnings: Array.length(model.components) === 0
          ? ["Stack is empty"]
          : [],
      }
      let newModel = {...model, validationResult: Some(result)}
      newModel
    }

  | ValidationResult(result) => {
      let newModel = {...model, validationResult: Some(result)}
      newModel
    }

  // Export
  | ExportDesignToJson(description) => {
      Export.exportDesignToJson(model, description)
      model
    }

  | ExportToSelurCompose => {
      Export.exportToSelurCompose(model)
      model
    }

  | ExportToDockerCompose => {
      Export.exportToDockerCompose(model)
      model
    }

  | ExportToPodmanCompose => {
      Export.exportToPodmanCompose(model)
      model
    }

  // Import
  | TriggerImportDesign => {
      // Trigger file picker (side effect)
      Import.triggerImport(
        importedModel => ImportDesignSuccess(importedModel),
        error => ImportDesignError(error),
      )
      model
    }

  | ImportDesignSuccess(importedModel) => {
      Js.Console.log("Design imported successfully")
      importedModel
    }

  | ImportDesignError(error) => {
      Js.Console.error2("Import failed:", error)
      // TODO: Show error message to user
      model
    }

  // API communication
  | SaveStack => {
      // TODO: Send stack to backend API
      Js.Console.log("Saving stack...")
      model
    }

  | LoadStack(stackId) => {
      // TODO: Load stack from backend API
      Js.Console.log2("Loading stack:", stackId)
      model
    }

  | StackSaved(result) => {
      switch result {
      | Ok(stackId) => {
          Js.Console.log2("Stack saved with ID:", stackId)
          model
        }
      | Error(err) => {
          Js.Console.error2("Failed to save stack:", err)
          model
        }
      }
    }

  | StackLoaded(result) => {
      switch result {
      | Ok(loadedModel) => {
          Js.Console.log("Stack loaded successfully")
          loadedModel
        }
      | Error(err) => {
          Js.Console.error2("Failed to load stack:", err)
          model
        }
      }
    }
  }
}
