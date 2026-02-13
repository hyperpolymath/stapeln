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
        componentType,
        position,
        config: Dict.make(),
      }
      let newModel = {
        ...model,
        components: Array.concat(model.components, [newComponent]),
      }
      newModel
    }

  | RemoveComponent(id) => {
      let newComponents = Array.keep(model.components, c => c.id !== id)
      let newConnections = Array.keep(model.connections, conn => conn.from !== id && conn.to !== id)
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
        comp.id === id ? {...comp, position} : comp
      )
      let newModel = {...model, components: newComponents}
      newModel
    }

  | UpdateComponentConfig(id, config) => {
      let newComponents = Array.map(model.components, comp =>
        comp.id === id ? {...comp, config} : comp
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

  | DragMove(mousePos) => switch model.dragState {
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

  | DragEnd => {
      let newModel = {...model, dragState: NotDragging}
      newModel
    }

  // Canvas operations
  | ZoomIn => {
      let newZoom = Math.min(model.zoomLevel *. 1.2, 3.0) // Max 3x zoom
      let newModel = {...model, zoomLevel: newZoom}
      newModel
    }

  | ZoomOut => {
      let newZoom = Math.max(model.zoomLevel /. 1.2, 0.5) // Min 0.5x zoom
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
        warnings: Array.length(model.components) === 0 ? ["Stack is empty"] : [],
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
        importedModel => {
          // Log success - actual model update would need to be handled via Tea.Cmd
          Console.log2("Design imported successfully:", importedModel)
          ()
        },
        error => {
          // Log error - actual error handling would need to be handled via Tea.Cmd
          Console.error2("Import failed:", error)
          ()
        },
      )
      model
    }

  | ImportDesignSuccess(importedModel) => {
      Console.log("Design imported successfully")
      importedModel
    }

  | ImportDesignError(error) => {
      Console.error2("Import failed:", error)

      // TODO: Show error message to user
      model
    }

  // API communication
  | SaveStack => {
      Console.log("Saving stack to backend...")
      model
    }

  | LoadStack(stackId) => {
      Console.log2("Loading stack:", stackId)
      model
    }

  | StackSaved(result) => switch result {
    | Ok(stackId) => {
        Console.log2("Stack saved with ID:", stackId)
        model
      }
    | Error(err) => {
        Console.error2("Failed to save stack:", err)
        model
      }
    }

  | StackLoaded(result) => switch result {
    | Ok(loadedModel) => {
        Console.log("Stack loaded successfully")
        loadedModel
      }
    | Error(err) => {
        Console.error2("Failed to load stack:", err)
        model
      }
    }

  // Security
  | RunSecurityScan => {
      Console.log("Running security scan...")
      model
    }

  | SecurityScanResult(result) => {
      Console.log("Security scan complete")
      {...model, validationResult: Some(result)}
    }

  | RunGapAnalysis => {
      Console.log("Running gap analysis...")
      model
    }

  | GapAnalysisResult(result) => {
      Console.log("Gap analysis complete")
      {...model, validationResult: Some(result)}
    }
  }
}
