// SPDX-License-Identifier: PMPL-1.0-or-later
// Model.res - TEA Model (application state)

type componentType =
  | CerroTorre // Container builder (.ctp bundles)
  | LagoGrey // Base image designer (Alpine/Chainguard alternative)
  | Svalinn // Edge gateway
  | Selur // IPC bridge
  | Vordr // Runtime/orchestrator
  | Podman // Container runtime
  | Docker // Container runtime
  | Nerdctl // Container runtime
  | Volume // Persistent storage
  | Network // Networking

type position = {
  x: float,
  y: float,
}

type component = {
  id: string,
  componentType: componentType,
  position: position,
  config: dict<string>, // Component-specific configuration
}

type connection = {
  id: string,
  from: string, // Component ID
  to: string, // Component ID
}

type dragState =
  | NotDragging
  | DraggingComponent(component)
  | DraggingCanvas(position)

type rec model = {
  components: array<component>,
  connections: array<connection>,
  selectedComponent: option<string>,
  dragState: dragState,
  canvasOffset: position,
  zoomLevel: float,
  validationResult: option<validationResult>,
}

and validationResult = {
  valid: bool,
  errors: array<string>,
  warnings: array<string>,
}

let initialModel = {
  components: [],
  connections: [],
  selectedComponent: None,
  dragState: NotDragging,
  canvasOffset: {x: 0.0, y: 0.0},
  zoomLevel: 1.0,
  validationResult: None,
}

// Helper functions

let generateId = () => {
  // Simple UUID v4 generation
  let chars = "0123456789abcdef"
  let uuid = ref("")
  for i in 0 to 35 {
    let idx = Js.Math.random_int(0, 16)
    let char = String.charAt(chars, idx)
    uuid := uuid.contents ++ char
    if i == 7 || i == 12 || i == 17 || i == 22 {
      uuid := uuid.contents ++ "-"
    }
  }
  uuid.contents
}

let findComponent = (model: model, id: string): option<component> => {
  Array.getBy(model.components, c => c.id == id)
}

let componentTypeToString = (ct: componentType): string => {
  switch ct {
  | CerroTorre => "Cerro Torre"
  | LagoGrey => "Lago Grey"
  | Svalinn => "Svalinn"
  | Selur => "selur"
  | Vordr => "Vörðr"
  | Podman => "Podman"
  | Docker => "Docker"
  | Nerdctl => "nerdctl"
  | Volume => "Volume"
  | Network => "Network"
  }
}
