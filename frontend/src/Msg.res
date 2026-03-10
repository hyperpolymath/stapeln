// SPDX-License-Identifier: PMPL-1.0-or-later
// Msg.res - TEA Messages (events)

open Model

// Messages for the visual pipeline designer (node-graph editor)
type pipelineMsg =
  // Node management
  | AddNode(PipelineModel.nodeKind, float, float) // kind, x, y
  | RemoveNode(string) // nodeId
  | MoveNode(string, float, float) // nodeId, x, y
  | SelectNode(string) // nodeId
  | DeselectNode
  | UpdateNodeConfig(string, PipelineModel.nodeKind) // nodeId, updated kind
  // Connection management
  | StartConnection(string, string) // fromNode, fromPort
  | UpdateConnection(float, float) // mouseX, mouseY (while drawing)
  | CompleteConnection(string, string) // toNode, toPort
  | CancelConnection
  | RemoveConnection(string) // connectionId
  // Drag and drop (node movement)
  | StartDrag(string, float, float) // nodeId, offsetX, offsetY
  | Drag(float, float) // mouseX, mouseY
  | EndDrag
  // Canvas navigation
  | SetZoom(float)
  | SetPan(float, float) // panX, panY
  // Validation
  | ValidatePipeline
  | ValidationResult(PipelineModel.pipelineValidation)
  // Code generation
  | GenerateOutput(PipelineModel.outputFormat)
  | OutputGenerated(string) // generated code
  // Persistence
  | LoadTemplate(string) // templateId
  | SavePipeline
  | LoadPipeline(string) // pipelineId
  | PipelineSaved(Result.t<string, string>)
  | PipelineLoaded(Result.t<PipelineModel.pipeline, string>)
  // Panel controls
  | SetLeftTab(PipelineModel.leftTab)
  | SetRightTab(PipelineModel.rightTab)
  | ToggleLeftPanel
  | ToggleRightPanel
  // Pipeline execution
  | RunPipeline
  | StopPipeline
  | NodeStatusUpdate(string, PipelineModel.nodeStatus) // nodeId, new status

type msg =
  // Component management
  | AddComponent(componentType, position)
  | RemoveComponent(string)
  | UpdateComponentPosition(string, position)
  | UpdateComponentConfig(string, dict<string>)
  | SelectComponent(option<string>)
  // Connection management
  | AddConnection(string, string) // from, to
  | RemoveConnection(string)
  // Drag and drop
  | StartDragComponent(component, position)
  | StartDragCanvas(position)
  | DragMove(position)
  | DragEnd
  // Canvas operations
  | ZoomIn
  | ZoomOut
  | ResetZoom
  | PanCanvas(position)
  // Validation
  | ValidateStack
  | ValidationResult(validationResult)
  // Export
  | ExportDesignToJson(string) // description
  | ExportToSelurCompose
  | ExportToDockerCompose
  | ExportToPodmanCompose
  | ExportToKubernetesYaml
  | ExportToHelmChart
  // Import
  | TriggerImportDesign
  | ImportDesignSuccess(model)
  | ImportDesignError(string)
  // API communication
  | SaveStack
  | LoadStack(string)
  | StackSaved(Result.t<string, string>)
  | StackLoaded(Result.t<model, string>)
  // Security
  | RunSecurityScan
  | SecurityScanLoading
  | SecurityScanResult(Result.t<JSON.t, string>)
  | RunGapAnalysis
  | GapAnalysisLoading
  | GapAnalysisResult(Result.t<JSON.t, string>)
  // Settings
  | SaveSettings
  | LoadSettings
  | SettingsSaved(Result.t<unit, string>)
  | SettingsLoaded(Result.t<JSON.t, string>)
  // WebSocket real-time (optional — app works with REST only)
  | WsConnect
  | WsDisconnect
  | WsConnectionStateChanged(Socket.connectionState)
  | WsValidate
  | WsValidationResult(JSON.t)
  | WsSecurityScan
  | WsSecurityResult(JSON.t)
  | WsGapAnalysis
  | WsGapResult(JSON.t)
  // Pipeline designer
  | Pipeline(pipelineMsg)
