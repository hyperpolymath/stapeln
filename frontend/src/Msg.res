// SPDX-License-Identifier: PMPL-1.0-or-later
// Msg.res - TEA Messages (events)

open Model

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
