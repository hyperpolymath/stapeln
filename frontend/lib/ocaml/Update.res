// SPDX-License-Identifier: PMPL-1.0-or-later
// Update.res - State transitions (pure functions)

open Model
open Msg

// ---------------------------------------------------------------------------
// JSON parsing helpers
// ---------------------------------------------------------------------------

// Extract a string from a JSON value, with a default fallback
let jsonString = (json: JSON.t, fallback: string): string =>
  switch json {
  | String(s) => s
  | _ => fallback
  }

// Extract an int from a JSON value (JSON numbers are floats), with a default
let jsonInt = (json: JSON.t, fallback: int): int =>
  switch json {
  | Number(n) => Float.toInt(n)
  | _ => fallback
  }

// Extract a bool from a JSON value, with a default
let jsonBool = (json: JSON.t, fallback: bool): bool =>
  switch json {
  | Boolean(b) => b
  | _ => fallback
  }

// Extract a dict from a JSON value
let jsonDict = (json: JSON.t): option<Dict.t<JSON.t>> =>
  switch json {
  | Object(d) => Some(d)
  | _ => None
  }

// Extract a JSON array, returning empty array on failure
let jsonArray = (json: JSON.t): array<JSON.t> =>
  switch json {
  | Array(arr) => arr
  | _ => []
  }

// Extract an optional string (returns None for JSON null or missing)
let jsonOptString = (json: JSON.t): option<string> =>
  switch json {
  | String(s) => Some(s)
  | Null => None
  | _ => None
  }

// Extract an optional string array
let jsonOptStringArray = (json: JSON.t): option<array<string>> =>
  switch json {
  | Array(arr) => Some(Array.map(arr, item => jsonString(item, "")))
  | Null => None
  | _ => None
  }

// Extract a string array, returning empty array on failure
let jsonStringArray = (json: JSON.t): array<string> =>
  switch json {
  | Array(arr) => Array.map(arr, item => jsonString(item, ""))
  | _ => []
  }

// Safe dict field accessor
let field = (d: Dict.t<JSON.t>, key: string): option<JSON.t> => Dict.get(d, key)

// ---------------------------------------------------------------------------
// SecurityInspector JSON parsing
// ---------------------------------------------------------------------------

let parseSeverity = (s: string): SecurityInspector.severity =>
  switch String.toLowerCase(s) {
  | "critical" => Critical
  | "high" => High
  | "medium" => Medium
  | "low" => Low
  | _ => Info
  }

let parseCheckResult = (s: string): SecurityInspector.checkResult =>
  switch String.toLowerCase(s) {
  | "pass" => Pass
  | "fail" => Fail
  | "warning" => Warning
  | _ => Unknown
  }

let parseGrade = (s: string): SecurityInspector.grade =>
  switch s {
  | "A+" => APlus
  | "A" => A
  | "A-" => AMinus
  | "B+" => BPlus
  | "B" => B
  | "B-" => BMinus
  | "C+" => CPlus
  | "C" => C
  | "C-" => CMinus
  | "D" => D
  | _ => F
  }

let parseVulnerability = (json: JSON.t): option<SecurityInspector.vulnerability> =>
  switch jsonDict(json) {
  | None => None
  | Some(d) => {
      let id = switch field(d, "id") {
      | Some(v) => jsonString(v, "")
      | None => ""
      }
      if id === "" {
        None
      } else {
        let vuln: SecurityInspector.vulnerability = {
          id,
          title: switch field(d, "title") {
          | Some(v) => jsonString(v, "Unknown")
          | None => "Unknown"
          },
          severity: switch field(d, "severity") {
          | Some(v) => parseSeverity(jsonString(v, "info"))
          | None => Info
          },
          description: switch field(d, "description") {
          | Some(v) => jsonString(v, "")
          | None => ""
          },
          affectedComponent: switch field(d, "affectedComponent") {
          | Some(v) => jsonString(v, "")
          | None => ""
          },
          cveId: switch field(d, "cveId") {
          | Some(v) => jsonOptString(v)
          | None => None
          },
          fixAvailable: switch field(d, "fixAvailable") {
          | Some(v) => jsonBool(v, false)
          | None => false
          },
          fixDescription: switch field(d, "fixDescription") {
          | Some(v) => jsonOptString(v)
          | None => None
          },
        }
        Some(vuln)
      }
    }
  }

let parseSecurityCheck = (json: JSON.t): option<SecurityInspector.securityCheck> =>
  switch jsonDict(json) {
  | None => None
  | Some(d) => {
      let check: SecurityInspector.securityCheck = {
        name: switch field(d, "name") {
        | Some(v) => jsonString(v, "Unknown")
        | None => "Unknown"
        },
        description: switch field(d, "description") {
        | Some(v) => jsonString(v, "")
        | None => ""
        },
        result: switch field(d, "result") {
        | Some(v) => parseCheckResult(jsonString(v, "unknown"))
        | None => Unknown
        },
        details: switch field(d, "details") {
        | Some(v) => jsonString(v, "")
        | None => ""
        },
      }
      Some(check)
    }
  }

let parseExposedPort = (json: JSON.t): option<SecurityInspector.exposedPort> =>
  switch jsonDict(json) {
  | None => None
  | Some(d) => {
      let port: SecurityInspector.exposedPort = {
        port: switch field(d, "port") {
        | Some(v) => jsonInt(v, 0)
        | None => 0
        },
        protocol: switch field(d, "protocol") {
        | Some(v) => jsonString(v, "TCP")
        | None => "TCP"
        },
        service: switch field(d, "service") {
        | Some(v) => jsonString(v, "Unknown")
        | None => "Unknown"
        },
        risk: switch field(d, "risk") {
        | Some(v) => jsonString(v, "Low")
        | None => "Low"
        },
        publiclyAccessible: switch field(d, "publiclyAccessible") {
        | Some(v) => jsonBool(v, false)
        | None => false
        },
      }
      Some(port)
    }
  }

let parseMetrics = (json: JSON.t): SecurityInspector.securityMetrics =>
  switch jsonDict(json) {
  | None => {security: 0, performance: 0, reliability: 0, compliance: 0}
  | Some(d) => {
      security: switch field(d, "security") {
      | Some(v) => jsonInt(v, 0)
      | None => 0
      },
      performance: switch field(d, "performance") {
      | Some(v) => jsonInt(v, 0)
      | None => 0
      },
      reliability: switch field(d, "reliability") {
      | Some(v) => jsonInt(v, 0)
      | None => 0
      },
      compliance: switch field(d, "compliance") {
      | Some(v) => jsonInt(v, 0)
      | None => 0
      },
    }
  }

// Filter-map helper: map then keep only Some values
let filterMap = (arr: array<'a>, f: 'a => option<'b>): array<'b> =>
  Array.reduce(arr, [], (acc, item) =>
    switch f(item) {
    | Some(v) => Array.concat(acc, [v])
    | None => acc
    }
  )

let parseSecurityScanJson = (json: JSON.t): SecurityInspector.state =>
  switch jsonDict(json) {
  | None => SecurityInspector.init
  | Some(d) => {
      let metrics = switch field(d, "metrics") {
      | Some(v) => parseMetrics(v)
      | None => {security: 0, performance: 0, reliability: 0, compliance: 0}
      }
      let grade = switch field(d, "grade") {
      | Some(v) => parseGrade(jsonString(v, "F"))
      | None => F
      }
      let vulnerabilities = switch field(d, "vulnerabilities") {
      | Some(v) => filterMap(jsonArray(v), parseVulnerability)
      | None => []
      }
      let checks = switch field(d, "checks") {
      | Some(v) => filterMap(jsonArray(v), parseSecurityCheck)
      | None => []
      }
      let exposedPorts = switch field(d, "exposedPorts") {
      | Some(v) => filterMap(jsonArray(v), parseExposedPort)
      | None => []
      }
      let state: SecurityInspector.state = {
        metrics,
        grade,
        vulnerabilities,
        checks,
        exposedPorts,
        selectedVulnerability: None,
        showDetails: true,
        filterSeverity: None,
      }
      state
    }
  }

// ---------------------------------------------------------------------------
// GapAnalysis JSON parsing
// ---------------------------------------------------------------------------

let parseGapCategory = (s: string): GapAnalysis.gapCategory =>
  switch String.toLowerCase(s) {
  | "security" => Security
  | "compliance" => Compliance
  | "performance" => Performance
  | "reliability" => Reliability
  | "best_practice" | "bestpractice" => BestPractice
  | _ => BestPractice
  }

let parseGapSeverity = (s: string): GapAnalysis.gapSeverity =>
  switch String.toLowerCase(s) {
  | "critical" => Critical
  | "high" => High
  | "medium" => Medium
  | _ => Low
  }

let parseFixConfidence = (s: string): GapAnalysis.fixConfidence =>
  switch String.toLowerCase(s) {
  | "verified" => Verified
  | "high" => High
  | "medium" => Medium
  | "low" => Low
  | _ => Manual
  }

let parseIssueSource = (s: string): GapAnalysis.issueSource =>
  switch String.toLowerCase(s) {
  | "manual_review" | "manualreview" => ManualReview
  | "automated_scan" | "automatedscan" => AutomatedScan
  | "ci_workflow" | "ciworkflow" => CIWorkflow
  | "minikanren_reasoning" | "minikanrenreasoning" => MiniKanrenReasoning
  | "verisimdb_query" | "verisimdbquery" => VeriSimDBQuery
  | "hypatia_agent" | "hypatiaagent" => HypatiaAgent
  | other => ThirdPartyTool(other)
  }

let parseImpactScope = (
  s: string,
  components: array<string>,
): GapAnalysis.impactScope =>
  switch String.toLowerCase(s) {
  | "entire_stack" | "entirestack" => EntireStack
  | "external_dependencies" | "externaldependencies" => ExternalDependencies
  | "multiple_components" | "multiplecomponents" => MultipleComponents(components)
  | "single_component" | "singlecomponent" =>
    switch components[0] {
    | Some(c) => SingleComponent(c)
    | None => SingleComponent("unknown")
    }
  | _ => EntireStack
  }

let parseGap = (json: JSON.t): option<GapAnalysis.gap> =>
  switch jsonDict(json) {
  | None => None
  | Some(d) => {
      let id = switch field(d, "id") {
      | Some(v) => jsonString(v, "")
      | None => ""
      }
      if id === "" {
        None
      } else {
        let affectedComponents = switch field(d, "affectedComponents") {
        | Some(v) => jsonStringArray(v)
        | None => []
        }
        let gap: GapAnalysis.gap = {
          id,
          title: switch field(d, "title") {
          | Some(v) => jsonString(v, "Unknown")
          | None => "Unknown"
          },
          category: switch field(d, "category") {
          | Some(v) => parseGapCategory(jsonString(v, "best_practice"))
          | None => BestPractice
          },
          severity: switch field(d, "severity") {
          | Some(v) => parseGapSeverity(jsonString(v, "low"))
          | None => Low
          },
          description: switch field(d, "description") {
          | Some(v) => jsonString(v, "")
          | None => ""
          },
          impact: switch field(d, "impact") {
          | Some(v) => jsonString(v, "")
          | None => ""
          },
          source: switch field(d, "source") {
          | Some(v) => parseIssueSource(jsonString(v, "automated_scan"))
          | None => AutomatedScan
          },
          scope: switch field(d, "scope") {
          | Some(v) => parseImpactScope(jsonString(v, "entire_stack"), affectedComponents)
          | None => EntireStack
          },
          affectedComponents,
          fixAvailable: switch field(d, "fixAvailable") {
          | Some(v) => jsonBool(v, false)
          | None => false
          },
          fixConfidence: switch field(d, "fixConfidence") {
          | Some(v) => parseFixConfidence(jsonString(v, "manual"))
          | None => Manual
          },
          fixDescription: switch field(d, "fixDescription") {
          | Some(v) => jsonOptString(v)
          | None => None
          },
          fixCommands: switch field(d, "fixCommands") {
          | Some(v) => jsonOptStringArray(v)
          | None => None
          },
          estimatedEffort: switch field(d, "estimatedEffort") {
          | Some(v) => jsonString(v, "unknown")
          | None => "unknown"
          },
          tags: switch field(d, "tags") {
          | Some(v) => jsonStringArray(v)
          | None => []
          },
        }
        Some(gap)
      }
    }
  }

let parseGapAnalysisJson = (json: JSON.t): GapAnalysis.state =>
  switch jsonDict(json) {
  | None => GapAnalysis.init
  | Some(d) => {
      let gaps = switch field(d, "gaps") {
      | Some(v) => filterMap(jsonArray(v), parseGap)
      | None => []
      }
      let state: GapAnalysis.state = {
        gaps,
        appliedFixes: Belt.Map.String.empty,
        selectedGap: None,
        filterCategory: None,
        filterSeverity: None,
        showOnlyFixable: false,
        sortBy: BySeverity,
      }
      state
    }
  }

// ---------------------------------------------------------------------------
// Main update function
// ---------------------------------------------------------------------------

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
  | StartDragComponent(component, _mousePos) => {
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

  | ExportToKubernetesYaml => {
      Export.exportToKubernetesYaml(model)
      model
    }

  | ExportToHelmChart => {
      Export.exportToHelmChart(model)
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
      {...model, securityLoading: true}
    }

  | SecurityScanLoading => {
      {...model, securityLoading: true}
    }

  | SecurityScanResult(result) => switch result {
    | Ok(json) => {
        Console.log("Security scan complete")
        let parsed = parseSecurityScanJson(json)
        {...model, securityState: Some(parsed), securityLoading: false}
      }
    | Error(err) => {
        Console.error2("Security scan failed:", err)
        {...model, securityLoading: false}
      }
    }

  | RunGapAnalysis => {
      Console.log("Running gap analysis...")
      {...model, gapLoading: true}
    }

  | GapAnalysisLoading => {
      {...model, gapLoading: true}
    }

  | GapAnalysisResult(result) => switch result {
    | Ok(json) => {
        Console.log("Gap analysis complete")
        let parsed = parseGapAnalysisJson(json)
        {...model, gapState: Some(parsed), gapLoading: false}
      }
    | Error(err) => {
        Console.error2("Gap analysis failed:", err)
        {...model, gapLoading: false}
      }
    }

  // Settings
  | SaveSettings => {
      Console.log("Saving settings to backend...")
      model
    }

  | LoadSettings => {
      Console.log("Loading settings from backend...")
      model
    }

  | SettingsSaved(result) => switch result {
    | Ok() => {
        Console.log("Settings saved successfully")
        model
      }
    | Error(err) => {
        Console.error2("Failed to save settings:", err)
        model
      }
    }

  // WebSocket messages
  | WsConnect => {
      Console.log("WebSocket connect requested")
      model
    }

  | WsDisconnect => {
      Console.log("WebSocket disconnect requested")
      {...model, wsState: Disconnected}
    }

  | WsConnectionStateChanged(state) => {
      {...model, wsState: state}
    }

  | WsValidate => {
      Console.log("WebSocket validate requested")
      model
    }

  | WsValidationResult(json) => {
      // Parse the validation result from the "data" wrapper
      switch jsonDict(json) {
      | Some(d) =>
        switch field(d, "data") {
        | Some(data) =>
          switch jsonDict(data) {
          | Some(dd) => {
              let valid = switch field(dd, "valid") {
              | Some(v) => jsonBool(v, false)
              | None => false
              }
              let errors = switch field(dd, "errors") {
              | Some(v) => jsonStringArray(v)
              | None => []
              }
              let warnings = switch field(dd, "warnings") {
              | Some(v) => jsonStringArray(v)
              | None => []
              }
              let result: validationResult = {valid, errors, warnings}
              {...model, validationResult: Some(result)}
            }
          | None => model
          }
        | None => model
        }
      | None => model
      }
    }

  | WsSecurityScan => {
      Console.log("WebSocket security scan requested")
      {...model, securityLoading: true}
    }

  | WsSecurityResult(json) => {
      switch jsonDict(json) {
      | Some(d) =>
        switch field(d, "data") {
        | Some(data) => {
            let parsed = parseSecurityScanJson(data)
            {...model, securityState: Some(parsed), securityLoading: false}
          }
        | None => {...model, securityLoading: false}
        }
      | None => {...model, securityLoading: false}
      }
    }

  | WsGapAnalysis => {
      Console.log("WebSocket gap analysis requested")
      {...model, gapLoading: true}
    }

  | WsGapResult(json) => {
      switch jsonDict(json) {
      | Some(d) =>
        switch field(d, "data") {
        | Some(data) => {
            let parsed = parseGapAnalysisJson(data)
            {...model, gapState: Some(parsed), gapLoading: false}
          }
        | None => {...model, gapLoading: false}
        }
      | None => {...model, gapLoading: false}
      }
    }

  | SettingsLoaded(result) => switch result {
    | Ok(json) => {
        // Parse JSON into settings fields
        let obj = switch json {
        | Object(d) => Some(d)
        | _ => None
        }
        switch obj {
        | Some(d) => {
            let theme = switch Dict.get(d, "theme") {
            | Some(v) =>
              switch v {
              | String(s) => s
              | _ => model.settings.theme
              }
            | None => model.settings.theme
            }
            let defaultRuntime = switch Dict.get(d, "defaultRuntime") {
            | Some(v) =>
              switch v {
              | String(s) => s
              | _ => model.settings.defaultRuntime
              }
            | None => model.settings.defaultRuntime
            }
            let autoSave = switch Dict.get(d, "autoSave") {
            | Some(v) =>
              switch v {
              | Boolean(b) => b
              | _ => model.settings.autoSave
              }
            | None => model.settings.autoSave
            }
            let backendUrl = switch Dict.get(d, "backendUrl") {
            | Some(v) =>
              switch v {
              | String(s) => s
              | _ => model.settings.backendUrl
              }
            | None => model.settings.backendUrl
            }
            let newSettings: settingsConfig = {
              theme,
              defaultRuntime,
              autoSave,
              backendUrl,
            }
            {...model, settings: newSettings}
          }
        | None => model
        }
      }
    | Error(err) => {
        Console.error2("Failed to load settings:", err)
        model
      }
    }
  }
}
