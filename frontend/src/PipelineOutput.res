// SPDX-License-Identifier: PMPL-1.0-or-later
// PipelineOutput.res - Right panel for the Assembly Pipeline Designer
//
// Four tabs: Preview (generated artifact with syntax highlighting),
// Validation (errors, warnings, security score), Output (generated files),
// and Security (supply chain verification, CVE results, compliance).

open PipelineModel

// ---------------------------------------------------------------------------
// External clipboard API binding
// ---------------------------------------------------------------------------

@val @scope(("navigator", "clipboard"))
external writeText: string => Promise.t<unit> = "writeText"

// ---------------------------------------------------------------------------
// Syntax highlighting helpers (basic keyword colouring)
// ---------------------------------------------------------------------------

// Containerfile keywords that should be highlighted
let containerfileKeywords = [
  "FROM",
  "RUN",
  "COPY",
  "ADD",
  "ENV",
  "WORKDIR",
  "USER",
  "EXPOSE",
  "LABEL",
  "ARG",
  "ENTRYPOINT",
  "CMD",
  "VOLUME",
  "HEALTHCHECK",
  "SHELL",
  "STOPSIGNAL",
  "ONBUILD",
  "MAINTAINER",
]

// Wrap recognised keywords in coloured spans. Returns an array of React elements
// representing a single line of highlighted source.
let highlightLine = (line: string): React.element => {
  // Check if the line starts with a known keyword
  let trimmed = String.trim(line)
  let isKeywordLine = Array.some(containerfileKeywords, kw =>
    String.startsWith(trimmed, kw)
  )
  let isComment = String.startsWith(trimmed, "#")

  if isComment {
    <span className="text-gray-500 italic"> {line->React.string} </span>
  } else if isKeywordLine {
    // Split at first space to colour the keyword
    let firstSpace = String.indexOf(trimmed, " ")
    if firstSpace > 0 {
      let keyword = String.slice(trimmed, ~start=0, ~end=firstSpace)
      let rest = String.sliceToEnd(trimmed, ~start=firstSpace)
      <>
        <span className="text-blue-400 font-bold"> {keyword->React.string} </span>
        <span className="text-gray-200"> {rest->React.string} </span>
      </>
    } else {
      <span className="text-blue-400 font-bold"> {trimmed->React.string} </span>
    }
  } else {
    <span className="text-gray-300"> {line->React.string} </span>
  }
}

// ---------------------------------------------------------------------------
// Sub-components
// ---------------------------------------------------------------------------

// Preview tab: shows generated artifact with basic syntax highlighting
module PreviewPanel = {
  @react.component
  let make = (
    ~generatedOutput: option<string>,
    ~outputFormat: outputFormat,
    ~dispatch: pipelineMsg => unit,
  ) => {
    let (copied, setCopied) = React.useState(() => false)

    let content = switch generatedOutput {
    | Some(text) => text
    | None => "# No output generated yet.\n# Add nodes and connections to your pipeline,\n# then the preview will appear here."
    }

    let handleCopy = () => {
      let _ = writeText(content)->Promise.then(_ => {
        setCopied(_ => true)
        // Reset after 2 seconds
        let _ = setTimeout(() => setCopied(_ => false), 2000)
        Promise.resolve()
      })
    }

    let handleDownload = () => {
      let filename = switch outputFormat {
      | Containerfile => "Containerfile"
      | SelurCompose => "selur-compose.toml"
      | PodmanCompose => "podman-compose.yml"
      | K8sManifest => "k8s-manifest.yaml"
      | HelmChart => "Chart.yaml"
      | OciBundle => "oci-layout.json"
      }
      dispatch(DownloadFile(filename))
    }

    <>
      // Format selector + action buttons
      <div className="flex items-center gap-2 mb-3">
        <label className="sr-only" htmlFor="format-select">
          {"Output format"->React.string}
        </label>
        <select
          id="format-select"
          value={outputFormatToString(outputFormat)}
          onChange={e => {
            let v: string = ReactEvent.Form.target(e)["value"]
            let fmt = switch v {
            | "Containerfile" => Containerfile
            | "selur-compose" => SelurCompose
            | "Podman Compose" => PodmanCompose
            | "Kubernetes Manifest" => K8sManifest
            | "Helm Chart" => HelmChart
            | "OCI Bundle" => OciBundle
            | _ => Containerfile
            }
            dispatch(SetOutputFormat(fmt))
          }}
          className="flex-1 px-2 py-1.5 text-sm rounded-md bg-gray-800 border border-gray-600
                     text-gray-100 focus:outline-none focus:ring-2 focus:ring-blue-500"
          ariaLabel="Select output format"
        >
          <option value="Containerfile"> {"Containerfile"->React.string} </option>
          <option value="selur-compose"> {"selur-compose.toml"->React.string} </option>
          <option value="Podman Compose"> {"podman-compose.yml"->React.string} </option>
          <option value="Kubernetes Manifest"> {"Kubernetes YAML"->React.string} </option>
          <option value="Helm Chart"> {"Helm Chart"->React.string} </option>
          <option value="OCI Bundle"> {"OCI Bundle"->React.string} </option>
        </select>
        <button
          className={"px-2 py-1.5 text-xs rounded-md border transition-colors "
            ++ "focus:outline-none focus:ring-2 focus:ring-blue-500 "
            ++ (copied
              ? "bg-green-700 border-green-600 text-green-100"
              : "bg-gray-800 border-gray-600 text-gray-300 hover:bg-gray-700")}
          onClick={_ => handleCopy()}
          ariaLabel="Copy generated output to clipboard"
        >
          {(copied ? "Copied" : "Copy")->React.string}
        </button>
        <button
          className="px-2 py-1.5 text-xs rounded-md bg-gray-800 border border-gray-600
                     text-gray-300 hover:bg-gray-700 transition-colors
                     focus:outline-none focus:ring-2 focus:ring-blue-500"
          onClick={_ => handleDownload()}
          ariaLabel="Download generated file"
        >
          {"Download"->React.string}
        </button>
      </div>

      // Syntax-highlighted code block
      <pre
        className="flex-1 overflow-auto rounded-lg bg-gray-950 border border-gray-700
                   p-3 text-xs font-mono leading-relaxed"
        role="region"
        ariaLabel="Generated artifact preview"
        tabIndex={0}
      >
        <code>
          {content
          ->String.split("\n")
          ->Array.mapWithIndex((idx, line) => {
            <div key={Int.toString(idx)} className="min-h-[1.2em]">
              // Line number gutter
              <span className="inline-block w-8 text-right pr-2 text-gray-600 select-none">
                {Int.toString(idx + 1)->React.string}
              </span>
              {highlightLine(line)}
            </div>
          })
          ->React.array}
        </code>
      </pre>
    </>
  }
}

// Validation tab: errors, warnings, security score, estimated size
module ValidationPanel = {
  @react.component
  let make = (
    ~validation: option<pipelineValidation>,
    ~dispatch: pipelineMsg => unit,
  ) => {
    switch validation {
    | None =>
      <div className="flex flex-col items-center justify-center py-8 text-center">
        <p className="text-sm text-gray-400 mb-3">
          {"No validation results yet."->React.string}
        </p>
        <button
          className="px-4 py-2 text-sm rounded-md bg-blue-600 text-white hover:bg-blue-500
                     transition-colors focus:outline-none focus:ring-2 focus:ring-blue-400"
          onClick={_ => dispatch(RunValidation)}
          ariaLabel="Run pipeline validation"
        >
          {"Run Validation"->React.string}
        </button>
      </div>

    | Some(v) =>
      <div className="flex flex-col gap-3">
        // Summary cards
        <div className="grid grid-cols-2 gap-2">
          // Security score
          <div
            className="p-2 rounded-lg bg-gray-800/60 border border-gray-700"
            role="status"
            ariaLabel={"Security score: " ++ Float.toString(v.securityScore)}
          >
            <div className="text-xs text-gray-400 mb-1">
              {"Security Score"->React.string}
            </div>
            <div
              className={
                "text-2xl font-bold "
                ++ (v.securityScore >= 80.0
                  ? "text-green-400"
                  : v.securityScore >= 50.0
                  ? "text-yellow-400"
                  : "text-red-400")
              }
            >
              {(Float.toFixed(v.securityScore, ~digits=0) ++ "/100")->React.string}
            </div>
          </div>

          // Estimated image size
          <div className="p-2 rounded-lg bg-gray-800/60 border border-gray-700">
            <div className="text-xs text-gray-400 mb-1">
              {"Est. Image Size"->React.string}
            </div>
            <div className="text-2xl font-bold text-gray-100">
              {if v.estimatedImageSize < 1024 * 1024 {
                let kb = Float.fromInt(v.estimatedImageSize) /. 1024.0
                Float.toFixed(kb, ~digits=1) ++ " KB"
              } else {
                let mb = Float.fromInt(v.estimatedImageSize) /. 1024.0 /. 1024.0
                Float.toFixed(mb, ~digits=1) ++ " MB"
              }->React.string}
            </div>
          </div>
        </div>

        // Overall status
        <div
          className={
            "p-2 rounded-lg text-sm font-medium text-center "
            ++ (v.isValid
              ? "bg-green-900/30 text-green-300 border border-green-700"
              : "bg-red-900/30 text-red-300 border border-red-700")
          }
          role="status"
        >
          {(v.isValid ? "Pipeline is valid" : "Pipeline has errors")->React.string}
        </div>

        // Errors list
        {Array.length(v.errors) > 0
          ? <div className="flex flex-col gap-1" role="list" ariaLabel="Validation errors">
              <h3 className="text-xs font-bold uppercase text-red-400 tracking-wider mb-1">
                {"Errors"->React.string}
              </h3>
              {v.errors
              ->Array.mapWithIndex((idx, issue) => {
                <button
                  key={"err-" ++ Int.toString(idx)}
                  role="listitem"
                  className="flex items-start gap-2 w-full text-left p-2 rounded-md
                             bg-red-900/20 border border-red-800/40 hover:bg-red-900/30
                             transition-colors text-sm
                             focus:outline-none focus:ring-2 focus:ring-red-500"
                  onClick={_ => dispatch(SelectNode(Some(issue.nodeId)))}
                  ariaLabel={"Error: " ++ issue.message ++ ". Click to select node."}
                >
                  <span className="text-red-400 font-bold flex-shrink-0">
                    {"!"->React.string}
                  </span>
                  <span className="text-red-200"> {issue.message->React.string} </span>
                </button>
              })
              ->React.array}
            </div>
          : React.null}

        // Warnings list
        {Array.length(v.warnings) > 0
          ? <div className="flex flex-col gap-1" role="list" ariaLabel="Validation warnings">
              <h3 className="text-xs font-bold uppercase text-yellow-400 tracking-wider mb-1">
                {"Warnings"->React.string}
              </h3>
              {v.warnings
              ->Array.mapWithIndex((idx, issue) => {
                <button
                  key={"warn-" ++ Int.toString(idx)}
                  role="listitem"
                  className="flex items-start gap-2 w-full text-left p-2 rounded-md
                             bg-yellow-900/20 border border-yellow-800/40 hover:bg-yellow-900/30
                             transition-colors text-sm
                             focus:outline-none focus:ring-2 focus:ring-yellow-500"
                  onClick={_ => dispatch(SelectNode(Some(issue.nodeId)))}
                  ariaLabel={"Warning: " ++ issue.message ++ ". Click to select node."}
                >
                  <span className="text-yellow-400 font-bold flex-shrink-0">
                    {"~"->React.string}
                  </span>
                  <span className="text-yellow-200"> {issue.message->React.string} </span>
                </button>
              })
              ->React.array}
            </div>
          : React.null}

        // Re-run button
        <button
          className="mt-2 px-4 py-2 text-sm rounded-md bg-gray-700 text-gray-200
                     hover:bg-gray-600 transition-colors
                     focus:outline-none focus:ring-2 focus:ring-blue-500"
          onClick={_ => dispatch(RunValidation)}
          ariaLabel="Re-run validation"
        >
          {"Re-validate"->React.string}
        </button>
      </div>
    }
  }
}

// Output files tab: list of generated files with export options
module OutputFilesPanel = {
  @react.component
  let make = (
    ~generatedOutput: option<string>,
    ~outputFormat: outputFormat,
    ~dispatch: pipelineMsg => unit,
  ) => {
    // Derive a simple file list from current output
    let files = switch generatedOutput {
    | None => []
    | Some(content) =>
      let filename = switch outputFormat {
      | Containerfile => "Containerfile"
      | SelurCompose => "selur-compose.toml"
      | PodmanCompose => "podman-compose.yml"
      | K8sManifest => "k8s-manifest.yaml"
      | HelmChart => "Chart.yaml"
      | OciBundle => "oci-layout.json"
      }
      [{name: filename, size: String.length(content), preview: content}]
    }

    <div className="flex flex-col gap-3">
      {Array.length(files) === 0
        ? <div className="text-center py-8">
            <p className="text-sm text-gray-400 mb-1">
              {"No generated files yet."->React.string}
            </p>
            <p className="text-xs text-gray-500">
              {"Build your pipeline to generate output files."->React.string}
            </p>
          </div>
        : <>
            <div className="flex flex-col gap-2" role="list" ariaLabel="Generated files">
              {files
              ->Array.mapWithIndex((idx, file) => {
                <div
                  key={file.name ++ Int.toString(idx)}
                  role="listitem"
                  className="p-3 rounded-lg bg-gray-800/60 border border-gray-700"
                >
                  <div className="flex items-center justify-between mb-2">
                    <span className="text-sm font-medium text-gray-100">
                      {file.name->React.string}
                    </span>
                    <span className="text-xs text-gray-500">
                      {(Int.toString(file.size) ++ " bytes")->React.string}
                    </span>
                  </div>
                  <pre
                    className="text-xs font-mono text-gray-400 bg-gray-950 rounded p-2
                               max-h-24 overflow-auto"
                  >
                    {String.slice(file.preview, ~start=0, ~end=200)->React.string}
                    {String.length(file.preview) > 200
                      ? "..."->React.string
                      : React.null}
                  </pre>
                </div>
              })
              ->React.array}
            </div>

            // Export actions
            <div className="flex gap-2">
              <button
                className="flex-1 px-3 py-2 text-sm rounded-md bg-blue-600 text-white
                           hover:bg-blue-500 transition-colors
                           focus:outline-none focus:ring-2 focus:ring-blue-400"
                onClick={_ => dispatch(ExportAllZip)}
                ariaLabel="Export all files as ZIP archive"
              >
                {"Export ZIP"->React.string}
              </button>
              <button
                className="flex-1 px-3 py-2 text-sm rounded-md bg-green-700 text-white
                           hover:bg-green-600 transition-colors
                           focus:outline-none focus:ring-2 focus:ring-green-400"
                onClick={_ => dispatch(Deploy)}
                ariaLabel="Deploy pipeline to backend"
              >
                {"Deploy"->React.string}
              </button>
            </div>
          </>}
    </div>
  }
}

// Security tab: supply chain status, CVE results, compliance
module SecurityPanel = {
  @react.component
  let make = (
    ~pipeline: pipeline,
    ~dispatch: pipelineMsg => unit,
  ) => {
    // Derive supply chain status from node kinds present in the pipeline
    let hasSignStep = Array.some(pipeline.nodes, n =>
      switch n.kind {
      | SignStep(_) => true
      | _ => false
      }
    )
    let hasSbom = Array.some(pipeline.nodes, n =>
      switch n.kind {
      | SbomGenerate(_) => true
      | _ => false
      }
    )
    let hasProvenance = Array.some(pipeline.nodes, n =>
      switch n.kind {
      | ProvenanceAttach(_) => true
      | _ => false
      }
    )
    let hasSecurityGate = Array.some(pipeline.nodes, n =>
      switch n.kind {
      | SecurityGate(_) => true
      | _ => false
      }
    )

    let checklistItems = [
      ("Image signing", hasSignStep, "Add a Sign (Cerro Torre) node"),
      ("SBOM generation", hasSbom, "Add an SBOM Generate node"),
      ("Provenance attestation", hasProvenance, "Add a Provenance Attach node"),
      ("Security gate", hasSecurityGate, "Add a Svalinn Gate or Vulnerability Scan node"),
    ]

    let passCount = Array.reduce(checklistItems, 0, (acc, (_, ok, _)) =>
      ok ? acc + 1 : acc
    )

    <div className="flex flex-col gap-4">
      // Supply chain verification summary
      <div className="p-3 rounded-lg bg-gray-800/60 border border-gray-700">
        <h3 className="text-xs font-bold uppercase text-gray-400 tracking-wider mb-2">
          {"Supply Chain Verification"->React.string}
        </h3>
        <div
          className="flex items-center gap-2 mb-2"
          role="status"
          ariaLabel={"Supply chain: " ++ Int.toString(passCount) ++ " of " ++ Int.toString(Array.length(checklistItems)) ++ " checks passing"}
        >
          <span
            className={
              "text-xl font-bold "
              ++ (passCount === Array.length(checklistItems)
                ? "text-green-400"
                : passCount >= 2
                ? "text-yellow-400"
                : "text-red-400")
            }
          >
            {(Int.toString(passCount) ++ "/" ++ Int.toString(Array.length(checklistItems)))->React.string}
          </span>
          <span className="text-sm text-gray-400">
            {"checks passing"->React.string}
          </span>
        </div>

        <div className="flex flex-col gap-1.5" role="list" ariaLabel="Supply chain checklist">
          {checklistItems
          ->Array.mapWithIndex((idx, (label, ok, hint)) => {
            <div
              key={Int.toString(idx)}
              role="listitem"
              className="flex items-center gap-2 text-sm"
              ariaLabel={label ++ (ok ? " - passing" : " - not configured")}
            >
              <span
                className={
                  "w-5 h-5 flex items-center justify-center rounded-full text-xs font-bold "
                  ++ (ok
                    ? "bg-green-800 text-green-300"
                    : "bg-gray-700 text-gray-500")
                }
              >
                {(ok ? "+" : "-")->React.string}
              </span>
              <span className={ok ? "text-gray-200" : "text-gray-500"}>
                {label->React.string}
              </span>
              {ok
                ? React.null
                : <span className="text-xs text-gray-600 ml-auto">
                    {hint->React.string}
                  </span>}
            </div>
          })
          ->React.array}
        </div>
      </div>

      // Policy compliance
      <div className="p-3 rounded-lg bg-gray-800/60 border border-gray-700">
        <h3 className="text-xs font-bold uppercase text-gray-400 tracking-wider mb-2">
          {"Policy Compliance"->React.string}
        </h3>
        {hasSecurityGate
          ? <div className="text-sm text-green-300">
              {"Security policy enforcement is active."->React.string}
            </div>
          : <div className="text-sm text-yellow-300">
              {"No security gate configured. Add a Svalinn Gate for policy enforcement."->React.string}
            </div>}
      </div>

      // Recommendations
      <div className="p-3 rounded-lg bg-gray-800/60 border border-gray-700">
        <h3 className="text-xs font-bold uppercase text-gray-400 tracking-wider mb-2">
          {"Recommendations"->React.string}
        </h3>
        <ul className="flex flex-col gap-1.5 text-sm text-gray-300" role="list">
          {!hasSignStep
            ? <li role="listitem" className="flex items-start gap-1.5">
                <span className="text-blue-400 flex-shrink-0"> {"->"->React.string} </span>
                {"Add image signing for tamper-proof verification"->React.string}
              </li>
            : React.null}
          {!hasSbom
            ? <li role="listitem" className="flex items-start gap-1.5">
                <span className="text-blue-400 flex-shrink-0"> {"->"->React.string} </span>
                {"Generate an SBOM for dependency transparency"->React.string}
              </li>
            : React.null}
          {!hasProvenance
            ? <li role="listitem" className="flex items-start gap-1.5">
                <span className="text-blue-400 flex-shrink-0"> {"->"->React.string} </span>
                {"Attach SLSA provenance for build reproducibility"->React.string}
              </li>
            : React.null}
          {!hasSecurityGate
            ? <li role="listitem" className="flex items-start gap-1.5">
                <span className="text-blue-400 flex-shrink-0"> {"->"->React.string} </span>
                {"Add a vulnerability scan before pushing to registry"->React.string}
              </li>
            : React.null}
          {passCount === Array.length(checklistItems)
            ? <li role="listitem" className="text-green-300">
                {"All recommended security measures are in place."->React.string}
              </li>
            : React.null}
        </ul>
      </div>

      // Run full analysis button
      <button
        className="px-4 py-2 text-sm rounded-md bg-purple-700 text-white
                   hover:bg-purple-600 transition-colors
                   focus:outline-none focus:ring-2 focus:ring-purple-400"
        onClick={_ => dispatch(RunSecurityAnalysis)}
        ariaLabel="Run full security analysis"
      >
        {"Run Full Security Analysis"->React.string}
      </button>
    </div>
  }
}

// ---------------------------------------------------------------------------
// Main component
// ---------------------------------------------------------------------------

// Internal type for the output files panel file entries
type fileEntry = {
  name: string,
  size: int,
  preview: string,
}

@react.component
let make = (
  ~state: pipelineDesignerState,
  ~dispatch: pipelineMsg => unit,
) => {
  let panels = state.panels
  let activeTab = panels.activeRightTab

  if panels.rightCollapsed {
    // Collapsed state: narrow strip with expand button
    <aside
      className="flex flex-col items-center py-4 bg-gray-900 border-l border-gray-700"
      style={{width: "40px", minWidth: "40px"}}
      role="complementary"
      ariaLabel="Pipeline output (collapsed)"
    >
      <button
        className="p-1 rounded hover:bg-gray-700 text-gray-400 hover:text-gray-100
                   focus:outline-none focus:ring-2 focus:ring-blue-500"
        onClick={_ => dispatch(ToggleOutputCollapsed)}
        ariaLabel="Expand output panel"
        title="Expand output"
      >
        {"<"->React.string}
      </button>
    </aside>
  } else {
    <aside
      className="flex flex-col bg-gray-900 border-l border-gray-700 overflow-hidden"
      style={{
        width: Float.toString(panels.rightPanelWidth) ++ "px",
        minWidth: "240px",
        maxWidth: "500px",
      }}
      role="complementary"
      ariaLabel="Pipeline output"
    >
      // Panel header with collapse button
      <div className="flex items-center justify-between px-3 py-2 border-b border-gray-700">
        <h2 className="text-sm font-bold text-gray-200 tracking-wide uppercase">
          {"Output"->React.string}
        </h2>
        <button
          className="p-1 rounded hover:bg-gray-700 text-gray-400 hover:text-gray-100 text-xs
                     focus:outline-none focus:ring-2 focus:ring-blue-500"
          onClick={_ => dispatch(ToggleOutputCollapsed)}
          ariaLabel="Collapse output panel"
          title="Collapse"
        >
          {">"->React.string}
        </button>
      </div>

      // Tab bar
      <div
        className="flex border-b border-gray-700"
        role="tablist"
        ariaLabel="Output tabs"
      >
        {[
          (Preview, "Preview"),
          (Validation, "Validation"),
          (Output, "Output"),
          (Security, "Security"),
        ]
        ->Array.map(((tab, label)) => {
          let isActive = activeTab == tab
          <button
            key={label}
            role="tab"
            ariaSelected={isActive}
            className={
              "flex-1 px-2 py-2 text-xs font-medium transition-colors "
              ++ "focus:outline-none focus:ring-2 focus:ring-inset focus:ring-blue-500 "
              ++ (isActive
                ? "text-blue-400 border-b-2 border-blue-400"
                : "text-gray-400 hover:text-gray-200")
            }
            onClick={_ => dispatch(SetOutputTab(tab))}
          >
            {label->React.string}
          </button>
        })
        ->React.array}
      </div>

      // Tab content (scrollable, flex-grow to fill remaining space)
      <div className="flex-1 overflow-y-auto px-3 py-3 flex flex-col" role="tabpanel">
        {switch activeTab {
        | Preview =>
          <PreviewPanel
            generatedOutput={state.generatedOutput}
            outputFormat={state.outputFormat}
            dispatch
          />
        | Validation =>
          <ValidationPanel
            validation={state.validation}
            dispatch
          />
        | Output =>
          <OutputFilesPanel
            generatedOutput={state.generatedOutput}
            outputFormat={state.outputFormat}
            dispatch
          />
        | Security =>
          <SecurityPanel
            pipeline={state.pipeline}
            dispatch
          />
        }}
      </div>
    </aside>
  }
}
