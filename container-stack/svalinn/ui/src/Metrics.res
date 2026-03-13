// SPDX-License-Identifier: PMPL-1.0-or-later
// Metrics dashboard component for Svalinn Web UI
//
// Parses Prometheus text-format metrics from /metrics endpoint and renders
// stat cards plus a text-based histogram bar chart.

// ============================================================================
// Types
// ============================================================================

type metricValue = {
  name: string,
  help: string,
  metricType: string,
  value: float,
}

type histogramBucket = {
  le: string,
  count: float,
}

type metricsData = {
  requestsTotal: float,
  errorsTotal: float,
  authFailures: float,
  activeContainers: float,
  histogram: array<histogramBucket>,
  raw: string,
}

let emptyMetrics: metricsData = {
  requestsTotal: 0.0,
  errorsTotal: 0.0,
  authFailures: 0.0,
  activeContainers: 0.0,
  histogram: [],
  raw: "",
}

// ============================================================================
// Prometheus Text Format Parser
// ============================================================================

let parseFloat = (text: string): float =>
  switch Float.fromString(Js.String2.trim(text)) {
  | Some(value) => value
  | None => 0.0
  }

let extractSimpleMetric = (raw: string, metricName: string): float => {
  let lines = Js.String2.split(raw, "\n")
  let result = ref(0.0)
  lines->Belt.Array.forEach(line => {
    let trimmed = Js.String2.trim(line)
    if (
      Js.String2.startsWith(trimmed, metricName) &&
      !Js.String2.startsWith(trimmed, "#")
    ) {
      // Handle both "metric_name 123" and "metric_name{...} 123" formats
      let parts = if Js.String2.includes(trimmed, "}") {
        let afterBrace = switch Js.String2.split(trimmed, "}")->Belt.Array.get(1) {
        | Some(rest) => Js.String2.trim(rest)
        | None => ""
        }
        [metricName, afterBrace]
      } else {
        Js.String2.split(trimmed, " ")
      }
      switch Belt.Array.get(parts, Belt.Array.length(parts) - 1) {
      | Some(valueStr) => result := result.contents +. parseFloat(valueStr)
      | None => ()
      }
    }
  })
  result.contents
}

let extractHistogram = (raw: string, metricName: string): array<histogramBucket> => {
  let bucketPrefix = metricName ++ "_bucket{"
  let lines = Js.String2.split(raw, "\n")
  let buckets: ref<array<histogramBucket>> = ref([])
  lines->Belt.Array.forEach(line => {
    let trimmed = Js.String2.trim(line)
    if Js.String2.startsWith(trimmed, bucketPrefix) {
      // Extract le="..." value
      let leStart = switch Js.String2.indexOf(trimmed, "le=\"") {
      | -1 => None
      | idx => Some(idx + 4)
      }
      switch leStart {
      | Some(start) =>
        let rest = Js.String2.substr(trimmed, ~from=start)
        let leEnd = Js.String2.indexOf(rest, "\"")
        if leEnd > 0 {
          let le = Js.String2.substring(rest, ~from=0, ~to_=leEnd)
          // Get count value after the closing brace
          switch Js.String2.split(trimmed, "}")->Belt.Array.get(1) {
          | Some(countStr) =>
            let count = parseFloat(countStr)
            if le != "+Inf" {
              buckets := Belt.Array.concat(buckets.contents, [{le, count}])
            }
          | None => ()
          }
        }
      | None => ()
      }
    }
  })
  buckets.contents
}

let parseMetrics = (raw: string): metricsData => {
  {
    requestsTotal: extractSimpleMetric(raw, "svalinn_requests_total"),
    errorsTotal: extractSimpleMetric(raw, "svalinn_errors_total"),
    authFailures: extractSimpleMetric(raw, "svalinn_auth_failures_total"),
    activeContainers: extractSimpleMetric(raw, "svalinn_active_containers"),
    histogram: extractHistogram(raw, "svalinn_request_duration_seconds"),
    raw,
  }
}

// ============================================================================
// View
// ============================================================================

let statCard = (~title: string, ~value: string, ~note: string): React.element =>
  <div className="card stat-card">
    <div className="badge"> {React.string(title)} </div>
    <h3> {React.string(value)} </h3>
    <p className="muted"> {React.string(note)} </p>
  </div>

let formatFloat = (value: float): string => {
  let intPart = Belt.Float.toInt(value)
  if value == Belt.Int.toFloat(intPart) {
    Belt.Int.toString(intPart)
  } else {
    Js.Float.toFixedWithPrecision(value, ~digits=2)
  }
}

let histogramBar = (bucket: histogramBucket, maxCount: float): React.element => {
  let widthPercent = if maxCount > 0.0 {
    bucket.count /. maxCount *. 100.0
  } else {
    0.0
  }
  let widthStr = Js.Float.toFixedWithPrecision(widthPercent, ~digits=1) ++ "%"
  <div className="histogram-row" key={bucket.le}>
    <span className="histogram-label"> {React.string(bucket.le ++ "s")} </span>
    <div className="histogram-bar-track">
      <div className="histogram-bar-fill" style={ReactDOM.Style.make(~width=widthStr, ())} />
    </div>
    <span className="histogram-count"> {React.string(formatFloat(bucket.count))} </span>
  </div>
}

let view = (metrics: metricsData, loading: bool, error: option<string>): React.element =>
  <div>
    <div className="header">
      <h1> {React.string("Metrics")} </h1>
      {loading
        ? <div className="loading-spinner" />
        : <span className="badge"> {React.string("Prometheus")} </span>}
    </div>
    {switch error {
    | Some(message) =>
      <div className="error-banner">
        <strong> {React.string("Error: ")} </strong>
        {React.string(message)}
      </div>
    | None => React.null
    }}
    <div className="metrics-grid">
      {statCard(
        ~title="Requests Total",
        ~value=formatFloat(metrics.requestsTotal),
        ~note="Total HTTP requests handled.",
      )}
      {statCard(
        ~title="Errors Total",
        ~value=formatFloat(metrics.errorsTotal),
        ~note="Total error responses.",
      )}
      {statCard(
        ~title="Auth Failures",
        ~value=formatFloat(metrics.authFailures),
        ~note="Failed authentication attempts.",
      )}
      {statCard(
        ~title="Active Containers",
        ~value=formatFloat(metrics.activeContainers),
        ~note="Currently tracked containers.",
      )}
    </div>
    {if Belt.Array.length(metrics.histogram) > 0 {
      let maxCount =
        metrics.histogram
        ->Belt.Array.reduce(0.0, (acc, bucket) => acc > bucket.count ? acc : bucket.count)
      <div className="card">
        <h3> {React.string("Request Duration Distribution")} </h3>
        <div className="histogram-chart">
          {metrics.histogram->Belt.Array.map(bucket => histogramBar(bucket, maxCount))->React.array}
        </div>
      </div>
    } else {
      <div className="card">
        <h3> {React.string("Request Duration")} </h3>
        <p className="muted"> {React.string("No histogram data available.")} </p>
      </div>
    }}
  </div>
