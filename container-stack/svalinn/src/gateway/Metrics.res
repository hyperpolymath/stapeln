// SPDX-License-Identifier: PMPL-1.0-or-later
// Prometheus-compatible metrics collection for Svalinn Edge Gateway
//
// Provides in-memory counters, gauges, and simplified histograms.
// No external dependencies — all state held in mutable refs.

// ---- Counter ----

// A monotonically increasing counter (e.g. total requests).
type counter = {
  name: string,
  help: string,
  mutable value: float,
}

let makeCounter = (~name: string, ~help: string): counter => {
  name,
  help,
  value: 0.0,
}

let increment = (counter: counter): unit => {
  counter.value = counter.value +. 1.0
}

let incrementBy = (counter: counter, n: float): unit => {
  counter.value = counter.value +. n
}

// ---- Gauge ----

// A value that can go up or down (e.g. active containers).
type gauge = {
  name: string,
  help: string,
  mutable value: float,
}

let makeGauge = (~name: string, ~help: string): gauge => {
  name,
  help,
  value: 0.0,
}

let setGauge = (gauge: gauge, value: float): unit => {
  gauge.value = value
}

// ---- Histogram (simplified) ----

// A simplified histogram that counts observations into fixed buckets
// and tracks sum + count for average computation.
type histogram = {
  name: string,
  help: string,
  buckets: array<float>,
  mutable counts: array<float>,
  mutable sum: float,
  mutable count: float,
}

let makeHistogram = (~name: string, ~help: string, ~buckets: array<float>): histogram => {
  name,
  help,
  buckets,
  counts: Belt.Array.make(Belt.Array.length(buckets), 0.0),
  sum: 0.0,
  count: 0.0,
}

// Record an observed value into the histogram.
// Increments all bucket counts where the value <= bucket boundary.
let observe = (histogram: histogram, value: float): unit => {
  histogram.sum = histogram.sum +. value
  histogram.count = histogram.count +. 1.0

  Belt.Array.forEachWithIndex(histogram.buckets, (i, boundary) => {
    if value <= boundary {
      let current = switch Belt.Array.get(histogram.counts, i) {
      | Some(v) => v
      | None => 0.0
      }
      Belt.Array.setExn(histogram.counts, i, current +. 1.0)
    }
  })
}

// ---- Global metrics instances ----

// Total HTTP requests received.
let requestsTotal = makeCounter(
  ~name="svalinn_requests_total",
  ~help="Total HTTP requests received",
)

// Total HTTP request errors (5xx responses).
let requestsErrorsTotal = makeCounter(
  ~name="svalinn_requests_errors_total",
  ~help="Total HTTP request errors (5xx)",
)

// Total authentication failures (401/403 responses).
let authFailuresTotal = makeCounter(
  ~name="svalinn_auth_failures_total",
  ~help="Total authentication failures",
)

// HTTP request duration in seconds.
let requestDurationSeconds = makeHistogram(
  ~name="svalinn_request_duration_seconds",
  ~help="HTTP request duration in seconds",
  ~buckets=[0.01, 0.05, 0.1, 0.5, 1.0, 5.0],
)

// Number of active containers (fetched from Vordr on demand).
let containersActive = makeGauge(
  ~name="svalinn_containers_active",
  ~help="Number of currently active containers",
)

// ---- Prometheus text format ----

// Format a single counter in Prometheus exposition format.
let formatCounter = (c: counter): string => {
  `# HELP ${c.name} ${c.help}\n` ++
  `# TYPE ${c.name} counter\n` ++
  `${c.name} ${Belt.Float.toString(c.value)}\n`
}

// Format a single gauge in Prometheus exposition format.
let formatGauge = (g: gauge): string => {
  `# HELP ${g.name} ${g.help}\n` ++
  `# TYPE ${g.name} gauge\n` ++
  `${g.name} ${Belt.Float.toString(g.value)}\n`
}

// Format a simplified histogram in Prometheus exposition format.
let formatHistogram = (h: histogram): string => {
  let header =
    `# HELP ${h.name} ${h.help}\n` ++
    `# TYPE ${h.name} histogram\n`

  // Cumulative bucket counts (Prometheus histograms are cumulative).
  let cumulativeRef = ref(0.0)
  let bucketLines = Belt.Array.mapWithIndex(h.buckets, (i, boundary) => {
    let count = switch Belt.Array.get(h.counts, i) {
    | Some(v) => v
    | None => 0.0
    }
    cumulativeRef := cumulativeRef.contents +. count
    let cumulative = cumulativeRef.contents
    `${h.name}_bucket{le="${Belt.Float.toString(boundary)}"} ${Belt.Float.toString(cumulative)}\n`
  })

  let infLine = `${h.name}_bucket{le="+Inf"} ${Belt.Float.toString(h.count)}\n`
  let sumLine = `${h.name}_sum ${Belt.Float.toString(h.sum)}\n`
  let countLine = `${h.name}_count ${Belt.Float.toString(h.count)}\n`

  header ++
  Js.Array2.joinWith(bucketLines, "") ++
  infLine ++
  sumLine ++
  countLine
}

// Format all registered metrics in Prometheus text exposition format.
// Optionally fetches active container count from Vordr first.
let formatPrometheus = (): string => {
  formatCounter(requestsTotal) ++
  "\n" ++
  formatCounter(requestsErrorsTotal) ++
  "\n" ++
  formatCounter(authFailuresTotal) ++
  "\n" ++
  formatHistogram(requestDurationSeconds) ++
  "\n" ++
  formatGauge(containersActive)
}

// Fetch the active container count from Vordr and update the gauge.
// Silently swallows errors (metrics should never break the gateway).
let refreshContainersActive = async (vordrEndpoint: string): unit => {
  try {
    let response = await Fetch.fetch(
      vordrEndpoint ++ "/api/v1/containers",
      %raw(`{}`),
    )
    if Fetch.Response.ok(response) {
      let body = await Fetch.Response.json(response)
      // Expect an array of containers; count those with state "running".
      switch Js.Json.classify(body) {
      | Js.Json.JSONArray(arr) => {
          let running = Belt.Array.keep(arr, item => {
            switch Js.Json.classify(item) {
            | Js.Json.JSONObject(dict) =>
              switch Js.Dict.get(dict, "state") {
              | Some(stateJson) =>
                switch Js.Json.classify(stateJson) {
                | Js.Json.JSONString("running") => true
                | _ => false
                }
              | None => false
              }
            | _ => false
            }
          })
          setGauge(containersActive, Belt.Int.toFloat(Belt.Array.length(running)))
        }
      | _ => ()
      }
    }
  } catch {
  | _ => () // Silently ignore — gauge keeps its last known value
  }
}
