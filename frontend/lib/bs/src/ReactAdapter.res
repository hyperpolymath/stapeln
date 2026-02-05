// SPDX-License-Identifier: PMPL-1.0-or-later
// ReactAdapter.res - React hooks for DOM Mounter (Phase 3)

open DomMounterEnhanced

// ============================================================================
// REACT HOOKS
// ============================================================================

// Basic mount hook
let useDomMounter = (elementId: string): (bool, option<string>) => {
  let (mounted, setMounted) = React.useState(() => false)
  let (error, setError) = React.useState(() => None)

  React.useEffect(() => {
    // Mount on component mount
    switch mount(elementId) {
    | Ok() => setMounted(_ => true)
    | Error(msg) => setError(_ => Some(msg))
    }

    // Cleanup on component unmount
    Some(
      () => {
        setMounted(_ => false)
        setError(_ => None)
      },
    )
  }, [elementId])

  (mounted, error)
}

// Mount with lifecycle hooks
let useDomMounterWithHooks = (
  elementId: string,
  ~beforeMount: option<string => Result.t<unit, string>>=?,
  ~afterMount: option<string => unit>=?,
  ~beforeUnmount: option<string => unit>=?,
  ~afterUnmount: option<string => unit>=?,
  ~onError: option<string => unit>=?,
  (),
): (bool, option<string>) => {
  let (mounted, setMounted) = React.useState(() => false)
  let (error, setError) = React.useState(() => None)

  React.useEffect(() => {
    let hooks: lifecycleHooks = {
      beforeMount: beforeMount,
      afterMount: afterMount,
      beforeUnmount: beforeUnmount,
      afterUnmount: afterUnmount,
      onError: onError,
    }

    switch mountWithLifecycle(elementId, hooks) {
    | Ok() => setMounted(_ => true)
    | Error(msg) => setError(_ => Some(msg))
    }

    Some(
      () => {
        let _ = unmountWithLifecycle(elementId, hooks)
        setMounted(_ => false)
      },
    )
  }, [elementId])

  (mounted, error)
}

// Mount with security policy
let useDomMounterSecure = (
  elementId: string,
  policy: option<DomMounterSecurity.securityPolicy>,
): (bool, option<string>) => {
  let (mounted, setMounted) = React.useState(() => false)
  let (error, setError) = React.useState(() => None)

  React.useEffect(() => {
    let securityPolicy = switch policy {
    | Some(p) => p
    | None => DomMounterSecurity.defaultSecurityPolicy
    }

    switch DomMounterSecurity.mountWithPolicy(elementId, securityPolicy) {
    | Ok() => setMounted(_ => true)
    | Error(msg) => setError(_ => Some(msg))
    }

    Some(() => {
      setMounted(_ => false)
    })
  }, [elementId])

  (mounted, error)
}

// Mount with monitoring
let useDomMounterMonitored = (elementId: string): (bool, option<string>, healthStatus) => {
  let (mounted, setMounted) = React.useState(() => false)
  let (error, setError) = React.useState(() => None)
  let (health, setHealth) = React.useState(() => Healthy)

  React.useEffect(() => {
    // Start monitoring
    let _ = startMonitoring(elementId)

    // Mount
    switch mount(elementId) {
    | Ok() => setMounted(_ => true)
    | Error(msg) => setError(_ => Some(msg))
    }

    // Health check interval
    let intervalId = Js.Global.setInterval(() => {
      let (healthStatus, _msg) = healthCheck(elementId)
      setHealth(_ => healthStatus)
    }, 5000) // Check every 5 seconds

    Some(
      () => {
        Js.Global.clearInterval(intervalId)
        let _ = stopMonitoring(elementId)
        setMounted(_ => false)
      },
    )
  }, [elementId])

  (mounted, error, health)
}
