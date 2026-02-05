// SPDX-License-Identifier: PMPL-1.0-or-later
// TeaRouter.res - Simple TEA-compatible router

// Route type
type route<'a> = list<string> => option<'a>

// Parse URL path into route segments
let parsePath = (path: string): list<string> => {
  path
  ->String.split("/")
  ->List.fromArray
  ->List.keep(segment => String.length(segment) > 0)
}

// Get current URL path
let getCurrentPath = (): string => {
  %raw(`window.location.pathname`)
}

// Navigate to a path (pushState)
let navigate = (path: string): unit => {
  %raw(`window.history.pushState(null, "", path)`)
  // Dispatch popstate event to trigger route change
  %raw(`window.dispatchEvent(new PopStateEvent('popstate'))`)
}

// Replace current path (replaceState)
let replace = (path: string): unit => {
  %raw(`window.history.replaceState(null, "", path)`)
}

// Subscribe to route changes
let onRouteChange = (callback: string => unit): unit => {
  let handler = %raw(`() => callback(window.location.pathname)`)
  %raw(`window.addEventListener('popstate', handler)`)
}

// Simple route matching
module Match = {
  // Match exact path
  let exact = (expected: string, segments: list<string>): bool => {
    let expectedSegments = parsePath(expected)
    expectedSegments == segments
  }

  // Match path prefix
  let prefix = (expected: string, segments: list<string>): bool => {
    let expectedSegments = parsePath(expected)
    let rec check = (exp, seg) =>
      switch (exp, seg) {
      | (list{}, _) => true
      | (_, list{}) => false
      | (list{e, ...eRest}, list{s, ...sRest}) =>
        if e == s {
          check(eRest, sRest)
        } else {
          false
        }
      }
    check(expectedSegments, segments)
  }

  // Match with parameter
  let param = (segments: list<string>): option<(string, list<string>)> => {
    switch segments {
    | list{} => None
    | list{param, ...rest} => Some((param, rest))
    }
  }
}

// Route builder
module Route = {
  type t<'a> = list<string> => option<'a>

  // Create route from exact path
  let make = (path: string, value: 'a): t<'a> => {
    segments =>
      if Match.exact(path, segments) {
        Some(value)
      } else {
        None
      }
  }

  // Create route with parameter
  let makeWithParam = (path: string, constructor: string => 'a): t<'a> => {
    segments =>
      if Match.prefix(path, segments) {
        let remaining = List.drop(segments, List.length(parsePath(path)))
        switch Match.param(remaining) {
        | Some((param, _)) => Some(constructor(param))
        | None => None
        }
      } else {
        None
      }
  }

  // Try multiple routes
  let oneOf = (routes: array<t<'a>>): t<'a> => {
    segments => {
      let rec tryRoutes = routes =>
        switch routes {
        | list{} => None
        | list{route, ...rest} =>
          switch route(segments) {
          | Some(value) => Some(value)
          | None => tryRoutes(rest)
          }
        }
      tryRoutes(Array.toList(routes))
    }
  }
}

// Router component with TEA
module Router = {
  type config<'route, 'msg> = {
    routes: route<'route>,
    toMsg: 'route => 'msg,
    notFound: 'msg,
  }

  // Get current route
  let getRoute = (config: config<'route, 'msg>): 'msg => {
    let path = getCurrentPath()
    let segments = parsePath(path)
    switch config.routes(segments) {
    | Some(route) => config.toMsg(route)
    | None => config.notFound
    }
  }

  // Subscribe to route changes
  let subscriptions = (config: config<'route, 'msg>, dispatch: 'msg => unit): unit => {
    onRouteChange(_path => {
      let msg = getRoute(config)
      dispatch(msg)
    })
  }
}
