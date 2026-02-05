// SPDX-License-Identifier: PMPL-1.0-or-later
// ErrorBoundary.res - React error boundary to catch and handle errors gracefully

type state = {
  hasError: bool,
  error: option<string>,
}

type action =
  | CatchError(string)
  | Reset

let initialState = {
  hasError: false,
  error: None,
}

let reducer = (state, action) => {
  switch action {
  | CatchError(errorMsg) => {
      hasError: true,
      error: Some(errorMsg),
    }
  | Reset => initialState
  }
}

@react.component
let make = (~children, ~fallback=?, ~onError=?) => {
  let (state, dispatch) = React.useReducer(reducer, initialState)

  // Error handler
  let handleError = React.useCallback1((error, errorInfo) => {
    let errorMsg = switch error {
    | Some(e) => String.make(e)
    | None => "Unknown error occurred"
    }

    dispatch(CatchError(errorMsg))

    // Call optional onError callback
    switch onError {
    | Some(fn) => fn(errorMsg)
    | None => ()
    }

    // Log to console in development
    Console.error2("Error caught by ErrorBoundary:", error)
    Console.error2("Error info:", errorInfo)
  }, [onError])

  // Reset error state
  let handleReset = React.useCallback0(() => {
    dispatch(Reset)
  })

  if state.hasError {
    // Use custom fallback if provided, otherwise show default error UI
    switch fallback {
    | Some(fallbackUI) => fallbackUI
    | None =>
      <div
        role="alert"
        ariaLive=#assertive
        style={ReactDOM.Style.make(
          ~display="flex",
          ~flexDirection="column",
          ~alignItems="center",
          ~justifyContent="center",
          ~minHeight="100vh",
          ~padding="2rem",
          ~backgroundColor="#FFF5F5",
          ~color="#C53030",
          (),
        )}
      >
        <div style={ReactDOM.Style.make(~maxWidth="600px", ~textAlign="center", ())}>
          <h1
            style={ReactDOM.Style.make(
              ~fontSize="2rem",
              ~fontWeight="700",
              ~marginBottom="1rem",
              (),
            )}
          >
            {"⚠️ Something went wrong"->React.string}
          </h1>

          <p
            style={ReactDOM.Style.make(~fontSize="1rem", ~marginBottom="2rem", ~opacity="0.8", ())}
          >
            {switch state.error {
            | Some(msg) => msg
            | None => "An unexpected error occurred. Please try refreshing the page."
            }->React.string}
          </p>

          <div
            style={ReactDOM.Style.make(~display="flex", ~gap="1rem", ~justifyContent="center", ())}
          >
            <button
              onClick={_ => handleReset()}
              ariaLabel="Try again"
              style={ReactDOM.Style.make(
                ~padding="0.75rem 1.5rem",
                ~backgroundColor="#C53030",
                ~color="white",
                ~border="none",
                ~borderRadius="6px",
                ~fontSize="1rem",
                ~fontWeight="600",
                ~cursor="pointer",
                (),
              )}
            >
              {"Try Again"->React.string}
            </button>

            <button
              onClick={_ => {
                %raw(`window.location.href = '/'`)
              }}
              ariaLabel="Go to home page"
              style={ReactDOM.Style.make(
                ~padding="0.75rem 1.5rem",
                ~backgroundColor="white",
                ~color="#C53030",
                ~border="2px solid #C53030",
                ~borderRadius="6px",
                ~fontSize="1rem",
                ~fontWeight="600",
                ~cursor="pointer",
                (),
              )}
            >
              {"Go Home"->React.string}
            </button>
          </div>
        </div>
      </div>
    }
  } else {
    children
  }
}

// Helper to wrap components with error boundary
let wrap = (~children) => {
  <make> {children} </make>
}
