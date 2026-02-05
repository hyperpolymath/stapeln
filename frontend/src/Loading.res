// SPDX-License-Identifier: PMPL-1.0-or-later
// Loading.res - Loading states and skeletons with WCAG AAA accessibility

// Loading spinner
let spinner = (~size="default", ~color="#0052CC", ~label="Loading...") => {
  let spinnerSize = switch size {
  | "small" => "24px"
  | "large" => "64px"
  | "default" | _ => "40px"
  }

  <div
    role="status"
    ariaLabel=label
    ariaLive=#polite
    style={ReactDOM.Style.make(
      ~display="flex",
      ~alignItems="center",
      ~justifyContent="center",
      ~padding="2rem",
      (),
    )}>
    <div
      style={ReactDOM.Style.make(
        ~width=spinnerSize,
        ~height=spinnerSize,
        ~border="4px solid #E2E8F0",
        ~borderTop="4px solid " ++ color,
        ~borderRadius="50%",
        ~animation="spin 0.8s linear infinite",
        (),
      )}
    />
    <span
      style={ReactDOM.Style.make(
        ~position="absolute",
        ~left="-10000px",
        ~width="1px",
        ~height="1px",
        ~overflow="hidden",
        (),
      )}>
      {label->React.string}
    </span>
  </div>
}

// Skeleton loader for content
let skeleton = (~width="100%", ~height="20px", ~borderRadius="4px", ~marginBottom="0.5rem") => {
  <div
    role="status"
    ariaLabel="Loading content..."
    style={ReactDOM.Style.make(
      ~width,
      ~height,
      ~backgroundColor="#E2E8F0",
      ~borderRadius,
      ~marginBottom,
      ~animation="pulse 1.5s ease-in-out infinite",
      (),
    )}
  />
}

// Skeleton for a list item
let skeletonListItem = () => {
  <div
    style={ReactDOM.Style.make(
      ~padding="1rem",
      ~marginBottom="0.5rem",
      ~border="1px solid #E2E8F0",
      ~borderRadius="8px",
      (),
    )}>
    {skeleton(~width="60%", ~height="16px", ~marginBottom="0.75rem")}
    {skeleton(~width="100%", ~height="14px", ~marginBottom="0.5rem")}
    {skeleton(~width="80%", ~height="14px", ~marginBottom="0")}
  </div>
}

// Full page loading overlay
let overlay = (~message="Loading...", ~isDark=false) => {
  <div
    role="status"
    ariaLabel=message
    ariaLive=#polite
    style={ReactDOM.Style.make(
      ~position="fixed",
      ~top="0",
      ~left="0",
      ~right="0",
      ~bottom="0",
      ~display="flex",
      ~flexDirection="column",
      ~alignItems="center",
      ~justifyContent="center",
      ~backgroundColor=isDark ? "rgba(0, 0, 0, 0.8)" : "rgba(255, 255, 255, 0.9)",
      ~color=isDark ? "#FFFFFF" : "#000000",
      ~zIndex="9999",
      (),
    )}>
    {spinner(~size="large", ~color=isDark ? "#66B2FF" : "#0052CC", ~label=message)}
    <p style={ReactDOM.Style.make(~marginTop="1rem", ~fontSize="1.1rem", ~fontWeight="600", ())}>
      {message->React.string}
    </p>
  </div>
}

// Loading wrapper that shows spinner while loading
let wrapper = (~isLoading, ~children, ~fallback=?, ~size="default") => {
  if isLoading {
    switch fallback {
    | Some(fb) => fb
    | None => spinner(~size)
    }
  } else {
    children
  }
}

// Note: CSS animations should be added to App.css:
// @keyframes spin {
//   0% { transform: rotate(0deg); }
//   100% { transform: rotate(360deg); }
// }
// @keyframes pulse {
//   0%, 100% { opacity: 1; }
//   50% { opacity: 0.5; }
// }
