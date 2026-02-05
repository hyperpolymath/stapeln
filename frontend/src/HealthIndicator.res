// SPDX-License-Identifier: PMPL-1.0-or-later
// HealthIndicator.res - System health indicator component

@react.component
let make = (~health: int) => {
  let (color, label) = if health >= 90 {
    ("#4caf50", "Excellent")
  } else if health >= 75 {
    ("#4a9eff", "Good")
  } else if health >= 50 {
    ("#ff9800", "Fair")
  } else if health >= 25 {
    ("#f44336", "Poor")
  } else {
    ("#d32f2f", "Critical")
  }

  <div
    style={ReactDOM.Style.make(
      ~display="flex",
      ~alignItems="center",
      ~gap="12px",
      ~padding="12px 20px",
      ~background="rgba(30, 36, 49, 0.8)",
      ~border="2px solid " ++ color,
      ~borderRadius="8px",
      (),
    )}
  >
    <div style={ReactDOM.Style.make(~fontSize="20px", ())}> {" ❤️"->React.string} </div>
    <div>
      <div style={ReactDOM.Style.make(~fontSize="12px", ~color="#8892a6", ~marginBottom="2px", ())}>
        {"System Health"->React.string}
      </div>
      <div style={ReactDOM.Style.make(~fontSize="18px", ~fontWeight="700", ~color, ())}>
        {(Int.toString(health) ++ "% - " ++ label)->React.string}
      </div>
    </div>
  </div>
}
