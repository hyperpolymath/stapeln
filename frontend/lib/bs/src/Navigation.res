// SPDX-License-Identifier: PMPL-1.0-or-later
// Navigation.res - Navigation sidebar component

@react.component
let make = (~currentRoute: AppRouter.route, ~onNavigate: AppRouter.route => unit) => {
  <div
    style={ReactDOM.Style.make(
      ~width="240px",
      ~height="100vh",
      ~background="linear-gradient(180deg, #0a0e1a 0%, #1e2431 100%)",
      ~borderRight="2px solid #2a3142",
      ~display="flex",
      ~flexDirection="column",
      ~overflowY="auto",
      (),
    )}
  >
    <div style={ReactDOM.Style.make(~padding="24px 20px", ~borderBottom="1px solid #2a3142", ())}>
      <h1
        style={ReactDOM.Style.make(
          ~fontSize="24px",
          ~fontWeight="700",
          ~background="linear-gradient(135deg, #4a9eff, #7b6cff)",
          ~margin="0",
          (),
        )}
      >
        {"stapeln"->React.string}
      </h1>
      <p style={ReactDOM.Style.make(~fontSize="11px", ~color="#8892a6", ~margin="4px 0 0 0", ())}>
        {"Container Stack Designer"->React.string}
      </p>
    </div>

    <nav style={ReactDOM.Style.make(~flex="1", ~padding="12px 0", ())}>
      {Array.map(AppRouter.navigationItems, item => {
        let isActive = item.route == currentRoute

        <button
          key={AppRouter.routeToPath(item.route)}
          onClick={_ => onNavigate(item.route)}
          style={ReactDOM.Style.make(
            ~width="100%",
            ~display="flex",
            ~alignItems="center",
            ~gap="12px",
            ~padding="12px 20px",
            ~background=isActive ? "rgba(74, 158, 255, 0.15)" : "transparent",
            ~border="none",
            ~borderLeft=isActive ? "3px solid #4a9eff" : "3px solid transparent",
            ~color=isActive ? "#4a9eff" : "#8892a6",
            ~fontSize="14px",
            ~fontWeight=isActive ? "600" : "400",
            ~textAlign="left",
            ~cursor="pointer",
            ~transition="all 0.2s",
            (),
          )}
        >
          <span style={ReactDOM.Style.make(~fontSize="18px", ())}> {item.icon->React.string} </span>
          <span> {item.label->React.string} </span>
        </button>
      })->React.array}
    </nav>

    <div
      style={ReactDOM.Style.make(
        ~padding="20px",
        ~borderTop="1px solid #2a3142",
        ~display="flex",
        ~justifyContent="center",
        (),
      )}
    >
      <IdrisBadge style=Compact />
    </div>
  </div>
}
