// SPDX-License-Identifier: PMPL-1.0-or-later
// Breadcrumb.res - Breadcrumb navigation component

@react.component
let make = (~currentRoute: AppRouter.route) => {
  let meta = AppRouter.getRouteMeta(currentRoute)

  <div
    style={ReactDOM.Style.make(
      ~display="flex",
      ~alignItems="center",
      ~gap="8px",
      ~padding="12px 20px",
      ~background="rgba(30, 36, 49, 0.6)",
      ~borderBottom="1px solid #2a3142",
      (),
    )}
  >
    <span style={ReactDOM.Style.make(~fontSize="16px", ())}>
      {AppRouter.getRouteIcon(currentRoute)->React.string}
    </span>
    <span style={ReactDOM.Style.make(~fontSize="14px", ~fontWeight="600", ~color="#e0e6ed", ())}>
      {AppRouter.getRouteLabel(currentRoute)->React.string}
    </span>
    {switch meta {
    | Some(m) =>
      <span style={ReactDOM.Style.make(~fontSize="12px", ~color="#8892a6", ~marginLeft="8px", ())}>
        {("— " ++ m.description)->React.string}
      </span>
    | None => React.null
    }}
  </div>
}
