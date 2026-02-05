// SPDX-License-Identifier: PMPL-1.0-or-later
// IdrisBadge.res - "Idris inside" badge for formally verified components

// Badge variants
type badgeStyle =
  | Compact     // Small inline badge
  | Standard    // Medium standalone badge
  | Detailed    // Large badge with proof details

type proofType =
  | MemorySafety
  | ThreadSafety
  | TypeCorrectness
  | NonEmpty
  | ElementExists

// Helper: Get proof emoji
let proofEmoji = (proof: proofType): string => {
  switch proof {
  | MemorySafety => "🛡️"
  | ThreadSafety => "🔒"
  | TypeCorrectness => "✓"
  | NonEmpty => "📝"
  | ElementExists => "🔍"
  }
}

// Helper: Get proof label
let proofLabel = (proof: proofType): string => {
  switch proof {
  | MemorySafety => "Memory Safe"
  | ThreadSafety => "Thread Safe"
  | TypeCorrectness => "Type Correct"
  | NonEmpty => "Non-Empty"
  | ElementExists => "Element Exists"
  }
}

// Compact badge (inline)
let viewCompactBadge = (): React.element => {
  <span
    style={ReactDOM.Style.make(
      ~display="inline-flex",
      ~alignItems="center",
      ~gap="4px",
      ~padding="2px 8px",
      ~background="linear-gradient(135deg, #bc13fe 0%, #7209b7 100%)",
      ~color="white",
      ~borderRadius="4px",
      ~fontSize="10px",
      ~fontWeight="700",
      ~fontFamily="monospace",
      ~textTransform="uppercase",
      ~letterSpacing="0.5px",
      (),
    )}>
    {"⚡"->React.string}
    {"Idris²"->React.string}
  </span>
}

// Standard badge (standalone)
let viewStandardBadge = (): React.element => {
  <div
    style={ReactDOM.Style.make(
      ~display="inline-flex",
      ~alignItems="center",
      ~gap="8px",
      ~padding="8px 16px",
      ~background="linear-gradient(135deg, #bc13fe 0%, #7209b7 50%, #480ca8 100%)",
      ~border="2px solid #bc13fe",
      ~borderRadius="8px",
      ~boxShadow="0 4px 12px rgba(188, 19, 254, 0.3)",
      (),
    )}>
    <div
      style={ReactDOM.Style.make(
        ~fontSize="24px",
        ~lineHeight="1",
        (),
      )}>
      {"⚡"->React.string}
    </div>
    <div>
      <div
        style={ReactDOM.Style.make(
          ~fontSize="14px",
          ~fontWeight="700",
          ~color="white",
          ~fontFamily="monospace",
          ~lineHeight="1.2",
          (),
        )}>
        {"Idris² inside"->React.string}
      </div>
      <div
        style={ReactDOM.Style.make(
          ~fontSize="10px",
          ~color="rgba(255, 255, 255, 0.8)",
          ~fontWeight="600",
          ~marginTop="2px",
          (),
        )}>
        {"Formally Verified"->React.string}
      </div>
    </div>
  </div>
}

// Detailed badge (with proof list)
let viewDetailedBadge = (proofs: array<proofType>): React.element => {
  <div
    style={ReactDOM.Style.make(
      ~padding="20px",
      ~background="linear-gradient(135deg, #1e0836 0%, #2d0a4e 100%)",
      ~border="2px solid #bc13fe",
      ~borderRadius="12px",
      ~boxShadow="0 8px 24px rgba(188, 19, 254, 0.4)",
      (),
    )}>
    <div
      style={ReactDOM.Style.make(
        ~display="flex",
        ~alignItems="center",
        ~gap="12px",
        ~marginBottom="16px",
        (),
      )}>
      <div
        style={ReactDOM.Style.make(
          ~fontSize="32px",
          ~lineHeight="1",
          (),
        )}>
        {"⚡"->React.string}
      </div>
      <div>
        <div
          style={ReactDOM.Style.make(
            ~fontSize="20px",
            ~fontWeight="700",
            ~color="white",
            ~fontFamily="monospace",
            ~background="linear-gradient(135deg, #bc13fe, #f72585)",
            ~lineHeight="1.2",
            (),
          )}>
          {"Idris² inside"->React.string}
        </div>
        <div
          style={ReactDOM.Style.make(
            ~fontSize="12px",
            ~color="rgba(255, 255, 255, 0.7)",
            ~fontWeight="600",
            ~marginTop="4px",
            (),
          )}>
          {"Dependently-typed formal verification"->React.string}
        </div>
      </div>
    </div>

    <div
      style={ReactDOM.Style.make(
        ~display="flex",
        ~flexDirection="column",
        ~gap="8px",
        (),
      )}>
      {Array.map(proofs, proof =>
        <div
          key={proofLabel(proof)}
          style={ReactDOM.Style.make(
            ~display="flex",
            ~alignItems="center",
            ~gap="8px",
            ~padding="8px 12px",
            ~background="rgba(188, 19, 254, 0.15)",
            ~border="1px solid rgba(188, 19, 254, 0.3)",
            ~borderRadius="6px",
            (),
          )}>
          <span style={ReactDOM.Style.make(~fontSize="16px", ())}>
            {proofEmoji(proof)->React.string}
          </span>
          <span
            style={ReactDOM.Style.make(
              ~fontSize="13px",
              ~fontWeight="600",
              ~color="white",
              (),
            )}>
            {proofLabel(proof)->React.string}
          </span>
          <span
            style={ReactDOM.Style.make(
              ~marginLeft="auto",
              ~fontSize="11px",
              ~fontWeight="700",
              ~color="#4ade80",
              (),
            )}>
            {"✓ PROVEN"->React.string}
          </span>
        </div>
      )->React.array}
    </div>

    <div
      style={ReactDOM.Style.make(
        ~marginTop="16px",
        ~paddingTop="16px",
        ~borderTop="1px solid rgba(188, 19, 254, 0.3)",
        ~fontSize="11px",
        ~color="rgba(255, 255, 255, 0.6)",
        ~textAlign="center",
        (),
      )}>
      {"Compile-time guarantees backed by dependent type theory"->React.string}
    </div>
  </div>
}

// Main component
@react.component
let make = (~style: badgeStyle=Standard, ~proofs: option<array<proofType>>=?) => {
  switch style {
  | Compact => viewCompactBadge()
  | Standard => viewStandardBadge()
  | Detailed =>
    let proofsToShow = switch proofs {
    | Some(p) => p
    | None => [MemorySafety, ThreadSafety, TypeCorrectness, NonEmpty, ElementExists]
    }
    viewDetailedBadge(proofsToShow)
  }
}

// Tooltip component for hovering over compact badge
// TODO: Move to separate module (IdrisBadgeTooltip.res)
// @react.component
// let makeWithTooltip = (~showTooltip: bool=false) => {
//   let (hover, setHover) = React.useState(() => false)
// 
//   <div
//     style={ReactDOM.Style.make(
//       ~position="relative",
//       ~display="inline-block",
//       (),
//     )}
//     onMouseEnter={_ => setHover(_ => true)}
//     onMouseLeave={_ => setHover(_ => false)}>
//     {viewCompactBadge()}
// 
//     {hover || showTooltip
//       ? <div
//           style={ReactDOM.Style.make(
//             ~position="absolute",
//             ~bottom="calc(100% + 8px)",
//             ~left="50%",
//             ~transform="translateX(-50%)",
//             ~zIndex="1000",
//             ~whiteSpace="nowrap",
//             (),
//           )}>
//           <div
//             style={ReactDOM.Style.make(
//               ~padding="12px 16px",
//               ~background="linear-gradient(135deg, #1e0836 0%, #2d0a4e 100%)",
//               ~border="2px solid #bc13fe",
//               ~borderRadius="8px",
//               ~boxShadow="0 8px 24px rgba(188, 19, 254, 0.5)",
//               ~fontSize="12px",
//               ~color="white",
//               ~fontWeight="600",
//               (),
//             )}>
//             {"Formally verified with Idris² dependent types"->React.string}
//             <br />
//             {"Memory-safe • Thread-safe • Type-correct"->React.string}
//           </div>
//           {/* Tooltip arrow */}
//           <div
//             style={ReactDOM.Style.make(
//               ~position="absolute",
//               ~bottom="-8px",
//               ~left="50%",
//               ~transform="translateX(-50%)",
//               ~width="0",
//               ~height="0",
//               ~borderLeft="8px solid transparent",
//               ~borderRight="8px solid transparent",
//               ~borderTop="8px solid #bc13fe",
//               (),
//             )}
//           />
//         </div>
//       : React.null}
//   </div>
// }
