// SPDX-License-Identifier: PMPL-1.0-or-later
// Toast.res - Toast notification system for user feedback

type toastType =
  | Success
  | Error
  | Warning
  | Info

type toast = {
  id: string,
  message: string,
  toastType: toastType,
  duration: int, // milliseconds
  timestamp: float,
}

type state = {
  toasts: array<toast>,
  nextId: int,
}

type msg =
  | ShowToast(string, toastType, int)
  | DismissToast(string)
  | AutoDismissToast(string)

let initialState: state = {
  toasts: [],
  nextId: 0,
}

// Update function
let update = (msg: msg, state: state): state => {
  switch msg {
  | ShowToast(message, toastType, duration) =>
    let id = "toast-" ++ Int.toString(state.nextId)
    let newToast = {
      id,
      message,
      toastType,
      duration,
      timestamp: Date.now(),
    }
    {
      toasts: Array.concat(state.toasts, [newToast]),
      nextId: state.nextId + 1,
    }

  | DismissToast(id) => {
      ...state,
      toasts: Array.keep(state.toasts, t => t.id != id),
    }

  | AutoDismissToast(id) => // Same as DismissToast but triggered by timer
    {
      ...state,
      toasts: Array.keep(state.toasts, t => t.id != id),
    }
  }
}

// Helper: Get toast type color
let toastColor = (toastType: toastType): string => {
  switch toastType {
  | Success => "#4caf50"
  | Error => "#f44336"
  | Warning => "#ff9800"
  | Info => "#4a9eff"
  }
}

// Helper: Get toast type icon
let toastIcon = (toastType: toastType): string => {
  switch toastType {
  | Success => "✓"
  | Error => "✗"
  | Warning => "⚠"
  | Info => "ℹ"
  }
}

// Helper: Get toast type label
let toastLabel = (toastType: toastType): string => {
  switch toastType {
  | Success => "Success"
  | Error => "Error"
  | Warning => "Warning"
  | Info => "Info"
  }
}

// View: Single toast
let viewToast = (toast: toast, dispatch: msg => unit): React.element => {
  <div
    key={toast.id}
    className="toast"
    style={ReactDOM.Style.make(
      ~position="relative",
      ~display="flex",
      ~alignItems="center",
      ~gap="12px",
      ~padding="16px 20px",
      ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
      ~border="2px solid " ++ toastColor(toast.toastType),
      ~borderRadius="12px",
      ~boxShadow="0 8px 24px rgba(0, 0, 0, 0.4)",
      ~minWidth="300px",
      ~maxWidth="500px",
      ~marginBottom="12px",
      ~animation="slideIn 0.3s ease-out",
      (),
    )}
  >
    <div
      style={ReactDOM.Style.make(
        ~fontSize="24px",
        ~lineHeight="1",
        ~color=toastColor(toast.toastType),
        (),
      )}
    >
      {toastIcon(toast.toastType)->React.string}
    </div>

    <div style={ReactDOM.Style.make(~flex="1", ())}>
      <div
        style={ReactDOM.Style.make(
          ~fontSize="12px",
          ~fontWeight="600",
          ~color=toastColor(toast.toastType),
          ~textTransform="uppercase",
          ~letterSpacing="0.5px",
          ~marginBottom="4px",
          (),
        )}
      >
        {toastLabel(toast.toastType)->React.string}
      </div>
      <div style={ReactDOM.Style.make(~fontSize="14px", ~color="#e0e6ed", ~lineHeight="1.4", ())}>
        {toast.message->React.string}
      </div>
    </div>

    <button
      onClick={_ => dispatch(DismissToast(toast.id))}
      style={ReactDOM.Style.make(
        ~background="transparent",
        ~border="none",
        ~color="#8892a6",
        ~fontSize="20px",
        ~lineHeight="1",
        ~cursor="pointer",
        ~padding="4px",
        ~transition="color 0.2s",
        (),
      )}
    >
      {"×"->React.string}
    </button>
  </div>
}

// View: Toast container
@react.component
let make = (~toasts: array<toast>, ~dispatch: msg => unit) => {
  // Auto-dismiss toasts after their duration
  React.useEffect1(() => {
    let timeoutIds = Array.map(toasts, toast => {
      setTimeout(
        () => {
          dispatch(AutoDismissToast(toast.id))
        },
        toast.duration,
      )
    })

    Some(
      () => {
        Array.forEach(timeoutIds, clearTimeout)
      },
    )
  }, [toasts])

  <div
    className="toast-container"
    style={ReactDOM.Style.make(
      ~position="fixed",
      ~top="24px",
      ~right="24px",
      ~zIndex="10000",
      ~display="flex",
      ~flexDirection="column",
      ~alignItems="flex-end",
      (),
    )}
  >
    {Array.map(toasts, toast => viewToast(toast, dispatch))->React.array}
  </div>
}

// Helper: Create common toast messages
let success = (message: string): (string, toastType, int) => (message, Success, 3000)
let error = (message: string): (string, toastType, int) => (message, Error, 5000)
let warning = (message: string): (string, toastType, int) => (message, Warning, 4000)
let info = (message: string): (string, toastType, int) => (message, Info, 3000)
