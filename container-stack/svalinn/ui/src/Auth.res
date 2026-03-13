// SPDX-License-Identifier: PMPL-1.0-or-later
// Authentication UI for Svalinn Web UI
//
// Stores API key in memory only (not localStorage) for security.
// Provides login form and logout functionality.

// ============================================================================
// Types
// ============================================================================

type authState =
  | LoggedOut
  | LoggedIn(string)

type loginForm = {
  apiKey: string,
  error: option<string>,
  submitting: bool,
}

let emptyForm: loginForm = {
  apiKey: "",
  error: None,
  submitting: false,
}

// ============================================================================
// Auth Header Helper
// ============================================================================

let authorizationHeader = (authState: authState): option<(string, string)> =>
  switch authState {
  | LoggedOut => None
  | LoggedIn(token) => Some(("Authorization", "Bearer " ++ token))
  }

let isAuthenticated = (authState: authState): bool =>
  switch authState {
  | LoggedOut => false
  | LoggedIn(_) => true
  }

// ============================================================================
// View
// ============================================================================

type authMsg =
  | SetApiKey(string)
  | SubmitLogin
  | Logout
  | LoginError(string)

let loginView = (form: loginForm, dispatch: authMsg => unit): React.element =>
  <div>
    <div className="header">
      <h1> {React.string("Authentication")} </h1>
    </div>
    <div className="auth-form">
      <div className="card">
        <h3> {React.string("API Key Login")} </h3>
        <p className="muted">
          {React.string("Enter your Svalinn API key to access protected endpoints.")}
        </p>
        {switch form.error {
        | Some(message) =>
          <div className="error-banner">
            <strong> {React.string("Login failed: ")} </strong>
            {React.string(message)}
          </div>
        | None => React.null
        }}
        <div className="form-group">
          <label htmlFor="api-key"> {React.string("API Key")} </label>
          <input
            id="api-key"
            type_="password"
            className="form-input"
            placeholder="svl_..."
            value={form.apiKey}
            onChange={event => {
              let value = ReactEvent.Form.target(event)["value"]
              dispatch(SetApiKey(value))
            }}
            onKeyDown={event => {
              if ReactEvent.Keyboard.key(event) == "Enter" {
                dispatch(SubmitLogin)
              }
            }}
            disabled={form.submitting}
          />
        </div>
        <button
          className="btn-primary"
          onClick={_ => dispatch(SubmitLogin)}
          disabled={form.submitting || Js.String2.trim(form.apiKey) == ""}>
          {form.submitting ? React.string("Authenticating...") : React.string("Login")}
        </button>
      </div>
      <div className="card">
        <h3> {React.string("Security Notice")} </h3>
        <p className="muted">
          {React.string(
            "Your API key is stored in memory only and will be cleared when you close " ++
            "the browser tab. It is never written to localStorage or cookies.",
          )}
        </p>
      </div>
    </div>
  </div>

let logoutButton = (dispatch: authMsg => unit): React.element =>
  <button className="nav-logout" onClick={_ => dispatch(Logout)}>
    {React.string("Logout")}
  </button>
