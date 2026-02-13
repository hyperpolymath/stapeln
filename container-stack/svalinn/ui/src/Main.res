// SPDX-License-Identifier: PMPL-1.0 OR PMPL-1.0-or-later

open Tea

// ============================================================================
// Types
// ============================================================================

type model = {
  route: Route.t,
  plugins: array<Plugins.plugin>,
  config: Api.config,
  health: option<Api.health>,
  containers: array<Api.container>,
  imagesByPlugin: Belt.Map.String.t<array<Api.image>>,
  inspect: option<Api.inspect>,
  selectedContainer: option<string>,
  inspectTab: inspectTab,
  statusFilter: option<string>,
  verificationFilter: option<bool>,
  containerSort: containerSort,
  imageSort: imageSort,
  loading: bool,
  error: option<string>,
}

type msg =
  | UrlChanged(CadreRouter.Url.t)
  | Navigate(Route.t)
  | ConfigLoaded(result<Api.config, Tea.Http.httpError>)
  | PluginsLoaded(result<Plugins.registry, Tea.Http.httpError>)
  | HealthLoaded(result<Api.health, Tea.Http.httpError>)
  | ContainersLoaded(result<array<Api.container>, Tea.Http.httpError>)
  | ImagesLoaded(string, result<array<Api.image>, Tea.Http.httpError>)
  | InspectLoaded(result<Api.inspect, Tea.Http.httpError>)
  | SelectContainer(string)
  | SetInspectTab(inspectTab)
  | SetStatusFilter(option<string>)
  | SetVerificationFilter(option<bool>)
  | SetContainerSort(containerSort)
  | SetImageSort(imageSort)
  | RefreshTick(float)

type inspectTab =
  | Overview
  | RawJson

type containerSort =
  | ContainerName
  | ContainerStatus
  | ContainerCreated

type imageSort =
  | ImageName
  | ImageVerified

// ============================================================================
// Init
// ============================================================================

let init = (): (model, Cmd.t<msg>) => {
  let route = Route.fromUrl(CadreRouter.Url.fromLocation())
  (
    {
      route,
      plugins: [||],
      config: Api.defaultConfig,
      health: None,
      containers: [||],
      imagesByPlugin: Belt.Map.String.empty,
      inspect: None,
      selectedContainer: None,
      inspectTab: Overview,
      statusFilter: None,
      verificationFilter: None,
      containerSort: ContainerName,
      imageSort: ImageName,
      loading: true,
      error: None,
    },
    Cmd.batch([
      Plugins.loadRegistry("./plugins.json", PluginsLoaded),
      Api.loadConfig("./config.json", ConfigLoaded),
    ]),
  )
}

// ============================================================================
// Update
// ============================================================================

let update = (msg: msg, model: model): (model, Cmd.t<msg>) =>
  switch msg {
  | UrlChanged(url) => {
      let route = Route.fromUrl(url)
      switch route {
      | Images(Some(pluginId)) =>
        if Belt.Map.String.get(model.imagesByPlugin, pluginId) == None {
          switch Belt.Array.getBy(model.plugins, plugin => plugin.id == pluginId) {
          | Some(plugin) =>
            ({...model, route}, Api.getImages(plugin.baseUrl, ImagesLoaded(pluginId)))
          | None => ({...model, route}, Cmd.none)
          }
        } else {
          ({...model, route}, Cmd.none)
        }
      | _ => ({...model, route}, Cmd.none)
      }
    }

  | Navigate(route) => {
      Route.Nav.pushRoute(route)
      ({...model, route}, Cmd.none)
    }

  | ConfigLoaded(result) =>
    switch result {
    | Ok(config) =>
      (
        {...model, config},
        Cmd.batch([
          Api.getHealth(config.svalinnBaseUrl, HealthLoaded),
          Api.getContainers(config.svalinnBaseUrl, ContainersLoaded),
        ]),
      )
    | Error(_) =>
      (
        model,
        Cmd.batch([
          Api.getHealth(model.config.svalinnBaseUrl, HealthLoaded),
          Api.getContainers(model.config.svalinnBaseUrl, ContainersLoaded),
        ]),
      )
    }

  | PluginsLoaded(result) =>
    switch result {
    | Ok(registry) =>
      let nextModel = {...model, plugins: registry.plugins, loading: false}
      switch nextModel.route {
      | Images(Some(pluginId)) =>
        if Belt.Map.String.get(nextModel.imagesByPlugin, pluginId) == None {
          switch Belt.Array.getBy(registry.plugins, plugin => plugin.id == pluginId) {
          | Some(plugin) => (nextModel, Api.getImages(plugin.baseUrl, ImagesLoaded(pluginId)))
          | None => (nextModel, Cmd.none)
          }
        } else {
          (nextModel, Cmd.none)
        }
      | _ => (nextModel, Cmd.none)
      }
    | Error(error) =>
      (
        {...model, error: Some(Tea.Http.errorToString(error)), loading: false},
        Cmd.none,
      )
    }

  | HealthLoaded(result) =>
    switch result {
    | Ok(health) => ({...model, health: Some(health)}, Cmd.none)
    | Error(error) => ({...model, error: Some(Tea.Http.errorToString(error))}, Cmd.none)
    }

  | ContainersLoaded(result) =>
    switch result {
    | Ok(containers) => ({...model, containers}, Cmd.none)
    | Error(error) => ({...model, error: Some(Tea.Http.errorToString(error))}, Cmd.none)
    }

  | ImagesLoaded(pluginId, result) =>
    switch result {
    | Ok(images) =>
      (
        {...model, imagesByPlugin: model.imagesByPlugin->Belt.Map.String.set(pluginId, images)},
        Cmd.none,
      )
    | Error(error) => ({...model, error: Some(Tea.Http.errorToString(error))}, Cmd.none)
    }

  | InspectLoaded(result) =>
    switch result {
    | Ok(inspect) => ({...model, inspect: Some(inspect), inspectTab: Overview}, Cmd.none)
    | Error(error) => ({...model, error: Some(Tea.Http.errorToString(error))}, Cmd.none)
    }

  | SelectContainer(id) =>
    (
      {...model, selectedContainer: Some(id)},
      Api.getInspect(model.config.svalinnBaseUrl, id, InspectLoaded),
    )

  | SetInspectTab(tab) => ({...model, inspectTab: tab}, Cmd.none)

  | SetStatusFilter(filter) => ({...model, statusFilter: filter}, Cmd.none)

  | SetVerificationFilter(filter) => ({...model, verificationFilter: filter}, Cmd.none)

  | SetContainerSort(sort) => ({...model, containerSort: sort}, Cmd.none)

  | SetImageSort(sort) => ({...model, imageSort: sort}, Cmd.none)

  | RefreshTick(_) =>
    switch model.route {
    | Containers =>
      (
        model,
        Cmd.batch([
          Api.getHealth(model.config.svalinnBaseUrl, HealthLoaded),
          Api.getContainers(model.config.svalinnBaseUrl, ContainersLoaded),
        ]),
      )
    | _ => (model, Cmd.none)
    }
  }

// ============================================================================
// Subscriptions
// ============================================================================

let subscriptions = (_model: model): Sub.t<msg> =>
  Sub.batch([|
    Sub.Sub({
      key: "route-change",
      setup: dispatch => {
        let unsubscribe = CadreRouter.Navigation.onUrlChange(url => dispatch(UrlChanged(url)))
        Some(unsubscribe)
      },
    }),
    Sub.Time.every(8000, RefreshTick),
  |])

// ============================================================================
// View Helpers
// ============================================================================

let navLink = (~route: Route.t, ~label: string, ~active: bool): React.element => {
  let className = active ? Some("active") : None
  <Route.Link route ?className>
    {React.string(label)}
  </Route.Link>
}

let statCard = (~title: string, ~value: string, ~note: string): React.element =>
  <div className="card">
    <div className="badge"> {React.string(title)} </div>
    <h3> {React.string(value)} </h3>
    <p className="muted"> {React.string(note)} </p>
  </div>

let statusBadge = (health: option<Api.health>): React.element => {
  let label = switch health {
  | Some(health) => health.status
  | None => "unknown"
  }
  <span className="badge"> {React.string(`Gateway: ${label}`)} </span>
}

let containerRow = (container: Api.container, dispatch: msg => unit): React.element =>
  <div className="card" key={container.id}>
    <h3> {React.string(container.name)} </h3>
    <p className="muted"> {React.string(container.image)} </p>
    <p className="muted"> {React.string(`Status: ${container.status}`)} </p>
    {switch container.createdAt {
    | Some(createdAt) => <p className="muted"> {React.string(`Created: ${createdAt}`)} </p>
    | None => <p className="muted"> {React.string("Created: unknown")} </p>
    }}
    <span className="badge"> {React.string(container.policyVerdict)} </span>
    <button onClick={_ => dispatch(SelectContainer(container.id))}>
      {React.string("Inspect")}
    </button>
  </div>

let getInspectField = (inspect: Api.inspect, key: string): option<string> => {
  switch Js.Json.decodeObject(inspect.data) {
  | Some(dict) =>
    switch Js.Dict.get(dict, key) {
    | Some(value) => Js.Json.decodeString(value)
    | None => None
    }
  | None => None
  }
}

let copyToClipboard = (text: string): unit => {
  let clipboard = Js.Unsafe.get(Js.Global.navigator, "clipboard")
  if Js.Unsafe.isUndefined(clipboard) {
    Js.log("clipboard unavailable")
  } else {
    let writeText = Js.Unsafe.get(clipboard, "writeText")
    if Js.Unsafe.isUndefined(writeText) {
      Js.log("writeText unavailable")
    } else {
      ignore(Js.Unsafe.call(writeText, clipboard, [|text|]))
    }
  }
}

let inspectPanel = (inspect: Api.inspect, tab: inspectTab, dispatch: msg => unit): React.element => {
  let payload = Js.Json.stringifyAny(inspect.data)->Belt.Option.getWithDefault("{}")
  let name = getInspectField(inspect, "name")->Belt.Option.getWithDefault(inspect.id)
  let image = getInspectField(inspect, "image")->Belt.Option.getWithDefault("unknown")
  let status = getInspectField(inspect, "status")->Belt.Option.getWithDefault("unknown")
  <div className="card">
    <div className="header">
      <h3> {React.string(`Inspect: ${name}`)} </h3>
      <div>
        <button onClick={_ => dispatch(SetInspectTab(Overview))}> {React.string("Overview")} </button>
        <button onClick={_ => dispatch(SetInspectTab(RawJson))}> {React.string("JSON")} </button>
        <button onClick={_ => copyToClipboard(payload)}> {React.string("Copy JSON")} </button>
      </div>
    </div>
    {switch tab {
    | Overview =>
      <div>
        <p className="muted"> {React.string(`Image: ${image}`)} </p>
        <p className="muted"> {React.string(`Status: ${status}`)} </p>
      </div>
    | RawJson => <pre> {React.string(payload)} </pre>
    }}
  </div>
}

let sortContainers = (containers: array<Api.container>, sort: containerSort): array<Api.container> =>
  switch sort {
  | ContainerName => containers->Belt.Array.sortBy(c => c.name)
  | ContainerStatus => containers->Belt.Array.sortBy(c => c.status)
  | ContainerCreated =>
    containers->Belt.Array.sortBy(c => switch c.createdAt { | Some(value) => value | None => "" })
  }

let containersView = (model: model, dispatch: msg => unit): React.element =>
  let filtered = switch model.statusFilter {
  | Some(status) =>
    model.containers->Belt.Array.keep(container => container.status == status)
  | None => model.containers
  }
  let sorted = sortContainers(filtered, model.containerSort)
  <div>
    <div className="header">
      <h1> {React.string("Containers")} </h1>
      {statusBadge(model.health)}
    </div>
    <div className="card">
      <h3> {React.string("Filter")} </h3>
      <div>
        <button onClick={_ => dispatch(SetStatusFilter(None))}> {React.string("All")} </button>
        <button onClick={_ => dispatch(SetStatusFilter(Some("running")))}> {React.string("Running")} </button>
        <button onClick={_ => dispatch(SetStatusFilter(Some("stopped")))}> {React.string("Stopped")} </button>
        <button onClick={_ => dispatch(SetStatusFilter(Some("failed")))}> {React.string("Failed")} </button>
      </div>
      <h3> {React.string("Sort")} </h3>
      <div>
        <button onClick={_ => dispatch(SetContainerSort(ContainerName))}> {React.string("Name")} </button>
        <button onClick={_ => dispatch(SetContainerSort(ContainerStatus))}> {React.string("Status")} </button>
        <button onClick={_ => dispatch(SetContainerSort(ContainerCreated))}> {React.string("Created")} </button>
      </div>
    </div>
    <div className="card-grid">
      {statCard(
        ~title="Running",
        ~value=Belt.Int.toString(sorted->Belt.Array.length),
        ~note="Current containers.",
      )}
      {statCard(~title="Verified", ~value="0", ~note="Attestation results (pending).")}
      {statCard(~title="Queued", ~value="0", ~note="Scheduler queue (pending).")}
    </div>
    <div className="card-grid">
      {sorted->Belt.Array.map(container => containerRow(container, dispatch))->React.array}
    </div>
    {switch model.inspect {
    | Some(inspect) => inspectPanel(inspect, model.inspectTab, dispatch)
    | None => <div></div>
    }}
  </div>

let imageRow = (image: Api.image): React.element =>
  <div className="card" key={image.digest}>
    <h3> {React.string(`${image.name}:${image.tag}`)} </h3>
    <p className="muted"> {React.string(image.digest)} </p>
    <span className="badge">
      {React.string(image.verified ? "verified" : "unverified")}
    </span>
  </div>

let pluginPanel = (plugin: Plugins.plugin): React.element =>
  <div className="card">
    <h3> {React.string(plugin.label)} </h3>
    <p className="muted"> {React.string("Base URL")}: {React.string(plugin.baseUrl)} </p>
    <p className="muted">
      {React.string("Capabilities")}: {React.string(plugin.capabilities->Js.Array2.joinWith(", "))}
    </p>
    <p className="muted">
      {React.string("Panels")}: {React.string(plugin.panels->Belt.Array.map(panel => panel.label)->Js.Array2.joinWith(", "))}
    </p>
  </div>

let imagesView = (
  plugins: array<Plugins.plugin>,
  imagesByPlugin: Belt.Map.String.t<array<Api.image>>,
  activeId: option<string>,
  verificationFilter: option<bool>,
  dispatch: msg => unit,
  sort: imageSort,
): React.element => {
  let active = switch activeId {
  | Some(id) => Belt.Array.getBy(plugins, plugin => plugin.id == id)
  | None => Belt.Array.get(plugins, 0)
  }

  <div>
    <div className="header">
      <h1> {React.string("Images")} </h1>
      <span className="badge"> {React.string("Plugin-backed")} </span>
    </div>
    {switch active {
    | Some(plugin) =>
      let hasImagesPanel = plugin.panels->Belt.Array.some(panel => panel.panelType == "images")
      <div>
        <div className="card-grid"> {pluginPanel(plugin)} </div>
        <div className="card">
          <h3> {React.string("Filter")} </h3>
          <div>
            <button onClick={_ => dispatch(SetVerificationFilter(None))}> {React.string("All")} </button>
            <button onClick={_ => dispatch(SetVerificationFilter(Some(true)))}> {React.string("Verified")} </button>
            <button onClick={_ => dispatch(SetVerificationFilter(Some(false)))}> {React.string("Unverified")} </button>
          </div>
          <h3> {React.string("Sort")} </h3>
          <div>
            <button onClick={_ => dispatch(SetImageSort(ImageName))}> {React.string("Name")} </button>
            <button onClick={_ => dispatch(SetImageSort(ImageVerified))}> {React.string("Verified")} </button>
          </div>
        </div>
        {hasImagesPanel ?
          <div className="card-grid">
            {switch Belt.Map.String.get(imagesByPlugin, plugin.id) {
            | Some(images) =>
              let filtered = switch verificationFilter {
              | Some(flag) => images->Belt.Array.keep(image => image.verified == flag)
              | None => images
              }
              let sorted = switch sort {
              | ImageName => filtered->Belt.Array.sortBy(image => image.name)
              | ImageVerified => filtered->Belt.Array.sortBy(image => image.verified ? "1" : "0")
              }
              sorted->Belt.Array.map(imageRow)->React.array
            | None =>
              <div className="card">
                <h3> {React.string("No images loaded")} </h3>
                <p className="muted"> {React.string("Waiting for plugin response.")} </p>
              </div>
            }}
          </div>
        : <div className="card">
            <h3> {React.string("Plugin has no images panel")} </h3>
            <p className="muted"> {React.string("Check plugins.json panels configuration.")} </p>
          </div>
        }
      </div>
    | None =>
      <div className="card">
        <h3> {React.string("No image plugins configured")} </h3>
        <p className="muted"> {React.string("Add a plugin in plugins.json.")} </p>
      </div>
    }}
  </div>
}

// ============================================================================
// View
// ============================================================================

let view = (model: model, _dispatch: msg => unit): React.element => {
  let activeRoute = model.route
  let activeImages = switch model.route {
  | Images(pluginId) => pluginId
  | _ => None
  }

  <div className="app">
    <aside className="sidebar">
      <div className="brand"> {React.string("Svalinn Console")} </div>
      <nav className="nav">
        {navLink(~route=Route.Containers, ~label="Containers", ~active=activeRoute == Route.Containers)}
        {navLink(~route=Route.Images(None), ~label="Images", ~active=switch activeRoute {
          | Images(_) => true
          | _ => false
        })}
        {model.plugins
          ->Belt.Array.map(plugin =>
            navLink(
              ~route=Route.Images(Some(plugin.id)),
              ~label=plugin.label,
              ~active=activeImages == Some(plugin.id),
            )
          )
          ->React.array}
      </nav>
    </aside>
    <main className="main">
      {switch model.error {
      | Some(message) =>
        <div className="card">
          <h3> {React.string("Plugins failed to load")} </h3>
          <p className="muted"> {React.string(message)} </p>
        </div>
      | None =>
        switch model.route {
        | Containers => containersView(model, dispatch)
        | Images(pluginId) =>
          imagesView(
            model.plugins,
            model.imagesByPlugin,
            pluginId,
            model.verificationFilter,
            dispatch,
            model.imageSort,
          )
        | NotFound =>
          <div className="card">
            <h3> {React.string("Page not found")} </h3>
            <p className="muted"> {React.string("Use the sidebar to navigate.")} </p>
          </div>
        }
      }}
    </main>
  </div>
}

// ============================================================================
// App
// ============================================================================

module App = MakeWithDispatch({
  type model = model
  type msg = msg
  type flags = unit
  let init = _ => init()
  let update = update
  let view = view
  let subscriptions = subscriptions
})

let mount = () =>
  switch ReactDOM.querySelector("#root") {
  | Some(root) => {
      let rootElement = ReactDOM.Client.createRoot(root)
      rootElement->ReactDOM.Client.Root.render(<App flags=() />)
    }
  | None => ()
  }

let () = mount()
