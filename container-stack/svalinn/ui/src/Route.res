// SPDX-License-Identifier: PMPL-1.0-or-later

type t =
  | Containers
  | Images(option<string>)
  | Metrics
  | Policies
  | Verify
  | NotFound

let parser: CadreRouter.Parser.t<t> = {
  open CadreRouter.Parser
  oneOf([
    top->map(_ => Containers),
    s("containers")->map(_ => Containers),
    s("images")
      ->andThen(optional(str))
      ->map(((_, pluginId)) => Images(pluginId)),
    s("metrics")->map(_ => Metrics),
    s("policies")->map(_ => Policies),
    s("verify")->map(_ => Verify),
  ])
}

let toString = (route: t): string =>
  switch route {
  | Containers => "/containers"
  | Images(None) => "/images"
  | Images(Some(pluginId)) => "/images/" ++ pluginId
  | Metrics => "/metrics"
  | Policies => "/policies"
  | Verify => "/verify"
  | NotFound => "/not-found"
  }

let fromUrl = (url: CadreRouter.Url.t): t =>
  switch CadreRouter.Parser.parse(parser, url) {
  | Some(route) => route
  | None => NotFound
  }

module Nav = CadreRouter.Navigation.Make({
  type t = t
  let toString = toString
})

module Link = CadreRouter.Link.Make({
  type t = t
  let toString = toString
})
