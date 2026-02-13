// SPDX-License-Identifier: PMPL-1.0 OR PMPL-1.0-or-later

open Tea

// Types

type plugin = {
  id: string,
  label: string,
  baseUrl: string,
  capabilities: array<string>,
  panels: array<panel>,
}

type registry = {
  plugins: array<plugin>,
}

type panel = {
  id: string,
  label: string,
  panelType: string,
}

// Decoders

let panelDecoder: Json.decoder<panel> =
  Json.map3(
    (id, label, panelType) => {id, label, panelType},
    Json.field("id", Json.string),
    Json.field("label", Json.string),
    Json.field("type", Json.string),
  )

let pluginDecoder: Json.decoder<plugin> =
  Json.map5(
    (id, label, baseUrl, capabilities, panels) => {
      id,
      label,
      baseUrl,
      capabilities,
      panels,
    },
    Json.field("id", Json.string),
    Json.field("label", Json.string),
    Json.field("baseUrl", Json.string),
    Json.field("capabilities", Json.array(Json.string)),
    Json.field("panels", Json.array(panelDecoder)),
  )

let registryDecoder: Json.decoder<registry> =
  Json.map(
    plugins => {plugins},
    Json.field("plugins", Json.array(pluginDecoder)),
  )

let loadRegistry = (url: string, toMsg: result<registry, Tea.Http.httpError> => 'msg) =>
  Tea.Http.getJson(url, registryDecoder, toMsg)
