// SPDX-License-Identifier: PMPL-1.0 OR PMPL-1.0-or-later

open Tea

type config = {svalinnBaseUrl: string}

type health = {status: string}

type container = {
  id: string,
  name: string,
  image: string,
  status: string,
  policyVerdict: string,
  createdAt: option<string>,
}

type image = {
  name: string,
  tag: string,
  digest: string,
  verified: bool,
}

type inspect = {
  id: string,
  data: JSON.t,
}

let defaultConfig: config = {svalinnBaseUrl: "http://localhost:8000"}

let configDecoder: Json.decoder<config> =
  Json.map(
    svalinnBaseUrl => {svalinnBaseUrl},
    Json.field("svalinnBaseUrl", Json.string),
  )

let healthDecoder: Json.decoder<health> =
  Json.map(
    status => {status},
    Json.field("status", Json.string),
  )

let containerDecoder: Json.decoder<container> =
  Json.map6(
    (id, name, image, status, policyVerdict, createdAt) => {
      id,
      name,
      image,
      status,
      policyVerdict,
      createdAt,
    },
    Json.field("id", Json.string),
    Json.field("name", Json.string),
    Json.field("image", Json.string),
    Json.field("status", Json.string),
    Json.field("policyVerdict", Json.string),
    Json.optionalField("createdAt", Json.string),
  )

let containersDecoder: Json.decoder<array<container>> =
  Json.field("containers", Json.array(containerDecoder))

let imageDecoder: Json.decoder<image> =
  Json.map4(
    (name, tag, digest, verified) => {name, tag, digest, verified},
    Json.field("name", Json.string),
    Json.field("tag", Json.string),
    Json.field("digest", Json.string),
    Json.field("verified", Json.bool),
  )

let imagesDecoder: Json.decoder<array<image>> =
  Json.field("images", Json.array(imageDecoder))

let inspectDecoder: Json.decoder<inspect> =
  Json.map2(
    (id, data) => {id, data},
    Json.field("id", Json.string),
    Json.field("data", Json.value),
  )

let loadConfig = (url: string, toMsg: result<config, Tea.Http.httpError> => 'msg) =>
  Tea.Http.getJson(url, configDecoder, toMsg)

let getHealth = (baseUrl: string, toMsg: result<health, Tea.Http.httpError> => 'msg) =>
  Tea.Http.getJson(`${baseUrl}/healthz`, healthDecoder, toMsg)

let getContainers = (
  baseUrl: string,
  toMsg: result<array<container>, Tea.Http.httpError> => 'msg,
) => Tea.Http.getJson(`${baseUrl}/v1/containers`, containersDecoder, toMsg)

let getImages = (
  baseUrl: string,
  toMsg: result<array<image>, Tea.Http.httpError> => 'msg,
) => Tea.Http.getJson(`${baseUrl}/v1/images`, imagesDecoder, toMsg)

let getInspect = (
  baseUrl: string,
  id: string,
  toMsg: result<inspect, Tea.Http.httpError> => 'msg,
) => Tea.Http.getJson(`${baseUrl}/v1/containers/${id}/inspect`, inspectDecoder, toMsg)
