// SPDX-License-Identifier: PMPL-1.0-or-later
// DesignFormat.res - JSON schema for stapeln designs

open Model

// Design file format version
let currentVersion = "1.0"

type designMetadata = {
  version: string,
  created: string,
  author: string,
  description: string,
}

type designFile = {
  metadata: designMetadata,
  canvas: model,
}

// Serialize component to JSON
let componentToJson = (comp: component): Js.Json.t => {
  // Convert config dict<string> to dict<Js.Json.t>
  let configJson = Js.Dict.fromArray(
    comp.config
    ->Js.Dict.entries
    ->Array.map(((key, value)) => (key, Js.Json.string(value)))
  )

  Js.Dict.fromArray([
    ("id", Js.Json.string(comp.id)),
    ("type", Js.Json.string(componentTypeToString(comp.componentType))),
    ("position", Js.Dict.fromArray([
      ("x", Js.Json.number(comp.position.x)),
      ("y", Js.Json.number(comp.position.y)),
    ])->Js.Json.object_),
    ("config", configJson->Js.Json.object_),
  ])->Js.Json.object_
}

// Deserialize component from JSON
let componentFromJson = (json: Js.Json.t): option<component> => {
  open Js.Json
  switch classify(json) {
  | JSONObject(obj) => {
      let id = Js.Dict.get(obj, "id")->Belt.Option.flatMap(decodeString)
      let typeStr = Js.Dict.get(obj, "type")->Belt.Option.flatMap(decodeString)
      let position = Js.Dict.get(obj, "position")->Belt.Option.flatMap(posJson => {
        switch classify(posJson) {
        | JSONObject(posObj) => {
            let x = Js.Dict.get(posObj, "x")->Belt.Option.flatMap(decodeNumber)
            let y = Js.Dict.get(posObj, "y")->Belt.Option.flatMap(decodeNumber)
            switch (x, y) {
            | (Some(x), Some(y)) => Some({x: x, y: y})
            | _ => None
            }
          }
        | _ => None
        }
      })
      let config = Js.Dict.get(obj, "config")->Belt.Option.flatMap(cfg => {
        switch classify(cfg) {
        | JSONObject(dict) => {
            // Convert dict<Js.Json.t> to dict<string>
            let stringDict = Js.Dict.empty()
            dict->Js.Dict.entries->Array.forEach(((key, value)) => {
              switch decodeString(value) {
              | Some(str) => stringDict->Js.Dict.set(key, str)
              | None => () // Skip non-string values
              }
            })
            Some(stringDict)
          }
        | _ => None
        }
      })

      // Convert type string to componentType
      let componentType = switch typeStr {
      | Some("Cerro Torre") => Some(CerroTorre)
      | Some("Lago Grey") => Some(LagoGrey)
      | Some("Svalinn") => Some(Svalinn)
      | Some("selur") => Some(Selur)
      | Some("Vörðr") => Some(Vordr)
      | Some("Podman") => Some(Podman)
      | Some("Docker") => Some(Docker)
      | Some("nerdctl") => Some(Nerdctl)
      | Some("Volume") => Some(Volume)
      | Some("Network") => Some(Network)
      | _ => None
      }

      switch (id, componentType, position, config) {
      | (Some(id), Some(ct), Some(pos), Some(cfg)) =>
          Some({id: id, componentType: ct, position: pos, config: cfg})
      | _ => None
      }
    }
  | _ => None
  }
}

// Serialize connection to JSON
let connectionToJson = (conn: connection): Js.Json.t => {
  Js.Dict.fromArray([
    ("id", Js.Json.string(conn.id)),
    ("from", Js.Json.string(conn.from)),
    ("to", Js.Json.string(conn.to)),
  ])->Js.Json.object_
}

// Deserialize connection from JSON
let connectionFromJson = (json: Js.Json.t): option<connection> => {
  open Js.Json
  switch classify(json) {
  | JSONObject(obj) => {
      let id = Js.Dict.get(obj, "id")->Belt.Option.flatMap(decodeString)
      let from = Js.Dict.get(obj, "from")->Belt.Option.flatMap(decodeString)
      let to = Js.Dict.get(obj, "to")->Belt.Option.flatMap(decodeString)

      switch (id, from, to) {
      | (Some(id), Some(from), Some(to)) => Some({id: id, from: from, to: to})
      | _ => None
      }
    }
  | _ => None
  }
}

// Serialize model to JSON
let modelToJson = (model: model): Js.Json.t => {
  Js.Dict.fromArray([
    ("components", model.components->Array.map(componentToJson)->Js.Json.array),
    ("connections", model.connections->Array.map(connectionToJson)->Js.Json.array),
  ])->Js.Json.object_
}

// Deserialize model from JSON
let modelFromJson = (json: Js.Json.t): option<model> => {
  open Js.Json
  switch classify(json) {
  | JSONObject(obj) => {
      let components = Js.Dict.get(obj, "components")->Belt.Option.flatMap(arr => {
        switch classify(arr) {
        | JSONArray(items) => {
            let parsed = items->Array.map(componentFromJson)->Array.keepMap(x => x)
            Some(parsed)
          }
        | _ => None
        }
      })

      let connections = Js.Dict.get(obj, "connections")->Belt.Option.flatMap(arr => {
        switch classify(arr) {
        | JSONArray(items) => {
            let parsed = items->Array.map(connectionFromJson)->Array.keepMap(x => x)
            Some(parsed)
          }
        | _ => None
        }
      })

      switch (components, connections) {
      | (Some(comps), Some(conns)) =>
          Some({
            ...initialModel,
            components: comps,
            connections: conns,
          })
      | _ => None
      }
    }
  | _ => None
  }
}

// Serialize full design to JSON string
let serializeDesign = (model: model, metadata: designMetadata): string => {
  let design = Js.Dict.fromArray([
    ("version", Js.Json.string(metadata.version)),
    ("metadata", Js.Dict.fromArray([
      ("created", Js.Json.string(metadata.created)),
      ("author", Js.Json.string(metadata.author)),
      ("description", Js.Json.string(metadata.description)),
    ])->Js.Json.object_),
    ("canvas", modelToJson(model)),
  ])

  Js.Json.stringify(design->Js.Json.object_)
}

// Deserialize design from JSON string
let deserializeDesign = (jsonStr: string): Result.t<(designMetadata, model), string> => {
  try {
    let json = Js.Json.parseExn(jsonStr)
    open Js.Json

    switch classify(json) {
    | JSONObject(obj) => {
        let version = Js.Dict.get(obj, "version")->Belt.Option.flatMap(decodeString)

        let metadata = Js.Dict.get(obj, "metadata")->Belt.Option.flatMap(metaJson => {
          switch classify(metaJson) {
          | JSONObject(metaObj) => {
              let created = Js.Dict.get(metaObj, "created")->Belt.Option.flatMap(decodeString)
              let author = Js.Dict.get(metaObj, "author")->Belt.Option.flatMap(decodeString)
              let description = Js.Dict.get(metaObj, "description")->Belt.Option.flatMap(decodeString)

              switch (created, author, description) {
              | (Some(created), Some(author), Some(description)) =>
                  Some({version: version->Belt.Option.getWithDefault("1.0"), created, author, description})
              | _ => None
              }
            }
          | _ => None
          }
        })

        let canvas = Js.Dict.get(obj, "canvas")->Belt.Option.flatMap(modelFromJson)

        switch (metadata, canvas) {
        | (Some(meta), Some(model)) => Ok((meta, model))
        | _ => Error("Invalid design file structure")
        }
      }
    | _ => Error("Design file must be a JSON object")
    }
  } catch {
  | Js.Exn.Error(e) =>
      Error("JSON parse error: " ++ Js.Exn.message(e)->Belt.Option.getWithDefault("Unknown error"))
  }
}
