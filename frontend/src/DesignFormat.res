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
let componentToJson = (comp: component): JSON.t => {
  // Convert config dict<string> to dict<Js.Json.t>
  let configJson = Dict.fromArray(
    comp.config
    ->Dict.toArray
    ->Array.map(((key, value)) => (key, JSON.Encode.string(value))),
  )

  Dict.fromArray([
    ("id", JSON.Encode.string(comp.id)),
    ("type", JSON.Encode.string(componentTypeToString(comp.componentType))),
    (
      "position",
      Dict.fromArray([
        ("x", JSON.Encode.float(comp.position.x)),
        ("y", JSON.Encode.float(comp.position.y)),
      ])->JSON.Encode.object,
    ),
    ("config", configJson->JSON.Encode.object),
  ])->JSON.Encode.object
}

// Deserialize component from JSON
let componentFromJson = (json: JSON.t): option<component> => {
  open Js.Json
  switch classify(json) {
  | JSONObject(obj) => {
      let id = Dict.get(obj, "id")->Belt.Option.flatMap(JSON.Decode.string)
      let typeStr = Dict.get(obj, "type")->Belt.Option.flatMap(JSON.Decode.string)
      let position = Dict.get(obj, "position")->Belt.Option.flatMap(posJson => {
        switch classify(posJson) {
        | JSONObject(posObj) => {
            let x = Dict.get(posObj, "x")->Belt.Option.flatMap(JSON.Decode.float)
            let y = Dict.get(posObj, "y")->Belt.Option.flatMap(JSON.Decode.float)
            switch (x, y) {
            | (Some(x), Some(y)) => Some({x, y})
            | _ => None
            }
          }
        | _ => None
        }
      })
      let config = Dict.get(obj, "config")->Belt.Option.flatMap(cfg => {
        switch classify(cfg) {
        | JSONObject(dict) => {
            // Convert dict<Js.Json.t> to dict<string>
            let stringDict = Dict.make()
            dict
            ->Dict.toArray
            ->Array.forEach(((key, value)) => {
              switch JSON.Decode.string(value) {
              | Some(str) => stringDict->Dict.set(key, str)
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
        Some({id, componentType: ct, position: pos, config: cfg})
      | _ => None
      }
    }
  | _ => None
  }
}

// Serialize connection to JSON
let connectionToJson = (conn: connection): JSON.t => {
  Dict.fromArray([
    ("id", JSON.Encode.string(conn.id)),
    ("from", JSON.Encode.string(conn.from)),
    ("to", JSON.Encode.string(conn.to)),
  ])->JSON.Encode.object
}

// Deserialize connection from JSON
let connectionFromJson = (json: JSON.t): option<connection> => {
  open Js.Json
  switch classify(json) {
  | JSONObject(obj) => {
      let id = Dict.get(obj, "id")->Belt.Option.flatMap(JSON.Decode.string)
      let from = Dict.get(obj, "from")->Belt.Option.flatMap(JSON.Decode.string)
      let to = Dict.get(obj, "to")->Belt.Option.flatMap(JSON.Decode.string)

      switch (id, from, to) {
      | (Some(id), Some(from), Some(to)) => Some({id, from, to})
      | _ => None
      }
    }
  | _ => None
  }
}

// Serialize model to JSON
let modelToJson = (model: model): JSON.t => {
  Dict.fromArray([
    ("components", model.components->Array.map(componentToJson)->JSON.Encode.array),
    ("connections", model.connections->Array.map(connectionToJson)->JSON.Encode.array),
  ])->JSON.Encode.object
}

// Deserialize model from JSON
let modelFromJson = (json: JSON.t): option<model> => {
  open Js.Json
  switch classify(json) {
  | JSONObject(obj) => {
      let components = Dict.get(obj, "components")->Belt.Option.flatMap(arr => {
        switch classify(arr) {
        | JSONArray(items) => {
            let parsed = items->Array.map(componentFromJson)->Array.keepMap(x => x)
            Some(parsed)
          }
        | _ => None
        }
      })

      let connections = Dict.get(obj, "connections")->Belt.Option.flatMap(arr => {
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
  let design = Dict.fromArray([
    ("version", JSON.Encode.string(metadata.version)),
    (
      "metadata",
      Dict.fromArray([
        ("created", JSON.Encode.string(metadata.created)),
        ("author", JSON.Encode.string(metadata.author)),
        ("description", JSON.Encode.string(metadata.description)),
      ])->JSON.Encode.object,
    ),
    ("canvas", modelToJson(model)),
  ])

  JSON.stringify(design->JSON.Encode.object)
}

// Deserialize design from JSON string
let deserializeDesign = (jsonStr: string): Result.t<(designMetadata, model), string> => {
  try {
    let json = JSON.parseOrThrow(jsonStr)
    open Js.Json

    switch classify(json) {
    | JSONObject(obj) => {
        let version = Dict.get(obj, "version")->Belt.Option.flatMap(JSON.Decode.string)

        let metadata = Dict.get(obj, "metadata")->Belt.Option.flatMap(metaJson => {
          switch classify(metaJson) {
          | JSONObject(metaObj) => {
              let created = Dict.get(metaObj, "created")->Belt.Option.flatMap(JSON.Decode.string)
              let author = Dict.get(metaObj, "author")->Belt.Option.flatMap(JSON.Decode.string)
              let description =
                Dict.get(metaObj, "description")->Belt.Option.flatMap(JSON.Decode.string)

              switch (created, author, description) {
              | (Some(created), Some(author), Some(description)) =>
                Some({
                  version: version->Belt.Option.getWithDefault("1.0"),
                  created,
                  author,
                  description,
                })
              | _ => None
              }
            }
          | _ => None
          }
        })

        let canvas = Dict.get(obj, "canvas")->Belt.Option.flatMap(modelFromJson)

        switch (metadata, canvas) {
        | (Some(meta), Some(model)) => Ok((meta, model))
        | _ => Error("Invalid design file structure")
        }
      }
    | _ => Error("Design file must be a JSON object")
    }
  } catch {
  | JsExn(e) =>
    Error("JSON parse error: " ++ JsExn.message(e)->Belt.Option.getWithDefault("Unknown error"))
  }
}
