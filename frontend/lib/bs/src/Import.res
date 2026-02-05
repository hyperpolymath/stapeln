// SPDX-License-Identifier: PMPL-1.0-or-later
// Import.res - Import designs from files

open Model
open DesignFormat

// Read file as text
let readFileAsText = (file: WebAPI.file, callback: string => unit) => {
  let reader = WebAPI.makeFileReader()

  // Set onload handler
  reader->WebAPI.setOnLoad(_ => {
    let result = reader->WebAPI.getResult
    callback(result)
  })

  // Read file
  reader->WebAPI.readAsText(file)
}

// Import design from JSON file
let importDesignFromFile = (
  file: WebAPI.file,
  onSuccess: model => unit,
  onError: string => unit,
) => {
  readFileAsText(file, jsonStr => {
    switch deserializeDesign(jsonStr) {
    | Ok((metadata, model)) => {
        Js.Console.log2("Imported design created at:", metadata.created)
        Js.Console.log2("Author:", metadata.author)
        Js.Console.log2("Description:", metadata.description)
        Js.Console.log2("Components:", Array.length(model.components))
        onSuccess(model)
      }
    | Error(err) => {
        Js.Console.error2("Import error:", err)
        onError(err)
      }
    }
  })
}

// Create file input element and trigger import
let triggerImport = (onSuccess: model => unit, onError: string => unit) => {
  let input = WebAPI.createElement("input")

  input->WebAPI.setAttribute("type", "file")
  input->WebAPI.setAttribute("accept", ".json,application/json")

  // Set onchange handler
  input->WebAPI.elementToHtml
    ->Belt.Option.forEach(htmlElement => {
      htmlElement->WebAPI.addEventListener("change", _evt => {
        // Get selected files
        htmlElement
        ->WebAPI.htmlToInput
        ->Belt.Option.flatMap(WebAPI.getFiles)
        ->Belt.Option.flatMap(fileList => fileList->WebAPI.item(0))
        ->Belt.Option.forEach(file => {
          importDesignFromFile(file, onSuccess, onError)
        })
      })
    })

  // Trigger click
  input->WebAPI.elementToHtml
    ->Belt.Option.forEach(htmlElement => {
      htmlElement->WebAPI.click
    })
}
