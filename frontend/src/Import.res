// SPDX-License-Identifier: PMPL-1.0-or-later
// Import.res - Import designs from files

open Model
open DesignFormat

// Read file as text
let readFileAsText = (file: Webapi.File.t, callback: string => unit) => {
  let reader = Webapi.FileReader.make()

  // Set onload handler
  reader->Webapi.FileReader.setOnload(_ => {
    reader
    ->Webapi.FileReader.result
    ->Belt.Option.forEach(result => {
      // Result should be a string
      callback(result)
    })
  })

  // Read file
  reader->Webapi.FileReader.readAsText(file)
}

// Import design from JSON file
let importDesignFromFile = (
  file: Webapi.File.t,
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
  let input = Webapi.Dom.document->Webapi.Dom.Document.createElement("input")

  input->Webapi.Dom.Element.setAttribute("type", "file")
  input->Webapi.Dom.Element.setAttribute("accept", ".json,application/json")

  // Set onchange handler
  input->Webapi.Dom.Element.asHtmlElement
    ->Belt.Option.forEach(htmlElement => {
      htmlElement->Webapi.Dom.HtmlElement.addEventListener("change", _evt => {
        // Get selected files
        htmlElement
        ->Webapi.Dom.HtmlInputElement.ofHtmlElement
        ->Belt.Option.flatMap(Webapi.Dom.HtmlInputElement.files)
        ->Belt.Option.flatMap(Webapi.FileList.item(_, 0))
        ->Belt.Option.forEach(file => {
          importDesignFromFile(file, onSuccess, onError)
        })
      })
    })

  // Trigger click
  input->Webapi.Dom.HtmlElement.ofElement
    ->Belt.Option.forEach(htmlElement => {
      htmlElement->Webapi.Dom.HtmlElement.click
    })
}
