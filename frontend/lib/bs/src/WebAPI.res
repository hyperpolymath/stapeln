// SPDX-License-Identifier: PMPL-1.0-or-later
// WebAPI.res - Minimal Web API bindings for Deno

// Blob API
type blob
type blobOptions = {"type": string}

@new external makeBlob: (array<string>, blobOptions) => blob = "Blob"

// URL API
@val external createObjectURL: blob => string = "URL.createObjectURL"
@val external revokeObjectURL: string => unit = "URL.revokeObjectURL"

// File API
type file

@send external fileName: file => string = "name"
@send external fileSize: file => int = "size"

// FileReader API
type fileReader

@new external makeFileReader: unit => fileReader = "FileReader"
@send external readAsText: (fileReader, file) => unit = "readAsText"
@set external setOnLoad: (fileReader, unit => unit) => unit = "onload"
@get external getResult: fileReader => string = "result"

// FileList API
type fileList
@send external item: (fileList, int) => option<file> = "item"
@get external length: fileList => int = "length"

// DOM Elements
type element
type htmlElement
type htmlInputElement

@val @scope("document") external createElement: string => element = "createElement"
@send external setAttribute: (element, string, string) => unit = "setAttribute"
@send external click: htmlElement => unit = "click"
@send external addEventListener: (htmlElement, string, 'a => unit) => unit = "addEventListener"

@send external asHtmlElement: element => option<htmlElement> = "asHtmlElement"
@get external getFiles: htmlInputElement => option<fileList> = "files"

// Casts
external elementToHtml: element => option<htmlElement> = "%identity"
external htmlToInput: htmlElement => option<htmlInputElement> = "%identity"
