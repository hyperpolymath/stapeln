;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - stackur's position in the verified container ecosystem

(ecosystem
 (version "1.0")
 (name "stackur")
 (type "ui-tool")
 (purpose "Visual drag-and-drop designer for creating container stacks using verified containers")

 (position-in-ecosystem
  (role "developer-tool")
  (layer "ui-orchestration")
  (description "stackur provides a graphical interface for designing multi-container stacks using Cerro Torre, Svalinn, selur, and Vörðr. It generates compose.toml files for selur-compose and validates stack configurations using formal verification."))

 (related-projects
  ((name . "selur-compose")
   (relationship . "sibling-standard")
   (description . "stackur generates compose.toml files that selur-compose consumes for deployment"))
  ((name . "cerro-torre")
   (relationship . "sibling-standard")
   (description . "stackur includes Cerro Torre as a draggable component for .ctp bundle creation"))
  ((name . "svalinn")
   (relationship . "sibling-standard")
   (description . "stackur models Svalinn as an edge gateway component in the stack"))
  ((name . "selur")
   (relationship . "sibling-standard")
   (description . "stackur represents selur as the IPC bridge layer between components"))
  ((name . "vordr")
   (relationship . "sibling-standard")
   (description . "stackur models Vörðr as the container runtime/orchestrator"))
  ((name . "verified-container-spec")
   (relationship . "protocol-specification")
   (description . "stackur validates generated stacks against verified-container-spec schemas"))
  ((name . "rescript-tea")
   (relationship . "inspiration")
   (description . "TEA architecture for predictable UI state management"))
  ((name . "cadre-tea-router")
   (relationship . "inspiration")
   (description . "Routing for ReScript-TEA applications"))
  ((name . "ephapax")
   (relationship . "sibling-standard")
   (description . "Backend uses Ephapax linear types for stack validation"))
  ((name . "affinescript")
   (relationship . "potential-consumer")
   (description . "Backend uses AffineScript for resource constraints")))

 (provides
  (capability "visual-stack-design")
  (capability "drag-and-drop-ui")
  (capability "compose-file-generation")
  (capability "formal-stack-validation")
  (interface "web-ui-http-api"))

 (consumes
  (dependency "rescript-tea")
  (dependency "cadre-tea-router")
  (dependency "deno-runtime")
  (dependency "phoenix-framework")
  (dependency "ephapax-linear")
  (dependency "idris2-proofs"))

 (integration-points
  ((component . "selur-compose")
   (method . "compose-toml-export")
   (description . "stackur exports compose.toml files that selur-compose can deploy"))
  ((component . "cerro-torre")
   (method . "ct-cli-integration")
   (description . "stackur can invoke ct commands for bundle creation"))
  ((component . "podman")
   (method . "api-client")
   (description . "stackur can deploy directly to Podman via HTTP API"))
  ((component . "docker")
   (method . "api-client")
   (description . "stackur can deploy directly to Docker via HTTP API"))
  ((component . "nerdctl")
   (method . "cli-wrapper")
   (description . "stackur can deploy via nerdctl CLI")))

 (what-this-is
  "A visual drag-and-drop web application for designing container stacks using the verified-container-spec ecosystem. Users drag components (Cerro Torre, Svalinn, selur, Vörðr, Podman/Docker/nerdctl) onto a canvas, connect them, configure them, and stackur generates compose.toml files for deployment. The backend uses Elixir/Phoenix for API, Ephapax for linear type validation, AffineScript for resource constraints, Idris2 for formal proofs, and Rust for code generation. The frontend is pure ReScript-TEA running on Deno. Named 'stackur' (Icelandic for 'stack' with designer suffix) to match the Nordic naming theme.")

 (what-this-is-not
  "Not a container runtime (use Podman/Docker/nerdctl). Not a CLI tool (use selur-compose for CLI). Not a replacement for compose files (generates them). Not a general-purpose diagram tool (specific to verified containers). Not a monitoring dashboard (focused on design/configuration). Not a CI/CD system (generates configs for deployment)."))
