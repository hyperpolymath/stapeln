-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SBOM.idr — Software Bill of Materials integration
--
-- This module provides types and verification for SBOM attestations
-- following SPDX and CycloneDX formats.

module SBOM

import Data.Vect
import Data.List
import Data.So
import Decidable.Equality

%default total

--------------------------------------------------------------------------------
-- SBOM Formats
--------------------------------------------------------------------------------

||| Supported SBOM formats
public export
data SBOMFormat : Type where
  SPDX      : SBOMFormat  -- ISO/IEC 5962:2021
  CycloneDX : SBOMFormat  -- OWASP CycloneDX
  SWID      : SBOMFormat  -- ISO/IEC 19770-2:2015

public export
Eq SBOMFormat where
  SPDX == SPDX = True
  CycloneDX == CycloneDX = True
  SWID == SWID = True
  _ == _ = False

public export
Show SBOMFormat where
  show SPDX = "SPDX"
  show CycloneDX = "CycloneDX"
  show SWID = "SWID"

--------------------------------------------------------------------------------
-- Package Identifier (PURL)
--------------------------------------------------------------------------------

||| Package URL (purl) specification
public export
record PackageURL where
  constructor MkPackageURL
  scheme    : String  -- "pkg"
  ptype     : String  -- package type (npm, cargo, etc.)
  pkgNamespace : Maybe String
  name      : String
  version   : Maybe String
  qualifiers : List (String, String)
  subpath   : Maybe String

||| Create a simple PURL
export
simplePurl : (ptype : String) -> (name : String) -> (version : String) -> PackageURL
simplePurl pt n v = MkPackageURL "pkg" pt Nothing n (Just v) [] Nothing

||| PURL to string
export
purlToString : PackageURL -> String
purlToString p =
  p.scheme ++ ":" ++ p.ptype ++ "/" ++
  (maybe "" (++ "/") p.pkgNamespace) ++
  p.name ++
  maybe "" ("@" ++) p.version

--------------------------------------------------------------------------------
-- Vulnerability Identifiers
--------------------------------------------------------------------------------

||| Common vulnerability identifier formats
public export
data VulnId : Type where
  CVE  : (year : Nat) -> (number : Nat) -> VulnId  -- CVE-YYYY-NNNNN
  GHSA : String -> VulnId                          -- GitHub Security Advisory
  OSV  : String -> VulnId                          -- Open Source Vulnerability
  RUSTSEC : String -> VulnId                       -- Rust Security Advisory

public export
Show VulnId where
  show (CVE year num) = "CVE-" ++ show year ++ "-" ++ show num
  show (GHSA id) = "GHSA-" ++ id
  show (OSV id) = "OSV-" ++ id
  show (RUSTSEC id) = "RUSTSEC-" ++ id

--------------------------------------------------------------------------------
-- Vulnerability Severity
--------------------------------------------------------------------------------

||| CVSS v3.1 severity levels
public export
data Severity : Type where
  None     : Severity
  Low      : Severity
  Medium   : Severity
  High     : Severity
  Critical : Severity

public export
Eq Severity where
  None == None = True
  Low == Low = True
  Medium == Medium = True
  High == High = True
  Critical == Critical = True
  _ == _ = False

public export
Ord Severity where
  compare None None = EQ
  compare None _ = LT
  compare Low None = GT
  compare Low Low = EQ
  compare Low _ = LT
  compare Medium None = GT
  compare Medium Low = GT
  compare Medium Medium = EQ
  compare Medium _ = LT
  compare High Critical = LT
  compare High High = EQ
  compare High _ = GT
  compare Critical Critical = EQ
  compare Critical _ = GT

||| CVSS score to severity
export
cvssToSeverity : Double -> Severity
cvssToSeverity score =
  if score == 0.0 then None
  else if score < 4.0 then Low
  else if score < 7.0 then Medium
  else if score < 9.0 then High
  else Critical

--------------------------------------------------------------------------------
-- Package Dependency
--------------------------------------------------------------------------------

||| License identifier (SPDX format)
public export
record License where
  constructor MkLicense
  spdxId : String
  name   : Maybe String

||| A package dependency
public export
record Dependency where
  constructor MkDependency
  purl    : PackageURL
  license : Maybe License
  vulns   : List VulnId

||| Check if a dependency has vulnerabilities
export
hasVulns : Dependency -> Bool
hasVulns d = not (null d.vulns)

||| Get maximum severity of a dependency's vulnerabilities
||| Returns None if no vulnerabilities
export
maxSeverity : Dependency -> (lookup : VulnId -> Maybe Severity) -> Severity
maxSeverity d lookup =
  foldl (\acc, v => maybe acc (max acc) (lookup v)) None d.vulns

--------------------------------------------------------------------------------
-- SBOM Document
--------------------------------------------------------------------------------

||| An SBOM document with components
public export
record SBOMDocument where
  constructor MkSBOM
  format       : SBOMFormat
  specVersion  : String
  name         : String
  version      : String
  dependencies : List Dependency

||| Count total vulnerabilities in SBOM
export
totalVulns : SBOMDocument -> Nat
totalVulns doc = sum (map (length . vulns) doc.dependencies)

||| Filter dependencies by minimum severity
export
filterBySeverity : SBOMDocument -> Severity -> (lookup : VulnId -> Maybe Severity) -> List Dependency
filterBySeverity doc minSev lookup =
  filter (\d => maxSeverity d lookup >= minSev) doc.dependencies

--------------------------------------------------------------------------------
-- Verification Predicates
--------------------------------------------------------------------------------

||| Predicate: SBOM has no critical vulnerabilities
public export
data NoCriticalVulns : SBOMDocument -> Type where
  MkNoCriticalVulns : (doc : SBOMDocument) ->
                      (lookup : VulnId -> Maybe Severity) ->
                      (prf : filterBySeverity doc Critical lookup === []) ->
                      NoCriticalVulns doc

||| Predicate: SBOM has no high-or-above vulnerabilities
public export
data NoHighVulns : SBOMDocument -> Type where
  MkNoHighVulns : (doc : SBOMDocument) ->
                  (lookup : VulnId -> Maybe Severity) ->
                  (prf : filterBySeverity doc High lookup === []) ->
                  NoHighVulns doc

||| Predicate: All dependencies have known licenses
public export
data AllLicensed : SBOMDocument -> Type where
  MkAllLicensed : (doc : SBOMDocument) ->
                  (prf : all (\d => isJust d.license) doc.dependencies === True) ->
                  AllLicensed doc

--------------------------------------------------------------------------------
-- Verification Functions
--------------------------------------------------------------------------------

||| Verify SBOM has no critical vulnerabilities
-- PROOF_TODO: Replace believe_me with actual proof
||| Note: Uses believe_me for proof obligation - TODO: implement proper DecEq for Dependency
export
verifySBOM : (doc : SBOMDocument) ->
             (lookup : VulnId -> Maybe Severity) ->
             Either String (NoCriticalVulns doc)
verifySBOM doc lookup =
  let critVulns = filterBySeverity doc Critical lookup in
  case critVulns of
-- PROOF_TODO: Replace believe_me with actual proof
    [] => Right (MkNoCriticalVulns doc lookup (believe_me ()))
    _ => Left $ "Found critical vulnerabilities: " ++ show (length critVulns)

||| Check if SBOM is acceptable for deployment
export
isDeployable : SBOMDocument -> (lookup : VulnId -> Maybe Severity) -> Bool
isDeployable doc lookup =
  case filterBySeverity doc Critical lookup of
    [] => True
    _  => False

--------------------------------------------------------------------------------
-- SBOM Attestation
--------------------------------------------------------------------------------

||| Signed SBOM attestation
public export
record SBOMAttestation where
  constructor MkSBOMAttestation
  sbom      : SBOMDocument
  signature : String
  signerKey : String
  timestamp : Nat
  verified  : Bool

||| Create attestation from verified SBOM
export
attestSBOM : SBOMDocument -> (sig : String) -> (key : String) -> (ts : Nat) -> SBOMAttestation
attestSBOM doc sig key ts = MkSBOMAttestation doc sig key ts False

||| Mark attestation as verified
export
verifyAttestation : SBOMAttestation -> SBOMAttestation
verifyAttestation att = { verified := True } att
