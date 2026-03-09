# SPDX-License-Identifier: PMPL-1.0-or-later
# security_rules.ex - miniKanren security vulnerability reasoning rules

defmodule Stapeln.Kanren.SecurityRules do
  @moduledoc """
  Relational security rules expressed as miniKanren goals.

  Each rule encodes a security property as a logical relation. The engine
  queries these rules to deterministically identify vulnerabilities,
  classify severity, and suggest remediation.
  """

  alias Stapeln.Kanren.Core

  @doc """
  Relation: a service running as root is a vulnerability.
  vuln_root_container(service, severity, message)
  """
  def vuln_root_container(service, severity, message) do
    Core.all([
      Core.eq(severity, :critical),
      Core.eq(message, "Container runs as root user"),
      Core.fresh(1, fn user ->
        Core.all([
          Core.eq({:user, service}, {:user, user}),
          Core.eq(user, "root")
        ])
      end)
    ])
  end

  @doc """
  Relation: an exposed database port is high severity.
  vuln_exposed_db(port, severity, message)
  """
  def vuln_exposed_db(port, severity, message) do
    Core.conde([
      [Core.eq(port, 5432), Core.eq(severity, :high),
       Core.eq(message, "PostgreSQL port exposed without encryption")],
      [Core.eq(port, 3306), Core.eq(severity, :high),
       Core.eq(message, "MySQL port exposed without encryption")],
      [Core.eq(port, 27017), Core.eq(severity, :high),
       Core.eq(message, "MongoDB port exposed without authentication")],
      [Core.eq(port, 6379), Core.eq(severity, :high),
       Core.eq(message, "Redis port exposed without authentication")]
    ])
  end

  @doc """
  Relation: a service without health checks is medium severity.
  vuln_no_healthcheck(service, severity, message)
  """
  def vuln_no_healthcheck(service, severity, message) do
    Core.all([
      Core.fresh(1, fn has_health ->
        Core.all([
          Core.eq({:healthcheck, service}, {:healthcheck, has_health}),
          Core.eq(has_health, false),
          Core.eq(severity, :medium),
          Core.eq(message, "Service has no health check configured")
        ])
      end)
    ])
  end

  @doc """
  Relation: a service using default credentials is critical.
  vuln_default_creds(service, severity, message)
  """
  def vuln_default_creds(service, severity, message) do
    Core.all([
      Core.eq(severity, :critical),
      Core.eq(message, "Service uses default credentials"),
      Core.fresh(1, fn creds ->
        Core.all([
          Core.eq({:credentials, service}, {:credentials, creds}),
          Core.eq(creds, :default)
        ])
      end)
    ])
  end

  @doc """
  Relation: map a severity atom to a numeric score.
  severity_score(severity, score)
  """
  def severity_score(severity, score) do
    Core.conde([
      [Core.eq(severity, :critical), Core.eq(score, 10)],
      [Core.eq(severity, :high), Core.eq(score, 8)],
      [Core.eq(severity, :medium), Core.eq(score, 5)],
      [Core.eq(severity, :low), Core.eq(score, 2)],
      [Core.eq(severity, :info), Core.eq(score, 0)]
    ])
  end

  @doc """
  Relation: map a port to a known protocol.
  port_protocol(port, protocol, risk)
  """
  def port_protocol(port, protocol, risk) do
    Core.conde([
      [Core.eq(port, 80), Core.eq(protocol, "HTTP"), Core.eq(risk, :medium)],
      [Core.eq(port, 443), Core.eq(protocol, "HTTPS"), Core.eq(risk, :low)],
      [Core.eq(port, 8080), Core.eq(protocol, "HTTP-alt"), Core.eq(risk, :medium)],
      [Core.eq(port, 22), Core.eq(protocol, "SSH"), Core.eq(risk, :low)],
      [Core.eq(port, 5432), Core.eq(protocol, "PostgreSQL"), Core.eq(risk, :high)],
      [Core.eq(port, 3306), Core.eq(protocol, "MySQL"), Core.eq(risk, :high)],
      [Core.eq(port, 6379), Core.eq(protocol, "Redis"), Core.eq(risk, :high)],
      [Core.eq(port, 27017), Core.eq(protocol, "MongoDB"), Core.eq(risk, :high)],
      [Core.eq(port, 9090), Core.eq(protocol, "Prometheus"), Core.eq(risk, :medium)],
      [Core.eq(port, 3000), Core.eq(protocol, "Dev-server"), Core.eq(risk, :medium)]
    ])
  end

  @doc """
  Query all known database port vulnerabilities.
  Returns a list of {port, severity, message} tuples.
  """
  def query_db_vulnerabilities do
    Core.run_fresh(:all, fn q ->
      Core.fresh(3, fn port, sev, msg ->
        Core.all([
          vuln_exposed_db(port, sev, msg),
          Core.eq(q, {port, sev, msg})
        ])
      end)
    end)
  end

  @doc """
  Query the protocol and risk for a specific port.
  """
  def query_port_info(port_number) do
    Core.run_fresh(:all, fn q ->
      Core.fresh(2, fn protocol, risk ->
        Core.all([
          port_protocol(port_number, protocol, risk),
          Core.eq(q, {protocol, risk})
        ])
      end)
    end)
  end

  @doc """
  Query all severity scores.
  """
  def query_severity_scores do
    Core.run_fresh(:all, fn q ->
      Core.fresh(2, fn sev, score ->
        Core.all([
          severity_score(sev, score),
          Core.eq(q, {sev, score})
        ])
      end)
    end)
  end
end
