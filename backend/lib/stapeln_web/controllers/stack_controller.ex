defmodule StapelnWeb.StackController do
  use StapelnWeb, :controller

  alias Stapeln.Stacks
  alias Stapeln.Crypto

  def index(conn, _params) do
    with {:ok, stacks} <- Stacks.list() do
      json(conn, %{data: Enum.map(stacks, &serialize_stack/1)})
    end
  end

  def create(conn, params) do
    with {:ok, stack} <- Stacks.create(params) do
      conn
      |> put_status(:created)
      |> json(%{data: serialize_stack(stack)})
    end
  end

  def show(conn, %{"id" => raw_id}) do
    with {:ok, id} <- parse_id(raw_id),
         {:ok, stack} <- Stacks.fetch(id) do
      json(conn, %{data: serialize_stack(stack)})
    else
      {:error, :invalid_id} -> bad_request(conn, "invalid stack id")
      {:error, :not_found} -> not_found(conn)
    end
  end

  def update(conn, %{"id" => raw_id} = params) do
    attrs = Map.delete(params, "id")

    with {:ok, id} <- parse_id(raw_id),
         {:ok, stack} <- Stacks.update(id, attrs) do
      json(conn, %{data: serialize_stack(stack)})
    else
      {:error, :invalid_id} -> bad_request(conn, "invalid stack id")
      {:error, :not_found} -> not_found(conn)
    end
  end

  def validate(conn, %{"id" => raw_id}) do
    with {:ok, id} <- parse_id(raw_id),
         {:ok, report} <- Stacks.validate(id) do
      json(conn, %{data: serialize_report(report)})
    else
      {:error, :invalid_id} -> bad_request(conn, "invalid stack id")
      {:error, :not_found} -> not_found(conn)
    end
  end

  def security_scan(conn, %{"id" => raw_id}) do
    with {:ok, id} <- parse_id(raw_id),
         {:ok, report} <- Stacks.security_scan(id) do
      json(conn, %{data: report})
    else
      {:error, :invalid_id} -> bad_request(conn, "invalid stack id")
      {:error, :not_found} -> not_found(conn)
    end
  end

  def gap_analysis(conn, %{"id" => raw_id}) do
    with {:ok, id} <- parse_id(raw_id),
         {:ok, report} <- Stacks.gap_analysis(id) do
      json(conn, %{data: report})
    else
      {:error, :invalid_id} -> bad_request(conn, "invalid stack id")
      {:error, :not_found} -> not_found(conn)
    end
  end

  def sign_stack(conn, %{"id" => raw_id}) do
    with {:ok, id} <- parse_id(raw_id),
         {:ok, stack} <- Stacks.fetch(id) do
      {public_key, secret_key} = Crypto.generate_keypair()

      {:ok, signature, _updated_secret} = Crypto.sign_stack(stack, secret_key)

      json(conn, %{
        data: %{
          stack_id: id,
          signature: Base.encode64(signature),
          public_key: Base.encode64(public_key),
          algorithm: "hybrid-ed25519-hash",
          signed_at: DateTime.utc_now() |> DateTime.truncate(:second) |> DateTime.to_iso8601()
        }
      })
    else
      {:error, :invalid_id} -> bad_request(conn, "invalid stack id")
      {:error, :not_found} -> not_found(conn)
    end
  end

  def verify_stack(conn, %{"id" => raw_id} = params) do
    signature_b64 = Map.get(params, "signature")
    public_key_b64 = Map.get(params, "public_key")

    with {:ok, id} <- parse_id(raw_id),
         {:ok, stack} <- Stacks.fetch(id),
         {:ok, signature} <- decode_base64(signature_b64, "signature"),
         {:ok, public_key} <- decode_base64(public_key_b64, "public_key") do
      valid = Crypto.verify_stack(stack, signature, public_key)

      json(conn, %{
        data: %{
          stack_id: id,
          valid: valid,
          algorithm: "hybrid-ed25519-hash",
          verified_at: DateTime.utc_now() |> DateTime.truncate(:second) |> DateTime.to_iso8601()
        }
      })
    else
      {:error, :invalid_id} -> bad_request(conn, "invalid stack id")
      {:error, :not_found} -> not_found(conn)
      {:error, :missing_param, field} -> bad_request(conn, "missing required parameter: #{field}")
      {:error, :invalid_base64, field} -> bad_request(conn, "invalid base64 in parameter: #{field}")
    end
  end

  defp decode_base64(nil, field), do: {:error, :missing_param, field}

  defp decode_base64(value, field) when is_binary(value) do
    case Base.decode64(value) do
      {:ok, decoded} -> {:ok, decoded}
      :error -> {:error, :invalid_base64, field}
    end
  end

  defp serialize_report(report) do
    %{
      score: report.score,
      findings: Enum.map(report.findings, &serialize_finding/1),
      stack: serialize_stack(report.stack)
    }
  end

  defp serialize_finding(finding) do
    %{
      id: finding.id,
      severity: Atom.to_string(finding.severity),
      message: finding.message,
      hint: finding.hint
    }
  end

  defp serialize_stack(stack) do
    %{
      id: stack.id,
      name: stack.name,
      description: stack.description,
      services: stack.services,
      created_at: DateTime.to_iso8601(stack.created_at),
      updated_at: DateTime.to_iso8601(stack.updated_at)
    }
  end

  defp parse_id(raw_id) do
    case Integer.parse(to_string(raw_id)) do
      {id, ""} when id > 0 -> {:ok, id}
      _ -> {:error, :invalid_id}
    end
  end

  defp bad_request(conn, message) do
    conn
    |> put_status(:bad_request)
    |> json(%{error: message})
  end

  defp not_found(conn) do
    conn
    |> put_status(:not_found)
    |> json(%{error: "stack not found"})
  end
end
