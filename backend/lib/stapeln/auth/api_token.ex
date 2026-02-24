defmodule Stapeln.Auth.ApiToken do
  @moduledoc false

  @spec enabled?() :: boolean()
  def enabled? do
    auth_config()
    |> Keyword.get(:enabled, true)
  end

  @spec token() :: String.t() | nil
  def token do
    auth_config()
    |> Keyword.get(:token)
    |> normalize_blank()
  end

  @spec authorization_header_value() :: String.t() | nil
  def authorization_header_value do
    case token() do
      nil -> nil
      value -> "Bearer #{value}"
    end
  end

  @spec authorize_headers(map() | list()) :: :ok | {:error, atom()}
  def authorize_headers(headers) do
    if enabled?() do
      with {:ok, expected_token} <- fetch_configured_token(),
           provided_token <- extract_token_from_headers(headers),
           :ok <- verify_token(provided_token, expected_token) do
        :ok
      end
    else
      :ok
    end
  end

  @spec authorize_grpc_stream(any()) :: :ok | {:error, atom()}
  def authorize_grpc_stream(stream) do
    if enabled?() do
      with {:ok, expected_token} <- fetch_configured_token(),
           provided_token <- extract_token_from_grpc_stream(stream),
           :ok <- verify_token(provided_token, expected_token) do
        :ok
      end
    else
      :ok
    end
  end

  defp auth_config do
    Application.get_env(:stapeln, :api_auth, [])
  end

  defp fetch_configured_token do
    case token() do
      nil -> {:error, :token_not_configured}
      value -> {:ok, value}
    end
  end

  defp verify_token(nil, _expected), do: {:error, :missing_token}

  defp verify_token(provided, expected) when is_binary(provided) and is_binary(expected) do
    if byte_size(provided) == byte_size(expected) and
         Plug.Crypto.secure_compare(provided, expected) do
      :ok
    else
      {:error, :invalid_token}
    end
  end

  defp verify_token(_provided, _expected), do: {:error, :invalid_token}

  defp extract_token_from_grpc_stream(stream) do
    [
      grpc_stream_headers(stream),
      get_field(stream, :http_request_headers),
      get_field(stream, :metadata),
      get_field(stream, :headers),
      get_nested_field(stream, [:adapter, :payload, :headers]),
      get_nested_field(stream, [:adapter, :payload, :metadata]),
      stream
    ]
    |> Enum.find_value(&extract_token_from_headers/1)
  end

  defp grpc_stream_headers(stream) do
    if function_exported?(GRPC.Stream, :get_headers, 1) do
      GRPC.Stream.get_headers(stream)
    end
  rescue
    _ -> nil
  end

  defp get_nested_field(value, []), do: value

  defp get_nested_field(value, [key | rest]) do
    value
    |> get_field(key)
    |> get_nested_field(rest)
  end

  defp get_field(nil, _key), do: nil

  defp get_field(container, key) when is_map(container) do
    Map.get(container, key) || Map.get(container, Atom.to_string(key))
  end

  defp get_field(_container, _key), do: nil

  defp extract_token_from_headers(headers) when is_map(headers) or is_list(headers) do
    normalized = normalize_headers(headers)

    parse_bearer_token(Map.get(normalized, "authorization")) ||
      normalize_blank(Map.get(normalized, "x-api-key"))
  end

  defp extract_token_from_headers(_), do: nil

  defp parse_bearer_token(nil), do: nil

  defp parse_bearer_token(value) do
    value
    |> to_string()
    |> String.trim()
    |> String.split(~r/\s+/, parts: 2)
    |> case do
      [scheme, raw_token] ->
        if String.downcase(scheme) == "bearer" do
          normalize_blank(raw_token)
        else
          nil
        end

      _ ->
        nil
    end
  end

  defp normalize_headers(headers) when is_map(headers) do
    Enum.reduce(headers, %{}, fn {key, value}, acc ->
      case normalize_header_key(key) do
        nil -> acc
        header_key -> Map.put(acc, header_key, normalize_header_value(value))
      end
    end)
  end

  defp normalize_headers(headers) when is_list(headers) do
    Enum.reduce(headers, %{}, fn
      {key, value}, acc ->
        case normalize_header_key(key) do
          nil -> acc
          header_key -> Map.put(acc, header_key, normalize_header_value(value))
        end

      _other, acc ->
        acc
    end)
  end

  defp normalize_header_key(key) when is_atom(key),
    do: key |> Atom.to_string() |> String.downcase()

  defp normalize_header_key(key) when is_binary(key), do: String.downcase(key)
  defp normalize_header_key(_), do: nil

  defp normalize_header_value(value) when is_binary(value), do: value

  defp normalize_header_value(value) when is_list(value) do
    value
    |> Enum.map(&to_string/1)
    |> Enum.join(",")
  end

  defp normalize_header_value(value), do: to_string(value)

  defp normalize_blank(nil), do: nil

  defp normalize_blank(value) when is_binary(value) do
    case String.trim(value) do
      "" -> nil
      trimmed -> trimmed
    end
  end

  defp normalize_blank(value), do: value |> to_string() |> normalize_blank()
end
