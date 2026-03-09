# SPDX-License-Identifier: PMPL-1.0-or-later
# token.ex - JWT token generation and verification for stapeln auth

defmodule Stapeln.Auth.Token do
  @moduledoc """
  JWT-like token generation and verification using HMAC-SHA256.

  Tokens are Base64-encoded JSON payloads signed with a secret key derived
  from the Phoenix endpoint secret_key_base. No external JWT library needed.
  """

  @token_ttl_seconds 86_400 * 7  # 7 days

  @doc "Generate a signed token for the given user identifier."
  @spec generate(String.t()) :: String.t()
  def generate(user_id) when is_binary(user_id) do
    payload = %{
      sub: user_id,
      iat: System.system_time(:second),
      exp: System.system_time(:second) + @token_ttl_seconds
    }

    encoded_payload = payload |> Jason.encode!() |> Base.url_encode64(padding: false)
    signature = sign(encoded_payload)
    encoded_payload <> "." <> signature
  end

  @doc "Verify a token and return the user_id (subject) on success."
  @spec verify(String.t()) :: {:ok, String.t()} | {:error, atom()}
  def verify(token) when is_binary(token) do
    case String.split(token, ".", parts: 2) do
      [encoded_payload, signature] ->
        if Plug.Crypto.secure_compare(sign(encoded_payload), signature) do
          decode_and_validate(encoded_payload)
        else
          {:error, :invalid_signature}
        end

      _ ->
        {:error, :malformed_token}
    end
  end

  defp decode_and_validate(encoded_payload) do
    with {:ok, json} <- Base.url_decode64(encoded_payload, padding: false),
         {:ok, %{"sub" => sub, "exp" => exp}} <- Jason.decode(json) do
      if exp > System.system_time(:second) do
        {:ok, sub}
      else
        {:error, :token_expired}
      end
    else
      _ -> {:error, :invalid_payload}
    end
  end

  defp sign(data) do
    secret = secret_key()
    :crypto.mac(:hmac, :sha256, secret, data) |> Base.url_encode64(padding: false)
  end

  defp secret_key do
    Application.get_env(:stapeln, StapelnWeb.Endpoint)[:secret_key_base]
    |> Kernel.||(raise "secret_key_base not configured")
    |> binary_part(0, 32)
  end
end
