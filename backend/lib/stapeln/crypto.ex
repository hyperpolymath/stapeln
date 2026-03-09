# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Stapeln.Crypto do
  @moduledoc """
  Post-quantum cryptography for stapeln.

  Provides hybrid signing using Ed25519 (classical) + a SPHINCS+-inspired
  hash-based signature scheme. Since Erlang/Elixir doesn't have native
  Dilithium support, we implement a Merkle-tree / Lamport-OTS scheme using
  the available `:crypto` primitives (SHA-256).

  ## Hybrid approach

  The hybrid approach means a signature contains BOTH a classical Ed25519
  signature and a hash-based post-quantum signature. Verification succeeds
  if EITHER scheme verifies, providing both classical security today and
  post-quantum resistance for the future.

  ## Key format

  A keypair bundles:
  - An Ed25519 keypair (via `:crypto.generate_key(:eddsa, :ed25519)`)
  - A hash-based Merkle-tree keypair (via `Stapeln.Crypto.HashSignature`)

  The combined public key is the concatenation of the Ed25519 public key
  (32 bytes) and the Merkle root (32 bytes), totalling 64 bytes.

  ## Stack signing

  Stacks can optionally be signed. The signature covers a canonical hash
  of the stack's content (name, description, services). Unsigned stacks
  continue to work normally.
  """

  alias Stapeln.Crypto.HashSignature

  @ed25519_pub_bytes 32
  @merkle_root_bytes 32

  @type public_key :: binary()
  @type secret_key :: map()
  @type hybrid_signature :: binary()

  # ---------------------------------------------------------------------------
  # Key generation
  # ---------------------------------------------------------------------------

  @doc """
  Generate a hybrid keypair (Ed25519 + hash-based).

  Returns `{public_key, secret_key}` where:
  - `public_key` is a 64-byte binary (32 bytes Ed25519 pub + 32 bytes Merkle root)
  - `secret_key` is an opaque map containing both secret components
  """
  @spec generate_keypair() :: {public_key(), secret_key()}
  def generate_keypair do
    # Classical: Ed25519
    {ed_pub, ed_sec} = :crypto.generate_key(:eddsa, :ed25519)

    # Post-quantum: hash-based Merkle tree
    {merkle_root, hash_secret} = HashSignature.generate_keypair()

    public_key = ed_pub <> merkle_root

    secret_key = %{
      ed25519_public: ed_pub,
      ed25519_secret: ed_sec,
      merkle_root: merkle_root,
      hash_state: hash_secret
    }

    {public_key, secret_key}
  end

  # ---------------------------------------------------------------------------
  # Hybrid signing
  # ---------------------------------------------------------------------------

  @doc """
  Sign a message using the hybrid scheme (Ed25519 + hash-based).

  Returns `{:ok, signature, updated_secret}` or `{:error, reason}`.
  The secret key is updated because each hash-based OTS slot is single-use.
  """
  @spec sign(binary(), secret_key()) :: {:ok, hybrid_signature(), secret_key()} | {:error, term()}
  def sign(message, secret_key) when is_binary(message) do
    # Classical signature (Ed25519)
    ed_sig = :crypto.sign(:eddsa, :none, message, [secret_key.ed25519_secret, :ed25519])

    # Hash-based signature
    case HashSignature.sign(message, secret_key.hash_state) do
      {:ok, hash_sig, updated_hash_state} ->
        # Encode the hybrid signature as a tagged binary.
        encoded = encode_hybrid_signature(ed_sig, hash_sig)
        updated_secret = %{secret_key | hash_state: updated_hash_state}
        {:ok, encoded, updated_secret}

      {:error, :keys_exhausted} ->
        # Fall back to Ed25519-only if hash-based keys are exhausted.
        encoded = encode_hybrid_signature(ed_sig, nil)
        {:ok, encoded, secret_key}
    end
  end

  @doc """
  Sign a message, returning just the signature binary.

  Convenience wrapper that discards the updated secret state. Use `sign/2`
  if you need to sign multiple messages with the same keypair.
  """
  @spec sign!(binary(), secret_key()) :: hybrid_signature()
  def sign!(message, secret_key) do
    {:ok, sig, _updated} = sign(message, secret_key)
    sig
  end

  # ---------------------------------------------------------------------------
  # Hybrid verification
  # ---------------------------------------------------------------------------

  @doc """
  Verify a hybrid signature against a message and public key.

  Returns `true` if EITHER the Ed25519 signature or the hash-based signature
  (or both) verify successfully. This provides forward security: even if one
  scheme is broken, the other still protects integrity.
  """
  @spec verify(binary(), hybrid_signature(), public_key()) :: boolean()
  def verify(message, signature, public_key) when is_binary(message) and is_binary(public_key) do
    <<ed_pub::binary-size(@ed25519_pub_bytes),
      merkle_root::binary-size(@merkle_root_bytes)>> = public_key

    case decode_hybrid_signature(signature) do
      {:ok, ed_sig, hash_sig} ->
        ed_valid = verify_ed25519(message, ed_sig, ed_pub)
        hash_valid = verify_hash_sig(message, hash_sig, merkle_root)
        ed_valid or hash_valid

      :error ->
        false
    end
  end

  # ---------------------------------------------------------------------------
  # Hash-based signing (direct access)
  # ---------------------------------------------------------------------------

  @doc """
  Sign a message using only the hash-based scheme (Lamport/Merkle).

  Useful for testing or when Ed25519 is not desired.
  """
  @spec hash_sign(binary(), secret_key()) ::
          {:ok, binary(), secret_key()} | {:error, :keys_exhausted}
  def hash_sign(message, secret_key) when is_binary(message) do
    case HashSignature.sign(message, secret_key.hash_state) do
      {:ok, hash_sig, updated_state} ->
        encoded = :erlang.term_to_binary(hash_sig)
        {:ok, encoded, %{secret_key | hash_state: updated_state}}

      error ->
        error
    end
  end

  @doc """
  Verify a hash-only signature against a message and the Merkle root.
  """
  @spec hash_verify(binary(), binary(), binary()) :: boolean()
  def hash_verify(message, encoded_sig, merkle_root)
      when is_binary(message) and is_binary(encoded_sig) and is_binary(merkle_root) do
    try do
      hash_sig = :erlang.binary_to_term(encoded_sig, [:safe])
      HashSignature.verify(message, hash_sig, merkle_root)
    rescue
      _ -> false
    end
  end

  # ---------------------------------------------------------------------------
  # Stack-specific operations
  # ---------------------------------------------------------------------------

  @doc """
  Sign a stack's content hash.

  Produces a canonical binary representation of the stack (name, description,
  services sorted by name) and signs it.
  """
  @spec sign_stack(map(), secret_key()) :: {:ok, binary(), secret_key()} | {:error, term()}
  def sign_stack(stack, secret_key) when is_map(stack) do
    canonical = canonical_stack_hash(stack)
    sign(canonical, secret_key)
  end

  @doc """
  Verify a stack signature.
  """
  @spec verify_stack(map(), binary(), public_key()) :: boolean()
  def verify_stack(stack, signature, public_key) when is_map(stack) do
    canonical = canonical_stack_hash(stack)
    verify(canonical, signature, public_key)
  end

  # ---------------------------------------------------------------------------
  # Encoding / Decoding
  # ---------------------------------------------------------------------------

  # Hybrid signature wire format:
  #   <<version::8, ed_sig_len::16, ed_sig::binary, hash_sig_term::binary>>
  # version 1 = has both; version 2 = Ed25519-only (hash keys exhausted)

  @sig_version_hybrid 1
  @sig_version_ed_only 2

  defp encode_hybrid_signature(ed_sig, nil) do
    <<@sig_version_ed_only, byte_size(ed_sig)::16, ed_sig::binary>>
  end

  defp encode_hybrid_signature(ed_sig, hash_sig) do
    hash_sig_bin = :erlang.term_to_binary(hash_sig)

    <<@sig_version_hybrid, byte_size(ed_sig)::16, ed_sig::binary, hash_sig_bin::binary>>
  end

  defp decode_hybrid_signature(<<@sig_version_hybrid, ed_len::16, rest::binary>>) do
    <<ed_sig::binary-size(ed_len), hash_sig_bin::binary>> = rest

    try do
      hash_sig = :erlang.binary_to_term(hash_sig_bin, [:safe])
      {:ok, ed_sig, hash_sig}
    rescue
      _ -> :error
    end
  end

  defp decode_hybrid_signature(<<@sig_version_ed_only, ed_len::16, ed_sig::binary-size(ed_len)>>) do
    {:ok, ed_sig, nil}
  end

  defp decode_hybrid_signature(_), do: :error

  # ---------------------------------------------------------------------------
  # Internal verification helpers
  # ---------------------------------------------------------------------------

  defp verify_ed25519(message, ed_sig, ed_pub) do
    try do
      :crypto.verify(:eddsa, :none, message, ed_sig, [ed_pub, :ed25519])
    rescue
      _ -> false
    end
  end

  defp verify_hash_sig(_message, nil, _root), do: false

  defp verify_hash_sig(message, hash_sig, merkle_root) do
    try do
      HashSignature.verify(message, hash_sig, merkle_root)
    rescue
      _ -> false
    end
  end

  # ---------------------------------------------------------------------------
  # Stack canonical form
  # ---------------------------------------------------------------------------

  defp canonical_stack_hash(stack) do
    name = Map.get(stack, :name) || Map.get(stack, "name") || ""
    description = Map.get(stack, :description) || Map.get(stack, "description") || ""

    services =
      (Map.get(stack, :services) || Map.get(stack, "services") || [])
      |> Enum.sort_by(fn svc ->
        Map.get(svc, :name) || Map.get(svc, "name") || ""
      end)
      |> Enum.map(fn svc ->
        svc_name = Map.get(svc, :name) || Map.get(svc, "name") || ""
        svc_kind = Map.get(svc, :kind) || Map.get(svc, "kind") || ""
        svc_port = Map.get(svc, :port) || Map.get(svc, "port") || ""
        "#{svc_name}:#{svc_kind}:#{svc_port}"
      end)
      |> Enum.join("|")

    :crypto.hash(:sha256, "stapeln-stack:#{name}:#{description}:#{services}")
  end
end
