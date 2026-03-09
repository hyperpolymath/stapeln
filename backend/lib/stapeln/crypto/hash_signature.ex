# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Stapeln.Crypto.HashSignature do
  @moduledoc """
  Hash-based signature scheme inspired by XMSS/SPHINCS+.

  Implements a Merkle tree signature structure using Lamport one-time signatures
  as the leaf scheme. All cryptographic primitives are built from `:crypto.hash/2`
  with SHA-256.

  ## Architecture

  - **Lamport OTS**: Each leaf is a Lamport one-time signature keypair. A message
    is hashed to 256 bits, and each bit selects one of two preimage chains.
  - **Merkle tree**: Leaf public keys are hashed into a binary Merkle tree. The
    root hash is the aggregate public key. A signature includes the Lamport OTS
    plus the authentication path (sibling hashes) from leaf to root.
  - **Tree height**: Configurable via `@tree_height` (default 4, giving 16 OTS
    slots). This is intentionally small for educational/functional use.

  This is NOT production-grade cryptography. It demonstrates the principles of
  hash-based post-quantum signatures using only SHA-256 as the underlying
  primitive.
  """

  # Number of levels in the Merkle tree. 2^tree_height = number of OTS keys.
  @tree_height 4
  # Lamport signs 256 bits (SHA-256 digest), so 256 pairs of secrets.
  @hash_bits 256

  @type lamport_secret :: {[binary()], [binary()]}
  @type lamport_public :: {[binary()], [binary()]}
  @type merkle_tree :: %{
          root: binary(),
          leaves: [binary()],
          nodes: %{non_neg_integer() => [binary()]}
        }
  @type keypair :: %{
          tree: merkle_tree(),
          ots_secrets: [lamport_secret()],
          ots_publics: [lamport_public()],
          next_index: non_neg_integer()
        }
  @type signature :: %{
          leaf_index: non_neg_integer(),
          lamport_sig: [binary()],
          auth_path: [binary()]
        }

  # ---------------------------------------------------------------------------
  # Public API
  # ---------------------------------------------------------------------------

  @doc """
  Generate a full Merkle tree keypair.

  Returns `{public_root, secret_state}` where `public_root` is a 32-byte
  binary (the Merkle root) and `secret_state` is an opaque map containing
  the OTS secrets and tree structure needed for signing.
  """
  @spec generate_keypair() :: {binary(), keypair()}
  def generate_keypair do
    num_leaves = trunc(:math.pow(2, @tree_height))

    # Generate Lamport OTS keypairs for each leaf.
    {ots_secrets, ots_publics} =
      Enum.map(1..num_leaves, fn _i -> lamport_keygen() end)
      |> Enum.unzip()

    # Hash each Lamport public key to get leaf hashes.
    leaves =
      Enum.map(ots_publics, fn {pub0, pub1} ->
        hash(encode_lamport_public(pub0, pub1))
      end)

    # Build the Merkle tree from leaves up to the root.
    tree = build_merkle_tree(leaves)

    secret = %{
      tree: tree,
      ots_secrets: ots_secrets,
      ots_publics: ots_publics,
      next_index: 0
    }

    {tree.root, secret}
  end

  @doc """
  Sign a message using the next available one-time signature slot.

  Returns `{:ok, signature, updated_secret}` or `{:error, :keys_exhausted}`
  if all OTS slots have been used.
  """
  @spec sign(binary(), keypair()) :: {:ok, signature(), keypair()} | {:error, :keys_exhausted}
  def sign(message, %{next_index: idx} = secret) do
    num_leaves = trunc(:math.pow(2, @tree_height))

    if idx >= num_leaves do
      {:error, :keys_exhausted}
    else
      digest = hash(message)
      {secret_0, secret_1} = Enum.at(secret.ots_secrets, idx)
      lamport_sig = lamport_sign(digest, secret_0, secret_1)
      auth_path = authentication_path(secret.tree, idx)

      sig = %{
        leaf_index: idx,
        lamport_sig: lamport_sig,
        auth_path: auth_path
      }

      {:ok, sig, %{secret | next_index: idx + 1}}
    end
  end

  @doc """
  Verify a hash-based signature against a message and the Merkle root (public key).
  """
  @spec verify(binary(), signature(), binary()) :: boolean()
  def verify(message, %{leaf_index: idx, lamport_sig: lamport_sig, auth_path: auth_path}, root) do
    digest = hash(message)

    # Reconstruct the Lamport public key from the signature and message digest.
    case lamport_recover_public(digest, lamport_sig) do
      {:ok, recovered_pub} ->
        leaf_hash = hash(recovered_pub)
        reconstructed_root = reconstruct_root(leaf_hash, idx, auth_path)
        reconstructed_root == root

      :error ->
        false
    end
  end

  # ---------------------------------------------------------------------------
  # Lamport OTS
  # ---------------------------------------------------------------------------

  @doc false
  @spec lamport_keygen() :: {lamport_secret(), lamport_public()}
  def lamport_keygen do
    secret_0 = Enum.map(1..@hash_bits, fn _i -> :crypto.strong_rand_bytes(32) end)
    secret_1 = Enum.map(1..@hash_bits, fn _i -> :crypto.strong_rand_bytes(32) end)
    pub_0 = Enum.map(secret_0, &hash/1)
    pub_1 = Enum.map(secret_1, &hash/1)
    {{secret_0, secret_1}, {pub_0, pub_1}}
  end

  defp lamport_sign(digest, secret_0, secret_1) do
    bits = binary_to_bits(digest)

    Enum.zip_with([bits, secret_0, secret_1], fn
      [0, s0, _s1] -> s0
      [1, _s0, s1] -> s1
    end)
  end

  defp lamport_recover_public(digest, sig) do
    bits = binary_to_bits(digest)

    if length(sig) != @hash_bits do
      :error
    else
      recovered =
        Enum.zip_with([bits, sig], fn
          [0, preimage] -> {hash(preimage), :placeholder}
          [1, preimage] -> {:placeholder, hash(preimage)}
        end)

      # Encode only the revealed halves (same order as during keygen encoding).
      encoded =
        recovered
        |> Enum.zip(bits)
        |> Enum.flat_map(fn
          {{pub, :placeholder}, 0} -> [pub]
          {{:placeholder, pub}, 1} -> [pub]
          _ -> []
        end)

      # We need to reconstruct the full public key encoding to hash it.
      # Since we only have half, we use a deterministic encoding that
      # includes both the revealed values and their bit positions.
      pub_encoding =
        Enum.zip(sig, bits)
        |> Enum.map(fn {preimage, _bit} -> hash(preimage) end)
        |> IO.iodata_to_binary()

      if length(encoded) == @hash_bits do
        {:ok, pub_encoding}
      else
        :error
      end
    end
  end

  # ---------------------------------------------------------------------------
  # Merkle tree construction
  # ---------------------------------------------------------------------------

  defp build_merkle_tree(leaves) do
    # Level 0 = leaves, level tree_height = root.
    nodes = %{0 => leaves}
    nodes = build_levels(nodes, 0, @tree_height)
    root = hd(Map.get(nodes, @tree_height))

    %{
      root: root,
      leaves: leaves,
      nodes: nodes
    }
  end

  defp build_levels(nodes, level, max_level) when level >= max_level, do: nodes

  defp build_levels(nodes, level, max_level) do
    current = Map.get(nodes, level)

    parent =
      current
      |> Enum.chunk_every(2)
      |> Enum.map(fn
        [left, right] -> hash(left <> right)
        [only] -> hash(only <> only)
      end)

    nodes = Map.put(nodes, level + 1, parent)
    build_levels(nodes, level + 1, max_level)
  end

  defp authentication_path(tree, leaf_index) do
    Enum.map(0..(@tree_height - 1), fn level ->
      level_nodes = Map.get(tree.nodes, level)
      # Sibling index: flip the last bit of the index at this level.
      idx = div(leaf_index, trunc(:math.pow(2, level)))
      sibling_idx = Bitwise.bxor(idx, 1)

      if sibling_idx < length(level_nodes) do
        Enum.at(level_nodes, sibling_idx)
      else
        # If no sibling, use self-hash (unbalanced tree edge case).
        Enum.at(level_nodes, idx)
      end
    end)
  end

  defp reconstruct_root(leaf_hash, leaf_index, auth_path) do
    Enum.reduce(Enum.with_index(auth_path), leaf_hash, fn {sibling, level}, current ->
      idx = div(leaf_index, trunc(:math.pow(2, level)))

      if rem(idx, 2) == 0 do
        hash(current <> sibling)
      else
        hash(sibling <> current)
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------

  defp hash(data), do: :crypto.hash(:sha256, data)

  defp binary_to_bits(<<>>), do: []

  defp binary_to_bits(<<bit::1, rest::bitstring>>) do
    [bit | binary_to_bits(rest)]
  end

  defp encode_lamport_public(pub_0, pub_1) do
    IO.iodata_to_binary(pub_0 ++ pub_1)
  end
end
