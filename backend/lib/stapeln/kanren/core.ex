# SPDX-License-Identifier: PMPL-1.0-or-later
# core.ex - miniKanren relational programming engine for stapeln
#
# A minimal but complete implementation of miniKanren with:
# - Unification (==)
# - Conjunction (conj / all)
# - Disjunction (disj / any)
# - Fresh variables (fresh)
# - Run interface (run)
# - Walk and reification

defmodule Stapeln.Kanren.Core do
  @moduledoc """
  miniKanren relational logic engine.

  Provides a pure-Elixir implementation of the core miniKanren primitives
  for deterministic security vulnerability reasoning.
  """

  # A logic variable is a tagged integer
  defstruct [:id]
  @type lvar :: %__MODULE__{id: non_neg_integer()}

  # A substitution is a map from lvar -> term
  @type subst :: %{optional(lvar()) => term()}

  # A stream is a list of substitutions (lazy via thunks)
  @type stream :: [subst()] | (-> stream())

  # A goal is a function from substitution to stream
  @type goal :: (subst() -> stream())

  @doc "Create a fresh logic variable."
  @spec lvar(non_neg_integer()) :: lvar()
  def lvar(id), do: %__MODULE__{id: id}

  @doc "Check if a term is a logic variable."
  @spec lvar?(term()) :: boolean()
  def lvar?(%__MODULE__{}), do: true
  def lvar?(_), do: false

  @doc "Walk a term through a substitution to its value."
  @spec walk(term(), subst()) :: term()
  def walk(%__MODULE__{} = v, s) do
    case Map.get(s, v) do
      nil -> v
      val -> walk(val, s)
    end
  end

  def walk(t, _s), do: t

  @doc "Deep walk — recursively resolve all variables in a term."
  @spec walk_deep(term(), subst()) :: term()
  def walk_deep(t, s) do
    case walk(t, s) do
      %__MODULE__{} = v -> v
      [h | t_rest] -> [walk_deep(h, s) | walk_deep(t_rest, s)]
      {a, b} -> {walk_deep(a, s), walk_deep(b, s)}
      other -> other
    end
  end

  @doc "Unify two terms under a substitution. Returns nil on failure."
  @spec unify(term(), term(), subst()) :: subst() | nil
  def unify(u, v, s) do
    u = walk(u, s)
    v = walk(v, s)

    cond do
      u == v ->
        s

      lvar?(u) ->
        Map.put(s, u, v)

      lvar?(v) ->
        Map.put(s, v, u)

      is_list(u) and is_list(v) and length(u) == length(v) ->
        Enum.zip(u, v)
        |> Enum.reduce_while(s, fn {a, b}, acc ->
          case unify(a, b, acc) do
            nil -> {:halt, nil}
            new_s -> {:cont, new_s}
          end
        end)

      is_tuple(u) and is_tuple(v) and tuple_size(u) == tuple_size(v) ->
        u_list = Tuple.to_list(u)
        v_list = Tuple.to_list(v)
        unify(u_list, v_list, s)

      true ->
        nil
    end
  end

  @doc "Unification goal: succeed if u and v unify."
  @spec eq(term(), term()) :: goal()
  def eq(u, v) do
    fn s ->
      case unify(u, v, s) do
        nil -> []
        new_s -> [new_s]
      end
    end
  end

  @doc "Conjunction: succeed only if both goals succeed."
  @spec conj(goal(), goal()) :: goal()
  def conj(g1, g2) do
    fn s ->
      g1.(s) |> bind(g2)
    end
  end

  @doc "Disjunction: succeed if either goal succeeds."
  @spec disj(goal(), goal()) :: goal()
  def disj(g1, g2) do
    fn s ->
      mplus(g1.(s), g2.(s))
    end
  end

  @doc "Conjunction of multiple goals."
  @spec all([goal()]) :: goal()
  def all([]), do: fn s -> [s] end
  def all([g]), do: g
  def all([g | rest]), do: conj(g, all(rest))

  @doc "Disjunction of multiple goals (conde)."
  @spec conde([[goal()]]) :: goal()
  def conde(clauses) do
    goals = Enum.map(clauses, &all/1)
    Enum.reduce(goals, fn g, acc -> disj(acc, g) end)
  end

  @doc "Fresh: introduce new logic variables."
  @spec fresh(non_neg_integer(), (... -> goal())) :: goal()
  def fresh(n, f) when is_integer(n) and n > 0 do
    fn s ->
      vars = for i <- 0..(n - 1), do: lvar(System.unique_integer([:positive]) + i)

      goal =
        case n do
          1 -> f.(hd(vars))
          2 -> f.(Enum.at(vars, 0), Enum.at(vars, 1))
          3 -> f.(Enum.at(vars, 0), Enum.at(vars, 1), Enum.at(vars, 2))
          _ -> apply(f, vars)
        end

      goal.(s)
    end
  end

  @doc "Run a goal and return up to `n` results for variable `var`."
  @spec run(non_neg_integer() | :all, goal(), lvar()) :: [term()]
  def run(n, goal, var) do
    results = goal.(%{})
    results = take_stream(results, n)
    Enum.map(results, fn s -> walk_deep(var, s) end)
  end

  @doc "Run a goal with a fresh variable."
  @spec run_fresh(non_neg_integer() | :all, (lvar() -> goal())) :: [term()]
  def run_fresh(n, f) do
    q = lvar(System.unique_integer([:positive]))
    run(n, f.(q), q)
  end

  # Stream operations

  defp bind([], _g), do: []
  defp bind(f, g) when is_function(f), do: fn -> bind(f.(), g) end
  defp bind([h | t], g), do: mplus(g.(h), bind(t, g))

  defp mplus([], s2), do: s2
  defp mplus(f, s2) when is_function(f), do: fn -> mplus(s2, f.()) end
  defp mplus([h | t], s2), do: [h | mplus(t, s2)]

  defp take_stream(_, 0), do: []
  defp take_stream([], _), do: []
  defp take_stream(f, n) when is_function(f), do: take_stream(f.(), n)

  defp take_stream([h | t], :all), do: [h | take_stream(t, :all)]

  defp take_stream([h | t], n) when is_integer(n) and n > 0 do
    [h | take_stream(t, n - 1)]
  end
end
