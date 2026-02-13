# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Vordr.ReversibilityTest do
  use ExUnit.Case, async: true

  alias Vordr.Containers.Container
  alias Vordr.Reversibility
  alias Vordr.Reversibility.Journal

  setup do
    {:ok, pid} = Container.start_link("test-image:latest", name: "rev-test-#{System.unique_integer()}")
    {_, data} = Container.get_state(pid)
    %{pid: pid, container_id: data.id}
  end

  describe "journal" do
    test "logs transitions", %{pid: pid, container_id: id} do
      :ok = Container.create(pid)
      :ok = Container.start(pid)

      history = Journal.get_history(id)

      # Should have: nil → image_only, image_only → created, created → running
      assert length(history) >= 2
    end

    test "tracks reversibility", %{pid: pid, container_id: id} do
      :ok = Container.create(pid)
      :ok = Container.start(pid)

      # running can be reversed to stopped
      assert Journal.can_reverse?(id)
    end
  end

  describe "rollback" do
    test "can rollback running → created (via stop)", %{pid: pid, container_id: id} do
      :ok = Container.create(pid)
      :ok = Container.start(pid)

      # Preview shows what rollback would do
      assert Reversibility.can_rollback?(id)
    end

    test "preview_rollback returns expected operation", %{pid: pid, container_id: id} do
      :ok = Container.create(pid)
      :ok = Container.start(pid)

      {:ok, operation} = Reversibility.preview_rollback(id)
      # From running, reverse would go back to created (via start's reverse)
      assert operation in [:start, :create]
    end
  end

  describe "execute with rollback" do
    test "execute logs to journal", %{container_id: id} do
      {:ok, :created} = Reversibility.execute(id, :create)

      history = Journal.get_history(id)
      assert length(history) >= 1
    end
  end
end
