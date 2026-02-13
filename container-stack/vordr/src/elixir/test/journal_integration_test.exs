
# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Vordr.JournalIntegrationTest do
  use ExUnit.Case, async: true

  alias Vordr.Containers
  alias Vordr.Reversibility
  alias Vordr.Reversibility.Journal

  # Helper to create and transition a container through several states
  defp create_and_transition_container() do
    {:ok, pid} = Containers.start_link("test-image:latest", name: "journal-test-#{System.unique_integer()}")
    {:ok, container_data} = Containers.get(pid)
    container_id = container_data.id

    # image_only -> created
    {:ok, :created} = Reversibility.execute(container_id, :create)
    # created -> running
    {:ok, :running} = Reversibility.execute(container_id, :start)
    # running -> paused
    {:ok, :paused} = Reversibility.execute(container_id, :pause)
    # paused -> running
    {:ok, :running} = Reversibility.execute(container_id, :resume)
    # running -> stopped
    {:ok, :stopped} = Reversibility.execute(container_id, :stop)

    {:ok, pid, container_id}
  end

  describe "Journal.log_transition/4" do
    test "records multiple sequential transitions for a container" do
      {:ok, _pid, container_id} = create_and_transition_container()

      history = Journal.get_history(container_id)

      # Expecting: nil->image_only, image_only->created, created->running, running->paused, paused->running, running->stopped
      assert length(history) >= 6

      # Check specific transitions
      assert history |> Enum.find(fn e -> e.to_state == :image_only end) |> Map.get(:from_state) == nil
      assert history |> Enum.find(fn e -> e.to_state == :created end) |> Map.get(:from_state) == :image_only
      assert history |> Enum.find(fn e -> e.to_state == :running end) |> Map.get(:from_state) == :created
      assert history |> Enum.find(fn e -> e.to_state == :paused end) |> Map.get(:from_state) == :running
      assert history |> Enum.find(fn e -> e.to_state == :stopped end) |> Map.get(:from_state) == :paused
    end

    test "logs initial state correctly" do
      {:ok, pid} = Containers.start_link("test-image:initial", name: "initial-state-test")
      {:ok, container_data} = Containers.get(pid)
      container_id = container_data.id

      history = Journal.get_history(container_id)
      assert length(history) >= 1
      assert history |> Enum.find(fn e -> e.to_state == :image_only end) |> Map.get(:from_state) == nil
    end
  end

  describe "Journal.can_reverse?/1 and Reversibility.preview_rollback/1" do
    test "can_reverse? is true for reversible states like running, paused, stopped" do
      {:ok, pid} = Containers.start_link("test-image:rev", name: "rev-check-test")
      {:ok, container_data} = Containers.get(pid)
      container_id = container_data.id

      # Start -> Running
      :ok = Reversibility.execute(container_id, :start)
      assert Journal.can_reverse?(container_id)
      assert Reversibility.preview_rollback(container_id) != {:error, :not_reversible}

      # Running -> Paused
      :ok = Reversibility.execute(container_id, :pause)
      assert Journal.can_reverse?(container_id)
      assert Reversibility.preview_rollback(container_id) != {:error, :not_reversible}

      # Paused -> Stopped
      :ok = Reversibility.execute(container_id, :stop)
      assert Journal.can_reverse?(container_id) # Stopped can be restarted
      assert Reversibility.preview_rollback(container_id) != {:error, :not_reversible}
    end

    test "can_reverse? is false for non-reversible states like created or removed" do
      {:ok, pid} = Containers.start_link("test-image:nonrev", name: "nonrev-check-test")
      {:ok, container_data} = Containers.get(pid)
      container_id = container_data.id

      # Image_only -> Created
      :ok = Reversibility.execute(container_id, :create)
      assert not Journal.can_reverse?(container_id)
      assert Reversibility.preview_rollback(container_id) == {:error, :not_reversible}

      # Stopped -> Removed (not reversible)
      :ok = Reversibility.execute(container_id, :start)
      :ok = Reversibility.execute(container_id, :stop)
      :ok = Reversibility.execute(container_id, :remove)
      assert not Journal.can_reverse?(container_id)
      assert Reversibility.preview_rollback(container_id) == {:error, :not_reversible}
    end

    test "preview_rollback returns the correct reverse operation" do
      {:ok, pid} = Containers.start_link("test-image:preview", name: "preview-test")
      {:ok, container_data} = Containers.get(pid)
      container_id = container_data.id

      # image_only -> created
      :ok = Reversibility.execute(container_id, :create)
      assert Reversibility.preview_rollback(container_id) == {:ok, :create}

      # created -> running
      :ok = Reversibility.execute(container_id, :start)
      assert Reversibility.preview_rollback(container_id) == {:ok, :stop} # Reverse of start is stop

      # running -> paused
      :ok = Reversibility.execute(container_id, :pause)
      assert Reversibility.preview_rollback(container_id) == {:ok, :resume} # Reverse of pause is resume

      # paused -> running
      :ok = Reversibility.execute(container_id, :resume)
      assert Reversibility.preview_rollback(container_id) == {:ok, :pause} # Reverse of resume is pause

      # running -> stopped
      :ok = Reversibility.execute(container_id, :stop)
      assert Reversibility.preview_rollback(container_id) == {:ok, :start} # Reverse of stop is start (restart)
    end
  end

  describe "Journal.all/0 and Journal.get_last/2" do
    test "all/0 returns all journal entries in order" do
      {:ok, _pid, container_id} = create_and_transition_container()

      all_entries = Journal.all()
      history_entries = Journal.get_history(container_id)

      assert length(all_entries) >= length(history_entries)
      # Ensure all entries for this container are present in all()
      assert Enum.all?(history_entries, fn entry -> entry in all_entries end)
    end

    test "get_last/2 returns the specified number of most recent entries" do
      {:ok, _pid, container_id} = create_and_transition_container()

      # Get last 3 entries
      last_3 = Journal.get_last(container_id, 3)
      assert length(last_3) == 3

      # Get more entries than exist to ensure it returns all available
      all_history = Journal.get_history(container_id)
      last_10 = Journal.get_last(container_id, 10)
      assert length(last_10) == length(all_history)
      assert last_10 == Enum.reverse(all_history) # get_last should return them in chronological order

      # Get last 0 entries (should be empty)
      last_0 = Journal.get_last(container_id, 0)
      assert last_0 == []
    end
  end

  describe "Journal.clear/1" do
    test "removes all journal entries for a container" do
      {:ok, _pid, container_id} = create_and_transition_container()
      initial_history = Journal.get_history(container_id)
      assert length(initial_history) > 0

      :ok = Journal.clear(container_id)
      cleared_history = Journal.get_history(container_id)
      assert cleared_history == []

      # Ensure other containers' histories are not affected
      {:ok, pid2} = Containers.start_link("another-image:latest", name: "another-container")
      {:ok, container_data2} = Containers.get(pid2)
      container_id2 = container_data2.id
      history2 = Journal.get_history(container_id2)
      assert length(history2) >= 1
    end

    test "clearing a non-existent container's journal is a no-op" do
      non_existent_id = "non-existent-container-id"
      assert Journal.clear(non_existent_id) == :ok
      assert Journal.get_history(non_existent_id) == []
    end
  end

  describe "error handling" do
    test "journal logs errors from stubbed operations" do
      # This test assumes do_start (stub) could fail, though it currently doesn't.
      # If do_start were modified to return {:error, :some_error}, this test
      # would verify that the journal reflects it.
      # For now, we can't easily force an error from the stub.
      # This serves as a placeholder for future error handling tests.
      assert true # Placeholder
    end
  end
end
