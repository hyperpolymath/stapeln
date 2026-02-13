# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Vordr.ContainerTest do
  use ExUnit.Case, async: true

  alias Vordr.Containers.Container

  setup do
    # Start a fresh container for each test
    {:ok, pid} = Container.start_link("test-image:latest", name: "test-#{System.unique_integer()}")
    %{pid: pid}
  end

  describe "state machine" do
    test "starts in image_only state", %{pid: pid} do
      {state, _data} = Container.get_state(pid)
      assert state == :image_only
    end

    test "image_only → created transition", %{pid: pid} do
      assert :ok = Container.create(pid)
      {state, _data} = Container.get_state(pid)
      assert state == :created
    end

    test "created → running transition", %{pid: pid} do
      :ok = Container.create(pid)
      assert :ok = Container.start(pid)
      {state, data} = Container.get_state(pid)
      assert state == :running
      assert data.pid != nil
    end

    test "running → paused → running transitions", %{pid: pid} do
      :ok = Container.create(pid)
      :ok = Container.start(pid)

      assert :ok = Container.pause(pid)
      {state, _} = Container.get_state(pid)
      assert state == :paused

      assert :ok = Container.resume(pid)
      {state, _} = Container.get_state(pid)
      assert state == :running
    end

    test "running → stopped → running transitions", %{pid: pid} do
      :ok = Container.create(pid)
      :ok = Container.start(pid)

      assert :ok = Container.stop(pid)
      {state, data} = Container.get_state(pid)
      assert state == :stopped
      assert data.exit_code == 0

      assert :ok = Container.restart(pid)
      {state, _} = Container.get_state(pid)
      assert state == :running
    end

    test "stopped → removed transition", %{pid: pid} do
      :ok = Container.create(pid)
      :ok = Container.start(pid)
      :ok = Container.stop(pid)

      assert :ok = Container.remove(pid)
      {state, _} = Container.get_state(pid)
      assert state == :removed
    end
  end

  describe "invalid transitions" do
    test "cannot start from image_only", %{pid: pid} do
      assert {:error, {:invalid_transition, :image_only, :start}} = Container.start(pid)
    end

    test "cannot pause from created", %{pid: pid} do
      :ok = Container.create(pid)
      assert {:error, {:invalid_transition, :created, :pause}} = Container.pause(pid)
    end

    test "cannot remove from running without force", %{pid: pid} do
      :ok = Container.create(pid)
      :ok = Container.start(pid)
      assert {:error, {:invalid_transition, :running, {:remove, false}}} = Container.remove(pid)
    end

    test "can force remove from running", %{pid: pid} do
      :ok = Container.create(pid)
      :ok = Container.start(pid)
      assert :ok = Container.remove(pid, force: true)
      {state, _} = Container.get_state(pid)
      assert state == :removed
    end
  end
end
