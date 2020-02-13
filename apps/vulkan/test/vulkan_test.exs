defmodule VulkanTest do
  use ExUnit.Case
  doctest Vulkan

  test "greets the world" do
    assert Vulkan.hello() == :world
  end
end
