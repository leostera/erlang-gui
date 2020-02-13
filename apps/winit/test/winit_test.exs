defmodule WinitTest do
  use ExUnit.Case
  doctest Winit

  test "greets the world" do
    assert Winit.hello() == :world
  end
end
