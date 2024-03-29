defmodule ScratchPad.MixProject do
  use Mix.Project

  def project do
    [
      app: :scratchpad,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      mod: {:scratchpad_app, []},
      extra_applications: [:logger, :chalk]
    ]
  end

  defp deps do
    [
    ]
  end
end
