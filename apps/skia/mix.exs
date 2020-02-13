defmodule Skia.MixProject do
  use Mix.Project

  @rustler_mode if Mix.env() == :prod, do: :release, else: :debug

  def project do
    [
      app: :skia,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      compilers: [:rustler] ++ Mix.compilers(),
      rustler_crates: [
        skia: [mode: @rustler_mode, path: "../../native/skia"]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Skia.Application, []}
    ]
  end

  defp deps do
    [
      {:rustler, "0.21.0"}
    ]
  end
end
