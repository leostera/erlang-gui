defmodule SkiaUmbrella.MixProject do
  use Mix.Project

  @rustler_mode if Mix.env() == :prod, do: :release, else: :debug

  def project do
    [
      apps_path: "apps",
      start_permanent: Mix.env() == :prod,
      compilers: [:rustler] ++ Mix.compilers(),
      rustler_crates: [
        skia: [mode: @rustler_mode],
        vulkan: [mode: @rustler_mode],
        winit: [mode: @rustler_mode]
      ],
      deps: deps()
    ]
  end

  defp deps do
    [
      {:rustler, "0.21.0"}
    ]
  end
end
