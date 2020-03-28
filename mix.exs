defmodule ErlangGUI.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  defp deps do
    [
      {
        :rustler,
        path: "/home/ostera/repos/github.com/rusterlium/rustler/rustler_mix/"
      },
      {:eflame, []}
    ]
  end
end
